%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
%% Copyright 2011-2012 Opscode, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%


-include_lib("webmachine/include/wm_reqdata.hrl").
-include_lib("chef_objects/include/chef_types.hrl").
-include_lib("chef_objects/include/chef_osc_defaults.hrl").
-include_lib("mixer/include/mixer.hrl").
-include_lib("ej/include/ej.hrl").

%% FIXME: this will be OPC only for the start most likely
-include_lib("stats_hero/include/stats_hero.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

-ifndef(BASE_RESOURCE).
-define(BASE_RESOURCE, chef_wm_base).
-endif.

-ifndef(BASE_ROUTES).
-define(BASE_ROUTES, chef_wm_routes).
-endif.

-type wm_req() :: #wm_reqdata{}.

%% Shared resource state shared by all chef_wm resource modules.
-record(base_state, {
          %% Concrete resource impl
          resource_mod :: atom(),
          %% unique request ID from nginx header (or generated if not
          %% found) set by chef_wm_util:read_req_id.
          reqid :: binary(),

          %% The name of the HTTP request header containing the unique ID set by the load
          %% balancer
          reqid_header_name :: string(),

          %% A fun/1 that closes over the request headers and returns
          %% header values as binaries or 'undefined'.
          %% chef_rest_wm:init sets this.
          header_fun = undefined :: fun((binary()|string()) -> binary() | 'undefined')
                                  | 'undefined',

          %% Message added to erchef log messages (not user visible).
          %% Used to pass extra info usually in non-200 cases to the
          %% shared request logging code.
          %%
          %% Formatted using ~p, but expected to be reasonably small.
          log_msg = "" :: term(),

          %% Time drift in seconds allowed between the timestamp in a
          %% singed request and the clock on the server.  Set in
          %% app.config {chef_rest, auth_skey}.
          auth_skew = 900 :: non_neg_integer(),

          %% The GUID for the organization name that appears in the
          %% request URL.  This gets set in chef_rest_wm:is_authorized
          %% if a client is making the request.
          organization_guid :: object_id(),

          %% The name of the organization parsed from the request URL.
          %% Set by chef_rest_wm:service_available.
          organization_name :: binary() | ?OSC_ORG_NAME,

          %% Batch size used to pull back large objects from couchdb.
          %% Currently used by the search resource to limit the number
          %% of nodes that are in memory at one time.
          batch_size = 5 :: non_neg_integer(),

          %% Opaque db connection context record as returned by
          %% chef_db:make_context.  Allows db layer access to request
          %% ID.  Set in chef_rest_wm:service_available
          chef_db_context :: chef_db:db_context(),

          %% Opaque db connection context record as returned by chef_authz:make_context.
          chef_authz_context :: chef_authz:chef_authz_context(),

          %% AuthzId for the actor making the request.
          requestor_id :: object_id(),

          %% Details for The actor making the request.
          requestor :: #chef_client{} | #chef_user{},

          %% A record containing resource-specific state.
          resource_state :: tuple(),

          %% Turn this on if superuser is allowed to bypass security checks for
          %% this endpoint.
          superuser_bypasses_checks = false :: true | false
         }).

-record(client_state, {
          client_data,
          client_authz_id,
          chef_client :: #chef_client{} | not_found
         }).

-record(cookbook_state, {
          %% authz id of the cookbook
          authz_id,
          %% authz id for the cookbooks container
          cookbook_container_id,
          %% EJson representation of a cookbook version
          cookbook_data,
          %% cookbook name from the URL
          cookbook_name,
          %% cookbook version from the URL
          cookbook_version,
          chef_cookbook_version :: #chef_cookbook_version{} | undefined,
          %% number of versions to display when doing cookbook list
          num_versions :: non_neg_integer() | all | undefined
         }).

-record(data_state, {
          data_bag_name,
          data_bag_item_name,
          data_bag_item_ejson,
          data_bag_authz_id,
          chef_data_bag :: #chef_data_bag{} | undefined,
          chef_data_bag_item :: #chef_data_bag_item{} | undefined
         }).

-record(environment_state, {
          environment_data,
          environment_authz_id,
          chef_environment :: #chef_environment{},

          %% Used for when we're returning environment-filtered cookbook version info
          num_versions :: num_versions(),

          %% Used when we're grabbing specific cookbooks filtered through an environment
          %% `all' indicates that all cookbooks should be returned (duh)
          cookbook :: binary() | all
         }).

-record(node_state, {
          environment_name,
          node_data,
          node_authz_id,
          chef_node :: #chef_node{}
         }).

-record(role_state, {
          %% EJson-encoded representation of a Role
          role_data,
          role_authz_id,
          chef_role :: #chef_role{}
         }).

-record(sandbox_state, {
          id,
          sandbox_authz_id,
          sandbox_data,
          chef_sandbox
          }).

-record(search_state, {
          solr_query = undefined,
          partial_paths = []
         }).

-record(depsolver_state, {
          chef_environment :: #chef_environment{},
          %% environment within which to depsolve from the URL
          environment_name :: binary(),
          %% list of required cookbooks from POST.  These have been processed
          %% and if there was a version in the recipe the we store it as a
          %% cookbook name, version tuple
          run_list_cookbooks :: [binary() | {binary(), binary()}]
        }).

-define(gv(X,L), proplists:get_value(X, L)).
-define(gv(X,L, D), proplists:get_value(X, L, D)).
