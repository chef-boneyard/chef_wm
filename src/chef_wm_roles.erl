%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@opscode.com>
%% @author Seth Falcon <seth@opscode.com>
%% Copyright 2012 Opscode, Inc. All Rights Reserved.
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


-module(chef_wm_roles).

-include("chef_wm.hrl").

-mixin([{chef_wm_base, [content_types_accepted/2,
                        content_types_provided/2,
                        finish_request/2,
                        malformed_request/2,
                        ping/2,
                        post_is_create/2]}]).

-mixin([{?BASE_RESOURCE, [forbidden/2,
                          is_authorized/2,
                          service_available/2]}]).

%% I think we will end up moving the generic complete wm callbacks like post_is_create,
%% content_types_* into chef_wm_base and mixing those in here separately so that we only
%% have to have those defined in one place.

%% chef_wm behavior callbacks
-behaviour(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3]).

-export([allowed_methods/2,
         create_path/2,
         from_json/2,
         resource_exists/2,
         to_json/2]).

init(Config) ->
    chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #role_state{}}.

request_type() ->
    "roles".

allowed_methods(Req, State) ->
    {['GET','POST'], Req, State}.

validate_request('GET', Req, State) ->
    {Req, State};
validate_request('POST', Req, #base_state{resource_state = RoleState} = State) ->
    Body = wrq:req_body(Req),
    {ok, Role} = chef_role:parse_binary_json(Body, create),
    {Req, State#base_state{resource_state = RoleState#role_state{role_data = Role}}}.

auth_info(Req, State) ->
    auth_info(wrq:method(Req), Req, State).

auth_info('POST', Req, State) ->
    {{create_in_container, role}, Req, State};
auth_info('GET', Req, State) ->
    chef_wm_authz:use_custom_acls(roles, {container, role}, Req, State).

resource_exists(Req, State) ->
    {true, Req, State}.

create_path(Req, #base_state{resource_state = #role_state{role_data = RoleData}}=State) ->
    Name = ej:get({<<"name">>}, RoleData),
    {binary_to_list(Name), Req, State}.

from_json(Req, #base_state{resource_state =
                               #role_state{role_data = RoleData,
                                           role_authz_id = AuthzId}} = State) ->
    chef_wm_base:create_from_json(Req, State, chef_role, {authz_id, AuthzId}, RoleData).

to_json(Req, State) ->
    {all_roles_json(Req, State), Req, State}.

%% @doc Generate a JSON string for a hash of rolename => role URI
%% pairs.
%% @end
%%
%% TODO: try to extract this to a common function, as this pattern
%% pops up with a few other endpoints, too
all_roles_json(Req, #base_state{chef_db_context = DbContext,
                                organization_name = OrgName}) ->
    RoleNames = chef_db:fetch_roles(DbContext, OrgName),
    RouteFun = ?BASE_ROUTES:bulk_route_fun(role, Req),
    UriMap= [{Name, RouteFun(Name)} || Name <- RoleNames],
    chef_json:encode({UriMap}).

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).
