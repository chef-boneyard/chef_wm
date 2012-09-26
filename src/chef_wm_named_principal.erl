%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Douglas Triggs <doug@opscode.com>
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


-module(chef_wm_named_principal).

-include("chef_wm.hrl").

-mixin([{chef_wm_base, [content_types_accepted/2,
                        content_types_provided/2,
                        finish_request/2,
                        malformed_request/2,
                        ping/2]}]).

-mixin([{?BASE_RESOURCE, [service_available/2]}]).

%% chef_wm behaviour callbacks
-behaviour(chef_wm).
-export([
         auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3
        ]).

-export([
         forbidden/2,
         is_authorized/2,
         allowed_methods/2,
         to_json/2
       ]).

init(Config) ->
    chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #client_state{}}.

request_type() ->
  "principal".

forbidden(Req, State) ->
    {false, Req, State}.

is_authorized(Req, State) ->
    {true, Req, State}.

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

validate_request(_Method, Req, #base_state{chef_db_context = DbContext,
                                      organization_name = OrgName,
                                      resource_state = ClientState} = State) ->
    Name = chef_wm_util:object_name(client, Req),
    Client = case chef_db:fetch_client(DbContext, OrgName, Name) of
                 not_found ->
                     not_found;
                 #chef_client{} = Found ->
                     Found
             end,
    ClientState1 = ClientState#client_state{chef_client = Client},
    {Req, State#base_state{resource_state = ClientState1}}.

%% This should never get called.
auth_info(Req, State) ->
    {{halt, 404}, Req, State}.

to_json(Req, #base_state{resource_state =
                             #client_state{chef_client = Client},
                         organization_name = OrgName} = State) ->
    EJson = chef_client:assemble_client_ejson(Client, OrgName),
    Json = ejson:encode(EJson),
    {Json, Req, State}.

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).
