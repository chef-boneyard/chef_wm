%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@opscode.com>
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


%% Authz logic and helpers for chef_wm endpoints
%%

-module(chef_wm_authz).

-export([allow_admin/1,
         allow_admin_or_requesting_node/2,
         allow_validator/1,
         is_admin/1,
         is_requesting_node/2,
         is_validator/1]).

-include("chef_wm.hrl").

-spec allow_admin(#chef_client{} | #chef_user{}) -> authorized | forbidden.
allow_admin(#chef_client{admin = true}) ->
    authorized;
allow_admin(#chef_client{}) ->
    forbidden;
allow_admin(#chef_user{admin = true}) ->
    authorized;
allow_admin(#chef_user{}) ->
    forbidden.

-spec allow_admin_or_requesting_node(#chef_client{} | #chef_user{}, binary()) -> authorized | forbidden.
allow_admin_or_requesting_node(#chef_client{name = Name}, Name) ->
    authorized;
allow_admin_or_requesting_node(#chef_client{} = Client, _Name) ->
    allow_admin(Client);
allow_admin_or_requesting_node(#chef_user{username = Name}, Name) ->
    authorized;
allow_admin_or_requesting_node(#chef_user{} = User, _Name) ->
    allow_admin(User).

-spec allow_validator(#chef_client{}) -> authorized | forbidden.
allow_validator(#chef_client{validator = true}) ->
    authorized;
allow_validator(#chef_client{}) ->
    forbidden.

-spec is_admin(#chef_client{} | #chef_user{}) -> true | false.
is_admin(#chef_client{admin = true}) ->
    true;
is_admin(#chef_client{}) ->
    false;
is_admin(#chef_user{admin = true}) ->
    true;
is_admin(#chef_user{}) ->
    false.

-spec is_requesting_node(#chef_client{} | #chef_user{}, binary()) -> true | false.
is_requesting_node(#chef_client{name = Name}, Name) ->
    true;
is_requesting_node(#chef_client{}, _Name) ->
    false;
is_requesting_node(#chef_user{username = Name}, Name) ->
    true;
is_requesting_node(#chef_user{}, _Name) ->
    false.

-spec is_validator(#chef_client{}) -> true | false.
is_validator(#chef_client{validator = true}) ->
    true;
is_validator(#chef_client{}) ->
    false.


