%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Brown <cb@opscode.com>
%% @author Seth Falcon <seth@opscode.com>
%% @author John Keiser <jkeiser@opscode.com>
%% @author Christopher Maier <cm@opscode.com>
%% @author Kevin Smith <kevin@opscode.com>
%% @author Seth Chisamore <schisamo@opscode.com>
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


-module(chef_wm_search).

-include("chef_wm.hrl").
-include_lib("chef_index/include/chef_solr.hrl").

%% We chose to *not* mixin chef_wm_base:post_is_create/2 as a POST in
%% this resource is purely for processing...not resource creation.
-mixin([{chef_wm_base, [content_types_accepted/2,
                        content_types_provided/2,
                        finish_request/2,
                        malformed_request/2,
                        ping/2]}]).

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
         process_post/2,
         resource_exists/2,
         to_json/2]).

init(Config) ->
    chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #search_state{}}.

request_type() ->
    "search".

allowed_methods(Req, State) ->
    {['GET','POST'], Req, State}.

validate_request('GET', Req, State) ->
    Query = make_query_from_params(Req),
    SearchState = #search_state{solr_query = Query},
    {Req, State#base_state{resource_state = SearchState}};
validate_request('POST', Req, State) ->
    Query = make_query_from_params(Req),
    Body = chef_json:decode(wrq:req_body(Req)),
    validate_body(Body),
    {NamePaths} = Body,
    SearchState = #search_state{solr_query = Query, partial_paths = NamePaths},
    {Req, State#base_state{resource_state = SearchState}}.

auth_info(Req, State) ->
    {authorized, Req, State}.

resource_exists(Req, #base_state{organization_guid = OrgGuid,
                                 resource_state = SearchState} = State) ->
    QueryWithoutGuid = SearchState#search_state.solr_query,
    try
        Query = chef_solr:add_org_guid_to_query(QueryWithoutGuid, OrgGuid),
        SearchState1 = SearchState#search_state{solr_query = Query},
        {true, Req, State#base_state{organization_guid = OrgGuid,
                                     resource_state = SearchState1}}
    catch
        throw:org_not_found ->
            %% Not sure we can ever get here; user in org check will
            %% have failed with 403 if no such org.
            OrgName = chef_wm_util:extract_from_path(organization_id, Req),
            NoOrg = resource_exists_message(org_not_found, OrgName),
            Req1 = chef_wm_util:set_json_body(Req, NoOrg),
            {false, Req1, State#base_state{log_msg = org_not_found}}
    end.

resource_exists_message(org_not_found, Org) ->
    Msg = iolist_to_binary([<<"organization '">>, Org,
                            <<"' does not exist.">>]),
    {[{<<"error">>, [Msg]}]}.

to_json(Req, #base_state{chef_db_context = DbContext,
                         resource_state = SearchState,
                         organization_name = OrgName,
                         batch_size = BatchSize,
                         reqid = ReqId} = State) ->
    Query = SearchState#search_state.solr_query,
    Paths = SearchState#search_state.partial_paths,

    case chef_wm_search_core:execute_search(Req, State, Query, Paths) of
        {error, {solr_400, _}=Why} ->
            {{halt, 400},
                chef_wm_util:set_json_body(Req,
                    malformed_request_message(Why, Req, State)),
                State#base_state{log_msg=Why}};
        {error, {solr_500, _}=Why} ->
            {{halt, 500},
                chef_wm_util:set_json_body(Req,
                    malformed_request_message(Why, Req, State)),
                State#base_state{log_msg=Why}};
        {{halt, 404}, R, S} ->
            {{halt, 404}, R, S};
        {ok, Ans, LogMsg} ->
            State1 = State#base_state{log_msg = LogMsg},
            {Ans, Req, State1}
    end.

%% POST to /search represents a partial search request
%% The posted request body should be of the form:
%% { "mk1" : [ "K1", "K2" ],
%%   "mk2" : [ "K3", "K4", "K5" ] }
%%
process_post(Req, State) ->
    {Ans, Req1, State1} = to_json(Req, State),
    case Ans of
        {halt, _} -> {Ans, Req1, State1};
        Result -> {true, wrq:set_resp_body(Result, Req), State1}
    end.

%% This helper function extracts the necessary params from the request and passes it to
%% chef_solr:make_query_from_params to return the query.
make_query_from_params(Req) ->
    % TODO - sort this out
    % ObjType = chef_wm_util:extract_from_path(object_type, Req),
    ObjType = chef_wm_util:extract_from_path(object_type, Req),
    QueryString = wrq:get_qs_value("q", Req),
    Start = wrq:get_qs_value("start", Req),
    Rows = wrq:get_qs_value("rows", Req),
    chef_solr:make_query_from_params(ObjType, QueryString, Start, Rows).


malformed_request_message(#ej_invalid{}, _Req, _State) ->
    Msg = <<"invalid partial search request body">>,
    {[{<<"error">>, [Msg]}]};
malformed_request_message({bad_query, Query}, _Req, _State) ->
    Msg = iolist_to_binary([<<"invalid search query: '">>, Query, <<"'">>]),
    {[{<<"error">>, [Msg]}]};
malformed_request_message({bad_param, {Param, Value}}, _Req, _State) ->
    Msg = iolist_to_binary([<<"invalid '">>, Param, <<"' value: '">>, Value, <<"'">>]),
    {[{<<"error">>, [Msg]}]};
malformed_request_message({solr_400, _}, _Req, _State) ->
    Msg = <<"invalid index name or query">>,
    {[{<<"error">>, [Msg]}]};
malformed_request_message({solr_500, _Query}, _Req, _State) ->
    Msg = <<"internal search error">>,
    {[{<<"error">>, [Msg]}]}.

-spec validate_body(ej:json_object()) -> {ok, ej:json_object()}.
validate_body(Body) ->
    case ej:valid(partial_search_spec(), Body) of
        ok -> {ok, Body};
        Bad -> throw(Bad)
    end.

partial_search_spec() ->
    {object_map,
     {{keys, string},
      {values, {array_map, string}}}}.
