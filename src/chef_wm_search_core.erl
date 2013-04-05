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


-module(chef_wm_search_core).

-include("chef_search.hrl").
-include("chef_wm.hrl").
-include_lib("chef_index/include/chef_solr.hrl").

-export([execute_search/4,
         handle_search_result/4]).

handle_search_result(ReqId, CacheKey, BulkGetFun,
                     #chef_search_result{result = Ids,  batch_size = BatchSize, 
                                         start = Start, solr_num_found = SolrNumFound} = _SearchResult) ->
    case ?SEARCH_CACHE:get(ReqId, CacheKey) of
        not_found ->
            DbResult = make_search_results(BulkGetFun, Ids,
                                           BatchSize, Start,
                                           SolrNumFound),
            ?SEARCH_CACHE:put(ReqId, CacheKey, DbResult),
            {cache_miss, DbResult};
        CacheValue ->
            {cache_hit, CacheValue}
    end.

execute_search(Req, #base_state{chef_db_context = DbContext,
                                organization_name = OrgName,
                                batch_size = BatchSize,
                                reqid = ReqId} = State, Query, Paths) ->
    case ?SH_TIME(ReqId, chef_solr, search, (Query)) of
        {ok, Start, SolrNumFound, Ids} ->
            IndexType = Query#chef_solr_query.index,
            BulkGetFun = make_bulk_get_fun(DbContext, OrgName,
                                           IndexType, Paths,
                                           Req),
            CacheKey = ?SEARCH_CACHE:make_key(OrgName, BatchSize, Start,
                                              Ids, wrq:raw_path(Req), Paths),
            {CacheStatus, {DbNumFound, Ans}} = handle_search_result(ReqId, CacheKey, BulkGetFun,
                                                                    #chef_search_result{result = Ids,  batch_size = BatchSize, 
                                                                                        start = Start, solr_num_found = SolrNumFound}),
            LogMsg = search_log_msg(CacheStatus, SolrNumFound,
                                    length(Ids), DbNumFound),
            case IndexType of
                {data_bag, BagName} when DbNumFound =:= 0 ->
                    case chef_db:data_bag_exists(DbContext, OrgName, BagName) of
                        true ->
                            {ok, Ans, LogMsg};
                        false ->
                            Msg = iolist_to_binary([<<"I don't know how to search for ">>,
                                                    BagName, <<" data objects.">>]),
                            {{halt, 404},
                                chef_wm_util:set_json_body(Req, {[{<<"error">>, [Msg]}]}),
                                State#base_state{log_msg=lists:flatten(["no data bag: ", BagName])}}
                    end;
                _Else ->
                    {ok, Ans, LogMsg}
            end;
        {error, Why} ->
            {error, Why}
    end.

search_log_msg(cache_miss, SolrNumFound, NumIds, DbNumFound) ->
    {search, SolrNumFound, NumIds, DbNumFound};
search_log_msg(cache_hit, SolrNumFound, NumIds, DbNumFound) ->
    {cached_search, SolrNumFound, NumIds, DbNumFound}.

%% @doc Returns a fun/1 that can be given a list of object IDs and returns a list of the
%% corresponding EJSON object. The fun wraps `chef_db:bulk_get' and in some cases does some
%% post-processing of the JSON to make it conform to the expected shape for Chef search
%% results.
%%
%% If `NamePaths' is non-empty, then the returned fun will create partial search results by
%% extracting the values specified by the paths and mapping them to the specified names in
%% the returned EJSON object.
make_bulk_get_fun(DbContext, OrgName, client, [], _Req) ->
    %% clients get special handling to add json_class which is not stored in the db (not
    %% even in couch).
    %%
    %% BUGBUG in waiting: special casing for one class is UBER CODE SMELL
    fun(Ids) ->
            Clients = chef_db:bulk_get(DbContext, OrgName, client, Ids),
            [ ej:set({<<"json_class">>}, Client, <<"Chef::ApiClient">>)
              || Client <- Clients ]
    end;
make_bulk_get_fun(DbContext, OrgName, {data_bag, BagName}, [], _Req) ->
    %% For data bag items, we return the raw object if the data bag item is coming from
    %% couchdb. Otherwise, we need to wrap the item in some additional JSON cruft to make it
    %% match the expected shape.
    %%
    %% BUGBUG BUGBUG: special casing for two classes is MEGA UBER CODE STENCH
    case chef_wm_darklaunch:is_enabled(<<"couchdb_data">>, OrgName) of
        true ->
            fun(Ids) ->
                    chef_db:bulk_get(DbContext, OrgName, data_bag_item, Ids)
            end;
        false ->
            fun(Ids) ->
                    Items = chef_db:bulk_get(DbContext, OrgName, data_bag_item, Ids),
                    %% FIXME: it would be great if we didn't have to wrap the data_bag_item
                    %% results this way at all. To reduce the CPU and memory use, we could
                    %% add a special bulk get query that also returns bag_name and item_name
                    %% and hand-craft the json to avoid parsing.
                    [ begin
                          RawItem = chef_json:decode(chef_db_compression:decompress(Item)),
                          ItemName = ej:get({<<"id">>}, RawItem),
                          chef_data_bag_item:wrap_item(BagName, ItemName, RawItem)
                      end || Item <- Items ]
            end
    end;
make_bulk_get_fun(DbContext, OrgName, Type, [], _Req) ->
    %% all other types just call into chef_db
    fun(Ids) ->
            chef_db:bulk_get(DbContext, OrgName, Type, Ids)
    end;
make_bulk_get_fun(DbContext, OrgName, Type, NamePaths, Req) ->
    %% Here NamePaths is a non-empty list of {Name, Path} tuples. This is the bulk_get fun
    %% that will be created if the user has requested partial search.
    fun(Ids) ->
            Items = chef_db:bulk_get(DbContext, OrgName, index_type_to_db_type(Type), Ids),
            RouteFun = ?BASE_ROUTES:url_for_search_item_fun(Req, Type, OrgName),
            [ begin
                  EJsonItem = parse_item(Type, Item),
                  Url = RouteFun(EJsonItem),
                  {[{<<"url">>, Url},
                    {<<"data">>,
                     {[ {Name, extract_path(EJsonItem, Path)}
                        || {Name, Path} <- NamePaths ]}}]}
              end
              || Item <- Items ]
    end.

%% chef_db:bulk_get expects a Chef object type, but the Type we have available is for the
%% search index. This will be correct except for the case of data_bag searches that need to
%% bulk_get data_bag_items. So we convert here.
index_type_to_db_type({data_bag, _}) ->
    data_bag_item;
index_type_to_db_type(Type) ->
    Type.

%% Possibly decompress and parse raw JSON into EJSON terms, else pass-through. Data coming
%% from chef_db:bulk_get may either be a list of JSON binary or a list of EJSON so this
%% function is used to normalize to EJSON.
%%
%% node data is a special case because node attributes are deep-merged prior to indexing for
%% search and we want to work with the same merged attributes for extracting partial search
%% data.
%%
%% data_bag is another special case while data bags are still in couchdb where they are
%% stored with wraper cruft. We provide a fun-head to deal with this.
parse_item(node, Item) ->
    Node = parse_item0(Item),
    %% This is fairly hacky. :(
    NodeRecStub = #chef_node{name = ej:get({<<"name">>}, Node),
                             environment = ej:get({<<"chef_environment">>}, Node)},
    chef_object:ejson_for_indexing(NodeRecStub, Node);
parse_item({data_bag, _}, Item) ->
    RawItem = parse_item0(Item),
    %% TODO: when data bags are no longer in couchdb, clean this
    case ej:get({<<"id">>}, RawItem) of
        undefined ->
            %% we have a crufted data_bag_item, de-cruft!
            ej:get({<<"raw_data">>}, RawItem);
        Id when is_binary(Id) ->
            %% no cruft, just return it
            RawItem
    end;
parse_item(_, Item) ->
    parse_item0(Item).

parse_item0(Item) when is_binary(Item) ->
    chef_json:decode(chef_db_compression:decompress(Item));
parse_item0({L}=Item) when is_list(L) ->
    %% should be valid EJSON format
    Item.

extract_path(_Item, []) ->
    null;
extract_path(Item, Path) ->
    ej:get(list_to_tuple(Path), Item, null).

make_search_results(BulkGetFun, Ids, BatchSize, Start, NumFound) ->
    Ans0 = search_result_start(Start, NumFound),
    {N, Ans1} = fetch_result_rows(Ids, BatchSize, BulkGetFun, {0, Ans0}),
    {N, search_result_finish(Ans1)}.

%% @doc Fetch a list of `Ids' in batches of size `BatchSize'.
%%
%% Each batch is fetched using `BulkGetFun'.  Results are cons'd onto `Acc' with each batch
%% separated by ``<<",">>''.
%%
%% Each set of results is processed to remove the _rev key and encode to JSON using ejson.
%% The ejson return value is post-processed to remove the JSON array markers.  The caller is
%% responsible for adding this back to create valid JSON.
%%
%% The purpose of this function is to allow us to build up the search response without ever
%% having more than `BatchSize' objects parsed into full EJSON terms. This helps us to limit
%% the RAM required to produce results for large searches. The trade-off is more complicated
%% processing logic than would be required if we just gathered all of the EJSON into a list
%% and then encoded it to JSON binary.
fetch_result_rows([], _BatchSize, _BulkGetFun, {N, Acc}) ->
    %% fetch complete, return fetched count and inner part of JSON array binary
    {N, Acc};
fetch_result_rows(Ids, BatchSize, BulkGetFun, {N, Acc}) when is_list(Ids) ->
    %% this head match when we are first called with the entire list of Ids. To get things
    %% started, we split the list of Ids into a batch of size `BatchSize' and the rest and
    %% get started.
    fetch_result_rows(safe_split(BatchSize, Ids), BatchSize, BulkGetFun, {N, Acc});
fetch_result_rows({Ids, []}, _BatchSize, BulkGetFun, {N, Acc}) ->
    %% This is the last batch, don't add a "," separator
    Docs = BulkGetFun(Ids),
    {N + length(Docs), encode_results(Docs, Acc)};
fetch_result_rows({Ids, Rest}, BatchSize, BulkGetFun, {N, Acc}) ->
    %% processing a batch happens here. we fetch the objects corresponding to the batch of
    %% Ids, encode them and then add them to our accumulator with the "," separator.
    Next = safe_split(BatchSize, Rest),
    Docs = BulkGetFun(Ids),
    fetch_result_rows(Next, BatchSize, BulkGetFun,
                      {N + length(Docs),
                       encode_results(Docs, <<",">>, Acc)}).

%% Catch the case where we're building results ending with
%% a dangling comma and strip it out. Completely ugly since
%% we're assuming the separator is a comma.
%% FIXME Refactor into a more readable/understandable design
encode_results([], [<<",">>|Acc]) ->
    Acc;
encode_results([], Acc) ->
    Acc;
encode_results(Results, Acc) ->
    [encode_result_rows(Results) | Acc].

encode_results([], _Prefix, Acc) ->
    Acc;
encode_results(Results, Prefix, Acc) ->
    [Prefix, encode_result_rows(Results) | Acc].

%% Encode a list of items as a JSON array partial. That is, encode as a JSON array and then
%% strip the '[' and ']' off the result. This allows us to incrementally encode a long array
%% of objects in batches avoiding having all objects in memory in order to encode them.
%%
%% This function knows how to deal with gzip binary from SQL and with EJSON data coming
%% straight from couch. If the data has come from couch, this is where couch cruft keys _id
%% and _rev are removed.
encode_result_rows([Item|_Rest]=Items) when is_binary(Item) ->
    ItemList = << <<(chef_db_compression:decompress(Bin))/binary, ",">> || Bin <- Items >>,
    %% remove trailing "," from binary
    binary:part(ItemList, {0, size(ItemList) - 1});
encode_result_rows(Items) ->
    %% ensure no couchdb cruft leaks out
    CleanItems = [ {remove_couchdb_keys(Doc)} || {Doc} <- Items ],
    Bin = chef_json:encode(CleanItems),
    %% remove leading '[' and trailing ']' so that we can add to this result.
    binary:part(Bin, {1, size(Bin) - 2}).

%% Remove couchdb internal keys "_rev" and "_id" from a tuple list where the keys are
%% assumed to be binaries. The first two instances of _rev or _id will be removed, so if you
%% somehow have duplicates, this will not remove all occurances.
remove_couchdb_keys([]) ->
    [];
remove_couchdb_keys(L) ->
    remove_couchdb_keys(L, 0).

remove_couchdb_keys([{Key, _}|T], N) when Key =:= <<"_rev">>;
                                          Key =:= <<"_id">> ->
    remove_couchdb_keys(T, N+1);
remove_couchdb_keys(L, N) when N > 1 ->
    L;
remove_couchdb_keys([H|T], N) ->
    [H|remove_couchdb_keys(T, N)];
remove_couchdb_keys([], _) ->
    [].


safe_split(N, L) ->
    try
        lists:split(N, L)
    catch
        error:badarg ->
            {L, []}
    end.

%% Return the start of a JSON response for search results. We take this approach to limit
%% RAM use and avoid having the entire result parsed into EJSON terms at one time.
search_result_start(Start, Total) ->
    % {"total":Total,"start":Start,"rows":[i1, i2]}
    ["\"rows\":[", ",",
     integer_to_list(Start), "\"start\":", ",",
     integer_to_list(Total), "\"total\":", "{"].

search_result_finish(Result) ->
    %% Note that all we need here is an iolist not a flat binary.
    lists:reverse([<<"]}">>|Result]).

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
