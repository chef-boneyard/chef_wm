%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
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

-module(chef_wm_base_tests).

-include_lib("eunit/include/eunit.hrl").

stats_hero_label_test_() ->
    GoodTests = [
                 ?_assertEqual(Expect, chef_wm_base:stats_hero_label(In))
                 || {In, Expect} <- [
                                     %% {Input, Expected}
                                     {{chef_sql, fetch_client}, <<"rdbms.chef_sql.fetch_client">>},
                                     {{chef_solr, some_fun}, <<"solr.chef_solr.some_fun">>}
                                    ] ],
    BadTests = [
                ?_assertError({bad_prefix, {bad, juju}},
                              chef_wm_base:stats_hero_label({bad, juju})) ],
    GoodTests ++ BadTests.
