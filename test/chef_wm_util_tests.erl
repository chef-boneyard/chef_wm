%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% Copyright 2013 Opscode, Inc. All Rights Reserved.
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

-module(chef_wm_util_tests).

-include_lib("eunit/include/eunit.hrl").

base_uri_test_() ->
    HttpReq = wrq:create(get, "1.1", "/",
        mochiweb_headers:from_binary([<<"Host: api.example.com:80\r\n">>])),
    HttpsReq = wrq:create(get, https, "1.1", "/",
        mochiweb_headers:from_binary([<<"Host: api.example.com:443\r\n">>])),
    NoHostReq = wrq:create(get, https, "1.1", "/",
        mochiweb_headers:from_binary([<<"Content-type: application/json\r\n">>])),
    [
        {"When base_resource_url is set to host_header",
            {foreach,
                fun() ->
                        application:set_env(chef_wm, base_resource_url, host_header)
                end,
                fun(_) ->
                        undefined %% noop
                end,
                [
                    {"with http request",  ?_test(?assertEqual("http://api.example.com:80",chef_wm_util:base_uri(HttpReq)))},
                    {"with https request", ?_test(?assertEqual("https://api.example.com:443",chef_wm_util:base_uri(HttpsReq)))},
                    {"without Host: header",
                        fun() ->
                                meck:new(wrq, [passthrough]),
                                meck:expect(wrq, host_tokens, 1, ["fake-s3", "com"]),
                                meck:expect(wrq, port, 1, 80),
                                ?assertEqual("https://fake-s3.com", chef_wm_util:base_uri(NoHostReq)),
                                meck:unload(wrq)
                        end
                    }
                ]
            }
        },
        {"When base_resource_url is not set",
            {foreach,
                fun() ->
                        application:unset_env(chef_wm, base_resource_url)
                end,
                fun(_) ->
                        undefined %% noop
                end,
                [
                    {"with http request",  ?_test(?assertEqual("http://api.example.com:80",chef_wm_util:base_uri(HttpReq)))},
                    {"with https request", ?_test(?assertEqual("https://api.example.com:443",chef_wm_util:base_uri(HttpsReq)))},
                    {"without Host: header",
                        fun() ->
                                meck:new(wrq, [passthrough]),
                                meck:expect(wrq, host_tokens, 1, ["fake-s3", "com"]),
                                meck:expect(wrq, port, 1, 80),
                                ?assertEqual("https://fake-s3.com", chef_wm_util:base_uri(NoHostReq)),
                                meck:unload(wrq)
                        end
                    }
                ]
            }
        },
        {"When base_resource_url is set to something",
            fun() ->
                    ExternalUrl = "https://fake-s3.com",
                    application:set_env(chef_wm, base_resource_url, ExternalUrl),

                    %% Regardless of request information, base_uri will return whatever
                    %% is set to base_resource_url if it is a string
                    ?assertEqual(ExternalUrl, chef_wm_util:base_uri(HttpReq)),
                    ?assertEqual(ExternalUrl, chef_wm_util:base_uri(HttpsReq))
            end
        }
    ].




