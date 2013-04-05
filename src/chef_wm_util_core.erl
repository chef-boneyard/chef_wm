%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Serdar Sutay <serdar@opscode.com>
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

-module(chef_wm_util_core).

-export([base_uri/1,
         set_json_body/2,
         set_uri_of_created_resource/1,
         set_uri_of_created_resource/2,
         not_found_message/2
        ]).

%% @doc Returns the base URI for the server as called by the client as a string.
base_uri(Req) ->
    Scheme = scheme(Req),
    Host = string:join(lists:reverse(wrq:host_tokens(Req)), "."),
    PortString = port_string(wrq:port(Req)),
    Scheme ++ "://" ++ Host ++ PortString.

scheme(Req) ->
    case wrq:get_req_header("x-forwarded-proto", Req) of
        undefined ->
            case wrq:scheme(Req) of
                https -> "https";
                http -> "http";
                P -> erlang:atom_to_list(P)
            end;
        Proto -> Proto
    end.

%% So this is kind of gross and will prevent correct port info if you run https on port 80
%% or http on port 443; otherwise it should work. The problem is two-fold, first webmachine
%% ignores scheme information when parsing the host header and so always sets the port to 80
%% if no port is present in the host header. But in a load-balanced situation, the scheme
%% from webmachine may not reflect what is in use at the load balancer. A simple compromise
%% is to treat both 80 and 443 as default and only include a port string if the port differs
%% from those.
port_string(Default) when Default =:= 80; Default =:= 443 ->
    "";
port_string(Port) ->
    [$:|erlang:integer_to_list(Port)].


%% @doc Converts the given Ejson-encoded data to a JSON string and
%% sets it as the request body, returning the updated request.
%% @end
set_json_body(Req, EjsonData) ->
    Json = chef_json:encode(EjsonData),
    wrq:set_resp_body(Json, Req).

%% @doc Sets the JSON body of a response and it's Location header to
%% point to the URI of a newly-created resource.
%%
%% The body will be of the form
%% ```
%%     {"uri":"http://foo.com/newresource"}
%% '''
%% Returns the updated request.
set_uri_of_created_resource(Req) ->
    set_uri_of_created_resource(chef_wm_util:full_uri(Req), Req).
set_uri_of_created_resource(Uri, Req) when is_list(Uri) ->
    set_uri_of_created_resource(list_to_binary(Uri), Req);
set_uri_of_created_resource(Uri, Req0) when is_binary(Uri) ->
    %% Uri needs to be a binary for encoding to JSON, but a string for the header value
    Req = set_json_body(Req0, {[{<<"uri">>, Uri}]}),
    wrq:set_resp_header("Location", binary_to_list(Uri), Req).

not_found_message(TypeName, ObjName) ->
    error_message_envelope(iolist_to_binary([TypeName, " '", ObjName, "' not found"])).

error_message_envelope(Message) when is_binary(Message) orelse
                                     is_tuple(Message) ->
    %% Tuple guard is really intended for grabbing EJson-encoded json objects, but we don't
    %% have guards for that.  It was added to accommodate depsolver messages.  This is part
    %% of an ongoing refactor, and may not ultimately be necessary.
    {[{<<"error">>, [Message]}]}.
