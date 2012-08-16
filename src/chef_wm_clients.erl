%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 80. -*-
%% ex: ts=4 sw=4 et
%% @author Douglas Triggs <doug@opscode.com>
%% @copyright 2011 Opscode, Inc.

%% @doc Resource module for Chef clients endpoint
%%
%% The REST API provided by this module is as follows:
%%
%% Create a new client with body of POST being client JSON
%% POST /organizations/:org_name/clients/
%%
%% Fetch all client names
%% GET /organizations/:org_name/clients/
%%
%%
-module(chef_wm_clients).

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
         allowed_methods/2,
         create_path/2,
         from_json/2,
         to_json/2
       ]).

init(Config) ->
    chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #client_state{}}.

request_type() ->
    "clients".

allowed_methods(Req, State) ->
    {['GET', 'POST'], Req, State}.

%% @doc Currently we need to allow the pivotal user to create clients for pedant.
%% We set up the state such that the superuser avoids the ACL checks.
%% FIXME: This is a temporary fix until pedant uses the validator which has
%% permissions to create a new client
validate_request('GET', Req, State) ->
    {Req, State#base_state{resource_state = #client_state{}}};
validate_request('POST', Req, State) ->
    case wrq:req_body(Req) of
        undefined ->
            throw({error, missing_body});
        Body ->
            {ok, Client} = chef_client:parse_binary_json(Body, undefined),
            {Req, State#base_state{superuser_bypasses_checks = true,
                    resource_state = #client_state{client_data = Client}}}
    end.

auth_info(Req, State) ->
    auth_info(wrq:method(Req), Req, State).

auth_info('POST', Req, State) ->
    {{create_in_container, client}, Req, State};
auth_info('GET', Req, State) ->
    {{container, client}, Req, State}.

create_path(Req, #base_state{resource_state = #client_state{client_data = ClientData}} = State) ->
    Name = ej:get({<<"name">>}, ClientData),
    {binary_to_list(Name), Req, State}.

%% @doc We generate a new public/private key pair, insert the public key into the DB
%% and return the private key as part of the response
from_json(Req, #base_state{reqid = RequestId,
                           resource_state = #client_state{client_data = ClientData,
                                                          client_authz_id = AuthzId}} = State) ->
    Name = ej:get({<<"name">>}, ClientData),
    {Cert, PrivateKey} = chef_cert_http:gen_cert(Name, RequestId),
    ClientData1 = chef_client:add_authn_fields(ClientData, Cert),
    case chef_wm_base:create_from_json(Req, State, chef_client, {authz_id, AuthzId}, ClientData1) of
        {true, Req1, State1} ->
            Req2 = chef_wm_util:append_field_to_json_body(Req1, <<"private_key">>, PrivateKey),
            {true,Req2, State1};
        Else ->
            Else
    end.

to_json(Req, State) ->
    {all_clients_json(Req, State), Req, State}.

%% Internal Functions
all_clients_json(Req, #base_state{chef_db_context = DbContext,
                                  organization_name = OrgName}) ->
    ClientNames = chef_db:fetch_clients(DbContext, OrgName),
    RouteFun = ?BASE_ROUTES:bulk_route_fun(client, Req),
    UriMap = [ {Name, RouteFun(Name)} || Name <- ClientNames ],
    ejson:encode({UriMap}).

malformed_request_message(Any, Req, State) ->
    chef_wm_util:malformed_request_message(Any, Req, State).
