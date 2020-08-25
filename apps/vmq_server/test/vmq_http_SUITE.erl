-module(vmq_http_SUITE).

-include_lib("eunit/include/eunit.hrl").

%% CT exports
-export([
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    all/0,
    groups/0
]).

%% test case exports
%% healthcheck endpoint
-export([
    simple_healthcheck_test/1
]).

%% test case exports
%% security headers
-export([
    cors_returned_on_get_request/1
]).

-define(HTTP_HOST, "http://localhost:8888").

init_per_suite(_Config) ->
    cover:start(),
    _Config.

end_per_suite(_Config) ->
    _Config.

init_per_testcase(_Case, Config) ->
    vmq_test_utils:setup(),
    vmq_server_cmd:set_config(allow_anonymous, true),
    Config.

end_per_testcase(_, Config) ->
    vmq_test_utils:teardown(),
    Config.

all() ->
    [
        {group, all}
    ].

groups() ->
    [
        {all, [sequence], [
            {group, health_endpoints},
            {group, security_headers}
        ]},
        %% Health endpoints
        {health_endpoints, [], [simple_healthcheck_test]},
        %% Cors et al
        {security_headers, [], [cors_returned_on_get_request]}
    ].

simple_healthcheck_test(_) ->
    %% we have to setup the listener here, because vmq_test_utils is overriding
    %% the default set in vmq_server.app.src
    vmq_server_cmd:listener_start(8888, [
        {http, true},
        {config_mod, vmq_health_http},
        {config_fun, routes}
    ]),
    application:ensure_all_started(inets),
    {ok, {_Status, _Headers, Body}} = httpc:request(?HTTP_HOST ++ "/health"),
    JsonResponse = jsx:decode(list_to_binary(Body), [return_maps, {labels, binary}]),
    <<"OK">> = maps:get(<<"status">>, JsonResponse).

%% ============================================================
%% Tests for CORS headers
%% ============================================================

cors_returned_on_get_request(_) ->
    application:ensure_all_started(inets),
    application:set_env(vmq_server, http_cors_allowed_origins, ["remote.com"]),
    ApiKey = vmq_http_mgmt_api:create_api_key(),
    vmq_server_cmd:listener_start(8888, [
        {http, true},
        {config_mod, vmq_http_mgmt_api},
        {config_fun, routes}
    ]),
    Auth = base64:encode(<<ApiKey/binary, ":">>),
    Endpoint = ?HTTP_HOST ++ "/api/v1/session/show",
    ContentType = "application/json",
    ReqHeaders = [
        {"Origin", "remote.com"},
        {"Authorization", "Basic " ++ Auth},
        {"Content-Type", ContentType}
    ],
    {ok, {Status, RespHeaders, _Body}} = httpc:request(get, {Endpoint, ReqHeaders}, [], []),
    ?assertEqual(200, Status),
    ?assertEqual("origin", proplists:get_value("vary", RespHeaders)).
