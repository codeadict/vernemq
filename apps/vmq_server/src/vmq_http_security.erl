%% Copyright 2020 Octavo Labs AG Switzerland (http://octavolabs.com)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
-module(vmq_http_security).

-export([set_cors_headers/1]).

%% CORS Documentation:
%% * https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS
%% * https://tools.ietf.org/html/rfc6454
%% * https://www.w3.org/TR/cors/
set_cors_headers(Req) ->
    Req0 = cowboy_req:set_resp_header(<<"vary">>, <<"origin">>, Req),
    case get_cors_origin(Req0) of
        undefined ->
            Req0;
        Origin ->
            Headers = #{
                <<"access-control-allow-origin">> => Origin,
                %% Allow cookies (or other user credentials) to be included on cross-origin requests.
                %% https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Credentials
                <<"access-control-allow-credentials">> => <<"true">>
            },
            OptionsHeaders = get_cors_options_headers(Req0),
            cowboy_req:set_resp_headers(maps:merge(Headers, OptionsHeaders), Req0)
    end.

get_cors_options_headers(Req) ->
    case cowboy_req:method(Req) of
        <<"OPTIONS">> ->
            MaxAge = application:get_env(vmq_server, http_cors_max_age, 1728000),
            AllowedHeaders = <<"content-type, authorization">>,
            #{
                <<"access-control-max-age">> => integer_to_list(MaxAge),
                <<"access-control-allow-headers">> => AllowedHeaders 
            };
        _Method->
            #{}
    end.

get_cors_origin(Req) ->
    case cowboy_req:header(<<"origin">>, Req) of
        undefined -> undefined;
        <<"null">> -> undefined;
        Origin ->
            AllowList = application:get_env(vmq_server, http_cors_allowlist, []),
            case lists:member(erlang:binary_to_list(Origin), AllowList) of
                true ->
                    Origin;
                false ->
                    case lists:member("*", AllowList) of
                        true -> Origin;
                        false -> undefined
                    end
            end
    end.
