%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Driebit BV
%% @doc Automatic translations for saved resources.

%% Copyright 2019 Driebit BV
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

-module(m_autotrans).

-include_lib("zotonic.hrl").

-export([
    translate/4,

    google_translation_api/4,
    microsoft_translation_services/4
]).

-define(GOOGLE_TRANSLATION_ENDPOINT, "https://translation.googleapis.com").
-define(MICROSOFT_TRANSLATOR_TEXT_ENDPOINT, "https://api.cognitive.microsofttranslator.com").


-spec translate( binary(), atom(), atom(), z:context() ) -> {ok, binary()} | {error, retry|unavailable}.
translate(Text, From, To, Context) ->
    first(
        [
            {google_translation_api, <<"Google Translation API">>},
            {microsoft_translation_services, <<"Microsoft Translation Services">>}
        ],
        [ Text, From, To ],
        false,
        Context).

%% @doc Try all translation services, return the first success result.
first([], _Args, false, _Context) ->
    {error, unavailable};
first([], _Args, true, _Context) ->
    {error, retry};
first([ {F, Desc} | Fs ], Args, IsRetry, Context) ->
    Key = list_to_atom( "autotrans_" ++ atom_to_list(F) ),
    case z_depcache:get(Key, Context) of
        {ok, false} ->
            first(Fs, Args, IsRetry, Context);
        {ok, true} ->
            first(Fs, Args, true, Context);
        undefined ->
            case erlang:apply(?MODULE, F, Args ++ [ Context ]) of
                {ok, _} = OK ->
                    OK;
                {error, ratelimit} ->
                    z:info("Automatic translation: rate limit hit for ~s",
                              [ Desc ],
                              [ {module, ?MODULE}, {line, ?LINE} ],
                              Context),
                    disable(Key, F, 120, "rate limited", true, Context),
                    first(Fs, Args, true, Context);
                {error, overload} ->
                    disable(Key, F, 60, "overload", true, Context),
                    z:info("Automatic translation: overload error for ~s",
                              [ Desc ],
                              [ {module, ?MODULE}, {line, ?LINE} ],
                              Context),
                    first(Fs, Args, true, Context);
                {error, eacces} ->
                    z:error("Automatic translation: invalid keys or access denied for ~s",
                              [ Desc ],
                              [ {module, ?MODULE}, {line, ?LINE} ],
                              Context),
                    disable(Key, F, 600, "access denied", false, Context),
                    first(Fs, Args, IsRetry, Context);
                {error, notfound} ->
                    z:warning("Automatic translation: URL not found (404) for ~s",
                              [ Desc ],
                              [ {module, ?MODULE}, {line, ?LINE} ],
                              Context),
                    disable(Key, F, 600, "not found", false, Context),
                    first(Fs, Args, IsRetry, Context);
                {error, Error} ->
                    z:warning("Automatic translation: unknown error for ~s: ~p",
                              [ Desc, Error ],
                              [ {module, ?MODULE}, {line, ?LINE} ],
                              Context),
                    % Some unknown error - assume it is transient
                    lager:error("autotrans function ~p returns error ~p", [ F, Error ]),
                    first(Fs, Args, true, Context)
            end
    end.

%% @doc Disable a translation service for a period of time.
disable(Key, Function, Timeout, Reason, IsRetry, Context) ->
    lager:info("autotrans function ~p is ~s", [ Function, Reason ]),
    z_depcache:set(Key, IsRetry, Timeout, Context).

%% @doc Translate a text with the free Google Translate API (no API key needed)
-spec google_translation_api( binary(), atom(), atom(), z:context() ) -> {ok, binary()} | {error, term()}.
google_translation_api(Text, From, To, Context) when is_binary(Text) ->
    ApiKey =  m_config:get_value(mod_autotrans, google_api_secret, Context),
    google_translation_api1(Text, From, To, ApiKey, Context).

%% @doc Translate a text with the free Google Translate API (no API key needed)
-spec google_translation_api1( binary(), atom(), atom(), binary(), z:context() ) -> {ok, binary()} | {error, term()}.
google_translation_api1(_Text, _From, _To, undefined, _Context) -> {error, disabled};
google_translation_api1(_Text, _From, _To, <<>>, _Context) -> {error, disabled};
google_translation_api1(Text, From, To, ApiKey, Context) when is_binary(Text) ->
    Url = iolist_to_binary([
            ?GOOGLE_TRANSLATION_ENDPOINT, "/language/translate/v2",
            "?key=", z_url:url_encode( z_convert:to_binary(ApiKey) )
        ]),
    Body = iolist_to_binary( mochijson2:encode([
        {<<"source">>, z_convert:to_binary(From)},
        {<<"target">>, z_convert:to_binary(To)},
        {<<"format">>, <<"html">>},
        {<<"q">>, Text}
    ])),
    Hs = [
        {"Content-Type", "application/json"},
        {"Content-Length", z_convert:to_list( size(Body) )},
        {"Referer", z_convert:to_list( iolist_to_binary( z_context:abs_url("/", Context)))}
    ],
    case hackney:request(post, z_convert:to_list(Url), Hs, Body) of
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            case StatusCode of
                200 ->
                    lager:info("autotrans: translated text using Google"),
                    {struct, JSON} = mochijson2:decode(RespBody),
                    {<<"data">>, {struct, DataProps}} = proplists:lookup(<<"data">>, JSON),
                    {<<"translations">>, [ {struct, Tr} | _ ]} = proplists:lookup(<<"translations">>, DataProps),
                    {ok, proplists:get_value(<<"translatedText">>, Tr)};
                401 ->
                    {error, eacces};
                403 ->
                    {error, eacces};
                503 ->
                    {error, overload};
                Other ->
                    {error, Other}
            end;
        {error, _} = Error ->
            Error
    end.


%% @doc Translate a text using Microsoft Azure translation services
%% https://docs.microsoft.com/en-us/azure/cognitive-services/translator/translator-text-how-to-signup
-spec microsoft_translation_services( binary(), atom(), atom(), z:context() ) -> {ok, binary()} | {error, term()}.
microsoft_translation_services(Text, From, To, Context) ->
    ApiKey =  m_config:get_value(mod_autotrans, microsoft_api_secret, Context),
    microsoft_translation_services1(Text, From, To, ApiKey).

microsoft_translation_services1(_Text, _From, _To, undefined) ->
    {error, disabled};
microsoft_translation_services1(_Text, _From, _To, <<>>) ->
    {error, disabled};
microsoft_translation_services1(Text, From, To, ApiKey) ->
    Url = iolist_to_binary([
            "https://api.cognitive.microsofttranslator.com/translate?api-version=3.0",
            "&from=", z_url:url_encode( z_convert:to_binary(From) ),
            "&to=", z_url:url_encode( z_convert:to_binary(To) ),
            "&textType=html"
        ]),
    Body = iolist_to_binary( mochijson2:encode([ {struct, [ {<<"Text">>, Text} ]} ]) ),
    Hs = [
        {"Content-Type", "application/json"},
        {"Content-Length", z_convert:to_list( size(Body) )},
        {"Ocp-Apim-Subscription-Key", z_convert:to_list(ApiKey)}
    ],
    case hackney:request(post, z_convert:to_list(Url), Hs, Body, []) of
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            case StatusCode of
                200 ->
                    lager:info("autotrans: translated text using Microsoft"),
                    [ {struct, TrProps} | _ ] = mochijson2:decode(RespBody),
                    {<<"translations">>, [ {struct, Trs} ]} = proplists:lookup(<<"translations">>, TrProps),
                    {<<"text">>, RespText} = proplists:lookup(<<"text">>, Trs),
                    {ok, RespText};
                401 -> {error, eacces};
                403 -> {error, eacces};
                503 -> {error, overload};
                Other -> {error, Other}
            end;
        {error, _} = Error ->
            Error
    end.
