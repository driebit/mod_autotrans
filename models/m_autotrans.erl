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

    google_translate_api/4,
    google_translate_api/3,

    microsoft_translation_services/4
]).

-define(GOOGLE_TRANSLATE_URL, "https://translate.googleapis.com/translate_a/single").

-define(MICROSOFT_TRANSLATOR_TEXT_ENDPOINT, "https://api.cognitive.microsofttranslator.com").


-spec translate( binary(), atom(), atom(), z:context() ) -> {ok, binary()} | {error, term()}.
translate(Text, From, To, Context) ->
    first(
        [
            microsoft_translation_services,
            google_translate_api
        ],
        [ Text, From, To ],
        Context).

%% @doc Try all translation services, return the first success result.
first([], _Args, _Context) ->
    {error, unavailable};
first([ F | Fs ], Args, Context) ->
    Key = list_to_atom( "autotrans_" ++ atom_to_list(F) ),
    case z_depcache:get(Key, Context) of
        {ok, disabled} ->
            first(Fs, Args, Context);
        undefined ->
            case erlang:apply(?MODULE, F, Args ++ [ Context ]) of
                {ok, _} = OK ->
                    OK;
                {error, ratelimit} ->
                    disable(Key, F, 120, "rate limited", Context),
                    first(Fs, Args, Context);
                {error, overload} ->
                    disable(Key, F, 60, "overload", Context),
                    first(Fs, Args, Context);
                {error, eacces} ->
                    disable(Key, F, 600, "access denied", Context),
                    first(Fs, Args, Context);
                {error, notfound} ->
                    disable(Key, F, 600, "not found", Context),
                    first(Fs, Args, Context);
                {error, Error} ->
                    % Some unknown error - assume it is transient
                    lager:error("autotrans function ~p returns error ~p", [ F, Error ]),
                    first(Fs, Args, Context)
            end
    end.

%% @doc Disable a translation service for a period of time.
disable(Key, Function, Timeout, Reason, Context) ->
    lager:info("autotrans function ~p is ~s", [ Function, Reason ]),
    z_depcache:set(Key, disabled, Timeout, Context).

%% @doc Translate a text with the free Google Translate API (no API key needed)
-spec google_translate_api( binary(), atom(), atom(), z:context() ) -> {ok, binary()} | {error, term()}.
google_translate_api(Text, From, To, _Context) when is_binary(Text) ->
    google_translate_api(Text, From, To).

%% @doc Translate a text with the free Google Translate API (no API key needed)
-spec google_translate_api( binary(), atom(), atom() ) -> {ok, binary()} | {error, term()}.
google_translate_api(Text, From, To) when is_binary(Text) ->
    Url = iolist_to_binary([
            ?GOOGLE_TRANSLATE_URL,
            "?client=gtx",
            "&ie=UTF-8&oe=UTF-8",
            "&dt=t",
            "&sl=", z_url:url_encode( z_convert:to_binary(From) ),
            "&tl=", z_url:url_encode( z_convert:to_binary(To) ),
            "&text=", z_url:url_encode( Text )
        ]),
    case z_url_fetch:fetch(Url, []) of
        {ok, {_FinalUrl, _Js, _Size, Body}} ->
            case catch mochijson2:decode(Body) of
                [ [ [ TransText, _SourceText | _ ] | _ ] | _ ] ->
                    {ok, TransText};
                _ ->
                    {error, format}
            end;
        {error, {403, _FinalUrl, _Hs, _Sz, _Body}} ->
            {error, ratelimit};
        {error, {404, _FinalUrl, _Hs, _Sz, _Body}} ->
            {error, notfound};
        {error, {401, _FinalUrl, _Hs, _Sz, _Body}} ->
            {error, eacces};
        {error, {503, _FinalUrl, _Hs, _Sz, _Body}} ->
            {error, overload};
        {error, _} = Error ->
            % Catch rate limit - save in depcache
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
