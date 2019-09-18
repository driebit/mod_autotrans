%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Driebit BV
%% @doc Automatic translations for save resources.

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

-module(mod_autotrans).

-mod_title("Automatic translations").
-mod_description("Automatically add translations to resources").
-mod_author("Driebit").
-mod_depends([ mod_admin, mod_translation, mod_mqtt ]).

-export([
    init/1,
    is_enabled/1,
    observe_rsc_update_done/2,
    autotrans/3,
    autotrans/2
]).

-include_lib("zotonic.hrl").

init(_Context) ->
    _ = application:ensure_all_started(hackney),
    ok.

is_enabled(Context) ->
    SourceLang = m_config:get_value(mod_autotrans, source_language, Context),
    DestLang = m_config:get_value(mod_autotrans, target_language, Context),
    case {SourceLang, DestLang} of
        {A, A} -> false;
        {undefined, _} -> false;
        {_, undefined} -> false;
        {<<>>, _} -> false;
        {_, <<>>} -> false;
        {"", _} -> false;
        {_, ""} -> false;
        _ -> true
    end.

observe_rsc_update_done(#rsc_update_done{ id = Id }, Context) ->
    case is_enabled(Context) of
        true ->
            lager:info("autotrans: scheduling automatic translation for ~p", [Id]),
            Version = m_rsc:p_no_acl(Id, version, Context),
            TaskId = <<"autotrans-", (z_convert:to_binary( Id ))/binary>>,
            z_pivot_rsc:insert_task(?MODULE, autotrans, TaskId, [Id, Version], Context);
        false ->
            ok
    end.

%% @doc Add automatic translations to the given resource, check the resource version.
autotrans(Id, Version, Context) ->
    case m_rsc:p_no_acl(Id, version, Context) of
        Version ->
            do_autotrans(Id, Version, Context);
        _OtherVersion ->
            % Ignore, there will be another task for the new version
            ok
    end.

%% @doc Add automatic translations to the given resource.
autotrans(Id, Context) ->
    Version = m_rsc:p_no_acl(Id, version, Context),
    do_autotrans(Id, Version, Context).


do_autotrans(_Id, undefined, _Context) ->
    {error, enoent};
do_autotrans(Id, Version, Context) ->
    case is_enabled(Context) of
        true ->
            do_autotrans1(Id, Version, Context);
        false ->
            {error, disabled}
    end.

do_autotrans1(Id, Version, Context) ->
    case m_rsc:get_raw(Id, Context) of
        Raw when is_list(Raw) ->
            SourceLang = z_convert:to_atom( m_config:get_value(mod_autotrans, source_language, Context) ),
            DestLang = z_convert:to_atom( m_config:get_value(mod_autotrans, target_language, Context) ),
            Hashes = proplists:get_value(autotrans_hashes, Raw, #{}),
            Props = find_properties(Raw, #{}),
            % Drop properties without (or empty) source language
            Props1 = maps:filter(
                fun(_K, {trans, Tr}) ->
                    case proplists:get_value(SourceLang, Tr) of
                        undefined -> false;
                        <<>> -> false;
                        _ -> true
                    end
                end,
                Props),
            % Drop unchanged properties
            Props2 = maps:filter(
                fun(K, {trans, Tr}) ->
                    T = proplists:get_value(SourceLang, Tr),
                    Hash = crypto:hash(md5, T),
                    Hash =/= maps:get(K, Hashes, undefined)
                end,
                Props1),
            % Translate the changed properties
            PropsList = maps:to_list(Props2),
            case trans_props(PropsList, SourceLang, DestLang, Context) of
                {ok, []} ->
                    ok;
                {ok, TransProps} ->
                    lager:info("autotrans: page ~p adding automatic translation to ~p", [Id, DestLang]),
                    Language = proplists:get_value(language, Raw, [SourceLang]),
                    Language1 = case lists:member(DestLang, Language) of
                        true -> Language;
                        false -> [ DestLang | Language ]
                    end,
                    UpdateProps = [
                        {language, Language1},
                        {autotrans_hashes, update_hashes(TransProps, SourceLang, Hashes)}
                        | TransProps
                    ],
                    UpdateOptions = [
                        no_touch,
                        {escape_texts, false},
                        {expected, [ {version, Version} ]}
                    ],
                    case m_rsc:update(Id, UpdateProps, UpdateOptions, z_acl:sudo(Context)) of
                        {ok, _} ->
                            publish_changes(Id, DestLang, TransProps, Context),
                            ok;
                        {error, _} ->
                            ok
                    end;
                {error, _} = Error ->
                    Error
            end;
        undefined ->
            ok
    end.

publish_changes(Id, DestLang, TransProps, Context) ->
    Props = lists:map(
        fun
            ({K, _}) ->
                V = m_rsc:p(Id, K, Context),
                {K, z_trans:lookup_fallback(V, DestLang, Context)};
            (_) ->
                []
        end,
        TransProps),
    Msg = [
        {language, DestLang},
        {translations, lists:flatten(Props)}
    ],
    z_mqtt:publish(
        <<"~site/rsc/",(z_convert:to_binary(Id))/binary, "/autotrans">>,
        Msg,
        z_acl:sudo(Context)).


update_hashes(TransProps, SourceLang, Hashes) ->
    lists:foldl(
        fun({P, {trans, Tr}}, Acc) ->
            SourceHash = crypto:hash(md5, proplists:get_value(SourceLang, Tr)),
            Acc#{ P => SourceHash }
        end,
        Hashes,
        TransProps).

%% Find all translatable properties
find_properties([], Acc) ->
    Acc;
find_properties([ {Prop, {trans, _} = Tr} | Props ], Acc) ->
    Acc1 = Acc#{ Prop => Tr },
    find_properties(Props, Acc1);
find_properties([ _P | Props ], Acc) ->
    find_properties(Props, Acc).


trans_props([], _SourceLang, _DestLang, _Context) ->
    {ok, []};
trans_props(Props, SourceLang, DestLang, Context) ->
    % Make single HTML of all properties
    Marker = <<"xp1GQq7">>,
    Html = lists:map(
        fun({_Prop, {trans, Tr}}) ->
            [
                "<hr id=\"", Marker, "\">",
                proplists:get_value(SourceLang, Tr)
            ]
        end,
        Props),
    Html1 = iolist_to_binary(Html),
    case m_autotrans:translate(Html1, SourceLang, DestLang, Context) of
        {ok, TransHtml} ->
            % Split again
            Translations = tl(re:split(TransHtml, <<"<hr +id *= *\"", Marker/binary, "\" *>">>)),
            case length(Translations) =:= length(Props) of
                true ->
                    Zipped = lists:zip(Props, Translations),
                    {ok, lists:map(
                        fun({{Prop, {trans, Tr}}, Target}) ->
                            Target1 = z_string:trim(Target),
                            Tr1 = lists:sort([ {DestLang, Target1} | proplists:delete(DestLang, Tr) ]),
                            {Prop, {trans, Tr1}}
                        end,
                        Zipped)};
                false ->
                    lager:info("autotrans returned less translatable strings than requested"),
                    {error, notrans}
            end;
        {error, _} = Error ->
            Error
    end.


