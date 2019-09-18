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
    autotrans_task/3,
    autotrans/2
]).

% For testing
-export([
    find_properties/2,
    trans_props/4,
    update_block_properties/2
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
            z_pivot_rsc:insert_task(?MODULE, autotrans_task, undefined, [Id, Version], Context);
        false ->
            ok
    end.

%% @doc Add automatic translations to the given resource, check the resource version.
autotrans_task(Id, Version, Context) ->
    case m_rsc:p_no_acl(Id, version, Context) of
        Version ->
            case do_autotrans(Id, Version, Context) of
                ok ->
                    ok;
                {error, retry} ->
                    lager:info("autotrans: delaying translation of ~p (version ~p) for 600 seconds",
                               [Id, Version]),
                    {delay, 600};
                {error, _} ->
                    ok
            end;
        OtherVersion ->
            % Ignore, there will be another task for the new version
            lager:info("autotrans: dropping automatic translation for ~p for version ~p, as version is now ~p",
                       [Id, Version, OtherVersion]),
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
                    lager:info("autotrans: page ~p didn't need new translations for ~p", [Id, DestLang]),
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
                        | update_block_properties(TransProps, Raw)
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
    Blocks = case m_rsc:p(Id, blocks, Context) of
        undefined -> [];
        Bs -> Bs
    end,
    Props = lists:map(
        fun
            ({{block, Name, K}, _}) ->
                case find_block(Name, Blocks) of
                    undefined -> [];
                    B ->
                        V = proplists:get_value(K, B),
                        KB = z_convert:to_binary(K),
                        {<<"blocks$", Name/binary, "$", KB/binary>>, z_trans:lookup_fallback(V, DestLang, Context)}
                end;
            ({K, _}) when is_atom(K) ->
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

update_block_properties(TransProps, Raw) ->
    Blocks = case proplists:get_value(blocks, Raw, []) of
        Bs when is_list(Bs) -> Bs;
        _ -> []
    end,
    lists:foldl(
        fun
            ({{block, Name, K}, V}, Acc) ->
                AccBlocks = proplists:get_value(blocks, Acc, Blocks),
                AccBlocks1 = case find_block(Name, AccBlocks) of
                    undefined ->
                        AccBlocks;
                    B ->
                        B1 = [ {K, V} | proplists:delete(K, B) ],
                        replace_block(Name, B1, AccBlocks)
                end,
                [ {blocks, AccBlocks1} | proplists:delete(blocks, Acc) ];
            (KV, Acc) ->
                [ KV | Acc ]
        end,
        [],
        TransProps).

find_block(_Name, []) ->
    undefined;
find_block(Name, [ B | Blocks ]) when is_list(B) ->
    case proplists:get_value(name, B) of
        Name -> B;
        _ -> find_block(Name, Blocks)
    end;
find_block(Name, [ _ | Blocks ]) ->
    find_block(Name, Blocks).

replace_block(Name, Block, Blocks) ->
    lists:map(
        fun
            (B) when is_list(B) ->
                case proplists:get_value(name, B) of
                    Name -> Block;
                    _ -> B
                end;
            (B) ->
                B
        end,
        Blocks).

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
find_properties([ {Prop, {trans, _} = Tr} | Props ], Acc) when is_atom(Prop) ->
    Acc1 = Acc#{ Prop => Tr },
    find_properties(Props, Acc1);
find_properties([ {blocks, Blocks} | Props ], Acc) ->
    Acc1 = find_block_properties(Blocks, Acc),
    find_properties(Props, Acc1);
find_properties([ _P | Props ], Acc) ->
    find_properties(Props, Acc).

find_block_properties([], Acc) ->
    Acc;
find_block_properties([ Block | Blocks ], Acc) when is_list(Block) ->
    case proplists:get_value(name, Block) of
        undefined ->
            find_block_properties(Blocks, Acc);
        Name ->
            BAcc = find_properties(Block, #{}),
            BAcc1 = maps:fold(
                fun(K, V, B) ->
                    B#{ {block, Name, K} => V }
                end,
                #{},
                BAcc),
            Acc1 = maps:merge(Acc, BAcc1),
            find_block_properties(Blocks, Acc1)
    end;
find_block_properties(_, Acc) ->
    Acc.

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


