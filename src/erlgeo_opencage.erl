%%
%% %CopyrightBegin%
%%
%% UXEON Sp. z o.o. 2016. All Rights Reserved.
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
%% %CopyrightEnd%
%%
%% =============================================================================
%% @author Bartosz Kołodziej <bartosz.kolodziej@uxeon.com>
%% @end
%% =============================================================================

%% @doc geocoder

-module('erlgeo_opencage').
-author("Bartosz Kołodziej <bartosz.kolodziej@uxeon.com>").
%-vsn(1).

%% -----------------------------------------------------------------------------
%% External exports
%% -----------------------------------------------------------------------------
-export([
    reverse/2,
    reverse/3,
    forward/1,
    forward/2
]).

%% -----------------------------------------------------------------------------
%% Macros
%% -----------------------------------------------------------------------------
-define(DECODE(Data), jsone:decode(Data,[
    {object_format,map},
    {allow_ctrl_chars,true}
])).
-define(DEFAULT_OPTIONS,#{
    no_annotations => true
}).

%% -----------------------------------------------------------------------------
%% Records
%% -----------------------------------------------------------------------------
-include_lib("xmerl/include/xmerl.hrl").

%% =============================================================================
%% External functions
%% =============================================================================
%% -----------------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%% -----------------------------------------------------------------------------
-spec reverse(float(),float()) -> map();
             (list(), list()) -> map();
             (binary(),binary()) -> map().
reverse(Lon,Lat) -> reverse(Lon,Lat,?DEFAULT_OPTIONS).

%% -----------------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%% -----------------------------------------------------------------------------
-spec reverse(float(),float(),map() | list()) -> map();
             (list(), list(),map() | list()) -> map();
             (binary(),binary(),map() | list()) -> map().
reverse(Lon,Lat,Options) when is_list(Options) ->
    PropList = lists:map(fun(Element) when is_tuple(Element) -> Element;
                 (Element) when is_atom(Element) -> {Element,true}
    end,Options),
    OptionsMap = maps:from_list(PropList),
    reverse(Lon,Lat,OptionsMap);

reverse(Lon,Lat,Options) when is_float(Lon) andalso is_float(Lat) ->
    StrLon = io_lib:format("~f",[Lon]),
    StrLat = io_lib:format("~f",[Lat]),
    reverse(StrLon,StrLat,Options);

reverse(Lon,Lat,Options) when is_binary(Lon) andalso is_binary(Lat) ->
    StrLon = binary_to_list(Lon),
    StrLat = binary_to_list(Lat),
    reverse(StrLon,StrLat,Options);

reverse(Lon,Lat,Options) when is_list(Lon) andalso is_list(Lat) ->
    send_request([Lat,"%2C",Lon],Options).

%% -----------------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%% -----------------------------------------------------------------------------
-spec forward(string() | binary()) -> map().
forward(Address) ->
    forward(Address,?DEFAULT_OPTIONS).

%% -----------------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%% -----------------------------------------------------------------------------
-spec forward(string() | binary(),map() | list()) -> map().
forward(Address,Options) ->
    send_request(encode(Address),Options).

%% =============================================================================
%% Internal functions
%% =============================================================================
%% -----------------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%% -----------------------------------------------------------------------------
send_request(Query,Options) ->
    URLOptions = maps:values(maps:map(fun
        (no_annotations,_) -> "&no_annotations=1";
        (no_dedupe,_) -> "&no_dedupe=1";
        (no_record,_) -> "&no_reocrd=1";
        (language,Lang) when is_binary(Lang) ->
            "&language=" ++ binary_to_list(Lang);
        (language,Lang) when is_list(Lang) ->
            "&language=" ++ Lang;
        (min_confidence,C) when is_integer(C) ->
            "&min_confidence=" ++ integer_to_list(C);
        (limit,Limit) when is_integer(Limit) ->
            "&limit=" ++ integer_to_list(Limit)
    end,Options)),
    OpenCageKey = case application:get_env(erlgeo,opencage_key) of
        {ok,"SET_YOUR_KEY"} -> error({invalid_env,opencage_key});
        {ok,EnvKey} when is_list(EnvKey) -> EnvKey;
        {ok,EnvKey} when is_binary(EnvKey) -> binary_to_list(EnvKey);
        undefined -> error({missing_env,opencage_key});
        _ -> error({invalid_env,opencage_key})
    end,
    URL = [
        "https://api.opencagedata.com/geocode/v1/json?q=",
        Query,
        "&pretty=0",
        URLOptions,
        "&key=",
        OpenCageKey
    ],
    Request = {
        lists:flatten(URL),
        []
    },
    {ok, {_,_,RespBody}} = httpc:request(get, Request, [], []),
    Decoded = ?DECODE(list_to_binary(RespBody)),
    {ok,Decoded}.

%% -----------------------------------------------------------------------------
%% @doc encode uri (because http_uri:encode does not support UTF-8)
%% @spec
%% @end
%% -----------------------------------------------------------------------------
-spec encode(list() | binary()) -> list().
encode(S) when is_list(S) ->
    encode(unicode:characters_to_binary(S));
encode(<<C:8, Cs/binary>>) when C >= $a, C =< $z ->
    [C] ++ encode(Cs);
encode(<<C:8, Cs/binary>>) when C >= $A, C =< $Z ->
    [C] ++ encode(Cs);
encode(<<C:8, Cs/binary>>) when C >= $0, C =< $9 ->
    [C] ++ encode(Cs);
encode(<<C:8, Cs/binary>>) when C == $. ->
    [C] ++ encode(Cs);
encode(<<C:8, Cs/binary>>) when C == $- ->
    [C] ++ encode(Cs);
encode(<<C:8, Cs/binary>>) when C == $_ ->
    [C] ++ encode(Cs);
encode(<<C:8, Cs/binary>>) ->
    escape_byte(C) ++ encode(Cs);
encode(<<>>) ->
    "".

-spec escape_byte(byte()) -> list().
escape_byte(C) ->
    "%" ++ hex_octet(C).

-spec hex_octet(byte()) -> list().
hex_octet(N) when N =< 9 ->
    [$0 + N];
hex_octet(N) when N > 15 ->
    hex_octet(N bsr 4) ++ hex_octet(N band 15);
hex_octet(N) ->
    [N - 10 + $a].
