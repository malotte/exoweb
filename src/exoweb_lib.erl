%%% -*- coding: latin-1 -*-
%%%---- BEGIN COPYRIGHT --------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2013, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ----------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author  Malotte W Lönne <malotte@malotte.net>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    exoweb application.
%%%    Library functions
%%%
%%% Created : 2013 by Malotte W Lönne
%%% @end
%%%-------------------------------------------------------------------
-module(exoweb_lib).
-include_lib("inets/include/httpd.hrl").
-include_lib ("nitrogen_core/include/wf.hrl").
-include("exoweb.hrl").

%% Support functions
-export([is_phone_no/2,
	 content_type_html/0,
	 parse_options/1,
	 to_utf8/1,
	 get_env/2,
	 sec/0,
	 error_txt/1,
	 roles2string/1]).


%%--------------------------------------------------------------------
%% @doc 
%% Phone number validation
%% @end
%%--------------------------------------------------------------------
-spec is_phone_no(Tag::atom(), String::string()) -> boolean().

is_phone_no(phone, "") ->
    true;
is_phone_no(phone, String) when is_list(String) ->
    Stripped = [C || C <- String, not lists:member(C, " -")],
    Num = case Stripped of
	      "+" ++ N -> N;
	      N -> N
	  end,
    try list_to_integer(Num) of
	_Int -> true
    catch
	error:_ -> false
    end.
	    
%%--------------------------------------------------------------------
%% @doc 
%% Set nitrogen content type.
%% @end
%%--------------------------------------------------------------------
-spec content_type_html() -> nitrogen_element().

content_type_html() ->
    CharSet = 
	case gettext:lang2cset(get(gettext_language)) of
	    {ok, CSet} -> CSet;
	    _ -> "iso-8859-1"
	end,
    %% ?dbg("CharSet=~p\n", [CharSet]),
    wf:content_type("text/html; charset='"++CharSet++"'").


%%--------------------------------------------------------------------
%% @doc 
%% Parses an url for data
%% @end
%%--------------------------------------------------------------------
-spec parse_options(FullUrl::string()) -> KeyValueList::list(tuple()).

parse_options(FullUrl) ->
    ?dbg("parse_options: Url ~p.", [FullUrl]),
    [_File, Query] = string:tokens(FullUrl,"?"),
    ?dbg("parse_options: Query ~p.", [Query]),
    KeyValueListString = string:tokens(Query,"&"),
    ?dbg("parse_options: KVstring ~p.", [KeyValueListString]),
    lists:map(fun(KeyValueString) ->
		      case string:tokens(KeyValueString,"=") of
			  [K, V] -> {list_to_atom(K), V};
			  [K] -> {list_to_atom(K), ""}
		      end
	      end,
	      KeyValueListString).


%%--------------------------------------------------------------------
%% @doc 
%% Convert chars to unicode binary.
%% @end
%%--------------------------------------------------------------------
-spec to_utf8(Chars::list(integer() | binary())) -> term().

to_utf8(Chars) ->
    unicode:characters_to_binary(Chars, utf32, utf8).


%%--------------------------------------------------------------------
%% @doc 
%% Get an application environment variable; fallback to a default value.
%% @end
%%--------------------------------------------------------------------
-spec get_env(Key::atom(), Default::term()) -> Value::term().

get_env(Key, Default) ->
    case application:get_env(exoweb, Key) of
        {ok, Value} -> Value;
        _           -> Default
    end.

%%--------------------------------------------------------------------
%% @doc
%% Seconds utility, gives timestamp as seconds.
%% @end
%%--------------------------------------------------------------------
-spec sec() -> Sec::integer().
sec() ->
    {MegaSec, Sec, _MilliSec} = os:timestamp(),
    1000 * MegaSec + Sec.


%%--------------------------------------------------------------------
%% Common parts 
%%--------------------------------------------------------------------

error_txt([{"code",_C},{"message",Msg},{"data",_Data}]) ->
    Msg;
error_txt(Txt) when is_list(Txt) ->
    Txt;
error_txt(Atom) when is_atom(Atom) ->
    atom_to_list(Atom).


roles2string(Roles) ->
    string:strip(lists:flatten([[R," "] || R <- Roles]), right).
   
