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
%%% @author Malotte W Lönne <malotte@malotte.net>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%  ExoDM WebService login page.
%%%
%%%
%%% @end

-module(exoweb_login).
-include("exoweb.hrl").

%% Callbacks for js
-export([event/1]).

%%--------------------------------------------------------------------
%% @doc 
%% Callback from  js when an event has occured
%% @end
%%--------------------------------------------------------------------
-spec event({Tag::atom(), list(tuple())}) -> 
   ok | {error, Error::term()}.

event({login, Args} = Event) ->
    ?dbg("event:  ~p",[Event]),
    User = proplists:get_value(name, Args),
    Pass = proplists:get_value(password, Args),

    case login(User, Pass) of
	ok ->
	    ?dbg("event: login ~p ok.", [User]),
	    ok;
	{error, Reason} ->
	    ?dbg("event: login ~p failed, reason ~p.", [User, Reason]),
		{error, e2string(Reason)}
    end;
event({user, Args} = Event) ->
    ?dbg("event:  ~p",[Event]),
    User = proplists:get_value(user, Args),
    {ok, User};
event(Event) ->
    ?dbg("event: unknown event ~p",[Event]),
    ok.


e2string(Reason) when is_list(Reason) ->
    Reason;
e2string(no_contact_with_host) ->
    "No contact with ExoDM server!";
e2string(unknown_user) ->
    "Unknown user!";
e2string(_Reason) ->
    "Login failed".

login(User, Pass) ->
    exoweb_data_if:login(#exoweb_user{name = User, password = Pass}).

