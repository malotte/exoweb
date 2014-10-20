%%% coding: latin-1
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
%%%  Exosense WebService confirm page.
%%%
%%% @end
-module (exoweb_confirm).

-include("exoweb.hrl").

%% Callbacks for js
-export([event/1]).

-import(exoweb_lib, [get_env/2, parse_options/1]).


%--------------------------------------------------------------------
%% @doc 
%% Callback from js when an event has occured.
%% @end
%%--------------------------------------------------------------------
-spec event({Tag::atom(), list(tuple())}) -> 
   ok | {error, Error::term()}.


event({confirm, Args}) ->
    ?dbg("event: confirm ~p.", [Args]),
    SessionId = proplists:get_value(session, Args),
    Account = proplists:get_value(account, Args),
    Email = proplists:get_value(email, Args),
    ?dbg("confirm_dialog: session id ~p, account ~p, email ~p.", 
	[SessionId, Account, Email]),
    case exoweb_session_server:retreive(SessionId) of
	{ok, Session=#exoweb_session {account = Account, email = Email}} ->
	    confirm_account(session_ok(Session));
	{ok, OtherSession} ->
	    ?dbg("confirm_dialog: session mismatch ~p.", [OtherSession]),
	    session_nok(Account, mismatch);
	{error, Reason} ->
	    ?dbg("confirm_dialog: session error ~p.", [Reason]),
	    session_nok(Account, Reason)
    end;
event(Event) ->
    ?dbg("event: unknown event ~p",[Event]),
    ok.

confirm_account({confirm, Account, Email, PassWord }) ->
   case create_account(Account, Email, PassWord) of
	ok ->
	    ?dbg("event: create of account ~p ok.", [Account]),
	    %% Get init_admin ??
	    Admin = Account ++ "/admin",
	    {ok, Admin};
	{error, Reason} = E ->
	    ?dbg("event: create of account ~p failed, reason ~p.", 
		[Account, Reason]),
	    E
    end.

session_ok(#exoweb_session {account = Account, email = Email, password = PassWord}) -> 
    ?dbg("session_ok: account ~p.", [Account]),
   {confirm, Account, Email, PassWord }.

session_nok(Account, not_found) -> 
    ?dbg("session_nok: account ~p.", [Account]),
    {error, "Session expired"};
session_nok(Account, _Reason) -> 
    ?dbg("session_nok: account ~p.", [Account]),
    {error, "Incorrect link"}.


create_account(Account, Email, PassWord) ->
    exoweb_data_if:create(
	#exoweb_account{name = Account, email = Email, password = PassWord}).
