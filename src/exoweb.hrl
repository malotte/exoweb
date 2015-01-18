%%% -*- coding: latin-1 -*-
%%%---- BEGIN COPYRIGHT -------------------------------------------------------
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
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% @author Malotte W Lönne <malotte@malotte.net>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    Defines needed for exoweb.
%%%
%%% Created 2013 by Malotte W Lönne
%%% @end
%%%-------------------------------------------------------------------
-ifndef(EXOWEB_HRL).
-define(EXOWEB_HRL, true).

-include_lib("lager/include/log.hrl").
%%-include_lib("gettext/include/gettext.hrl").

-define(EXOWEB, exoweb).

%% Enable simple language switch
-define(UTXT(S), 
	unicode:characters_to_binary(?TXT(S), utf32, utf8)).
-define(UTXT2(S,Lang),
	unicode:characters_to_binary(?TXT2(S,Lang), utf32, utf8)).

%% Session data for apply/confirm session
-record(exoweb_session,
	{id::integer(),
	 account::string(),
	 user::string(),
	 email::string(),
	 password::string(),
	 data::list({Key::atom(),Value::term()})}).

%% Cookie data for web session
-record(exoweb_cookie,
	{id::integer(),
	 pid::pid(),
	 account::string(),
	 user::string(),
	 password::string()}).

%% Records for data transfer
-record(exoweb_account,
	{name::string(),
	 email::string(),
	 password::string()}).

%% same as in exodm rpc:s
-define(USER_ATTRS, 
	[name, email, role, fullname, password, 
	'confirm-password', phone, skype]).

%% Roles
-define(INIT_ADMIN, "initial-admin").
-define(ADMIN, "admin").

-record(exoweb_user,
	{name::string(),
	 password::string(),
	 attributes::list({atom(), string()})}).

%% same as in exodm rpc:s
-define(DEVICE_ATTRS, 
	['device-id', 'server-key', 'device-key', msisdn, 'is-connected']).

%% 
-define(DEVICE_LIST_ATTRS, 
	['is-connected']). %% Changed, created, in-queue
	
%% Debug support
-define(dbg(Format, Args),
	lager:debug("~s(~p): " ++ Format, [?MODULE, self() | Args])).

%% Convenience defines
-ifndef(ee).
-define(ee(String, List), error_logger:error_msg(String, List)).
-endif.
-ifndef(ei).
-define(ei(String, List),  error_logger:info_msg(String, List)).
-endif.
-ifndef(l2i).
-define(l2i(List), list_to_integer(List)).
-endif.


-endif.
