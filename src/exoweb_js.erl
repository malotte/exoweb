%%% -*- coding: latin-1 -*-
%%%---- BEGIN COPYRIGHT --------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2014, Rogvall Invest AB, <tony@rogvall.se>
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
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%  ExoDM WebService javascript interface.
%%%
%%% @end

-module(exoweb_js).

-include("exoweb.hrl").

%% Cookie handling api for js interface
-export([create_cookie/2,
	 delete_cookie/0,
	 wrapper/3]).
	 
%%%===================================================================
%%% Javascript cookie handling
%%%===================================================================

%% Called at login
-spec create_cookie(WebSocket::term(), list(tuple())) -> ok.

create_cookie(WebSocket, Args) ->
    ?dbg("create_cookie: ~p.",[Args]),
    User = proplists:get_value(name, Args),
    Pass = proplists:get_value(password, Args),
    case user2account(User) of
	Account when is_list(Account) ->
	    ?dbg("create_cookie: ~p, ~p, ~p.", [Account, User, Pass]),
	    WebCookie = 
		exoweb_cookie_handler:store(
		  #exoweb_cookie{pid = WebSocket,
		                 user = User, 
				 account = Account,
				 password = Pass}),
	    ?dbg("create_cookie: cookie ~p.", [WebCookie]),
	    %% Set cookie at client
	    ok = wse:set(WebSocket, wse:document(), "cookie", 
			 "id=" ++ integer_to_list(WebCookie) ++ "; path=/" ++
			     "; expires=Thu, 01 Jan 2070 00:00:00 UTC"),
	                  %% "id=" ++ integer_to_list(WebCookie) ++ "; path=/");
	                  %% If adding path, add in logout (controllers.js) 
	                  %% as well
	    %% Change location on client
	    Path = proplists:get_value(path, Args),
	    ok = wse:set(WebSocket,  wse:window(), ["location", "href"], Path);
 	{error, illegal_user} = E ->
	    ?dbg("create_cookie: illegal user ~p.", [User]),
	    E
    end.


%% Called at logout 
-spec delete_cookie() -> ok.
delete_cookie() ->
    ?dbg("delete_cookie: ~p.",[self()]),
    %% First check if same web session as login
    case exoweb_cookie_handler:retreive(self()) of
	{ok, _Cookie=#exoweb_cookie{}} ->
	    ok;
	{error, not_found} ->
	    %% Second check if websession cookie
	    exoweb_cookie_handler:retreive(session_cookie()),
	    ok
    end.
  
%% Called when accessing exodm
wrapper(M, F, {Event, Args} = A) ->
    ?dbg("wrapper: ~p, ~p, ~p.",[M, F, A]),
    case cookie_check(self()) of
	{ok, _Cookie=#exoweb_cookie{ user = User, 
				     account = Account,
				     password = PassWord}} ->
	    ?dbg("wrapper:calling ~p:~p(~p)", [M,F,A]),
	    apply(M, F, [{Event, [{user, User}, 
				  {account, Account},
				  {password, PassWord} |Args]}]);
	{error, Reason} ->
	    ?dbg("wrapper: wrong cookie ~p.",[Reason]),
	    {error, illegal_cookie}
    end;
wrapper(M, F, A) ->
    ?dbg("wrapper: faulty args ~p, ~p, ~p.",[M, F, A]),
    {error, faulty_arguments}.
	
%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------

user2account("exodm-admin") ->
    "exodm";
user2account("getaround-admin") ->
    "getaround";
user2account(User) ->
    case string:tokens(User, "/") of
	[Account, _Name] -> Account;
	_O -> {error, illegal_user}
    end.
	     
cookie_check(Pid) ->
    %% First check if same web session as login
    case exoweb_cookie_handler:read(Pid) of
	{ok, _Cookie} = OK ->
	    OK;
	{error, not_found} ->
	    cookie_check()
    end.
    
cookie_check() ->
    %% Second check if websession cookie ok
    case exoweb_cookie_handler:read(session_cookie()) of
	{ok, _Cookie} = OK ->
	    OK;
	{error, Reason} ->
	    ?dbg("cookie_check: wrong cookie ~p.",[Reason]),
	    {error, illegal_cookie}
    end. 

session_cookie() ->
    case wse:session_header('Cookie') of
	{ok, Cookie} ->
	    ["id", CookieId] = string:tokens(Cookie, "="),
	    CookieId;
	{error, Reason} ->
	    ?dbg("session_cookie: no cookie!! ~p.",[Reason]),
	    undefined
    end.

