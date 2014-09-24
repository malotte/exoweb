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
%%%  ExoDM WebService apply page.
%%%
%%% @end
-module (exoweb_apply).

-include("exoweb.hrl").

%% Callbacks for js
-export([event/2]).

-import(exoweb_lib, [get_env/2]).

-define(WEBSITE, "www.feuerlabs.com").

%--------------------------------------------------------------------
%% @doc 
%% Callback from js when an event has occored
%% @end
%%--------------------------------------------------------------------
-spec event(Tag::atom(), Args::list(tuple())) -> 
    ok | {error, Error::term()}.

event(send, Args) ->
    ?dbg("event: send ~p", [Args]),
    Email = proplists:get_value(email, Args),
    PassWord = proplists:get_value(password, Args),
    case is_email_ok(Email) of
	true ->
	    case send_email(Email, PassWord) of
		ok ->
		    ?dbg("event: Confirm mail sent to: ~p", [Email]), 
		    ok;
		{error, _Reason} = E ->
		    ?dbg("event: Confirm not mail sent to: ~p, reason ", 
			 [Email, E]), 
		    E
	    end;
	false ->
	    {error, email_in_use}
    end.
	    

%%--------------------------------------------------------------------
%% @doc 
%% Internal
%% @end
%%--------------------------------------------------------------------
is_email_ok(_Email) ->
    %% Verify against exodm ???
    true.

%%--------------------------------------------------------------------
send_email(Email, PassWord) ->
    ?dbg("send_email: send ~p", [Email]),
    %% Mail service set up
    Flags = [{relay, get_env(smtp_relay, smtp_relay_required)},
  	     {username, get_env(smtp_user, "")},
	     {password, get_env(smtp_password, "")},
	     {port, get_env(smtp_port, 25)},
	     {ssl, get_env(smtp_ssl, false)},
	     %% always,never,if_available
	     {tls, get_env(smtp_tls, if_available)}, 
	     %% always,never,if_available
	     {auth, get_env(smtp_auth, if_available)}
	    ],
    
    Account = account(Email),
    ?dbg("send_email: account ~p", [Account]),
    SessionId = 
        exoweb_session_server:store(
	    #exoweb_session{account = Account, 
			    email = Email, 
	                    password = PassWord}),
    
    %% Create mail
    Sender = get_env(exodm_email,""),
    From = [["From: ", get_env(exodm_from,"")]],
    To = [["To: ", Email]],
    %% Language support ??
    Body = "Confirm creation of account " ++ Account ++ " at " ++ 
           url(Account, Email, SessionId),
    Subject = [["Subject: ", "exosense account confirmation"]],
    Date = [["Date: ", smtp_util:rfc5322_timestamp()]],
    MessageID = [["Message-ID:", smtp_util:generate_message_id()]],
    Headers = 
	[[H,"\r\n"] ||  H <- From ++ To ++ Subject ++ Date ++ MessageID ],
    Message = [Headers,"\r\n",Body],
        ?dbg("event: sending ~p, from ~p (~p) to ~p(~p), flags ~p.",
	[Message, From, Sender, To, Email, Flags]),

    case gen_smtp_client:send({Sender, [Email], Message}, Flags) of
	{ok,_Pid} ->
	    ok;
	Error ->     
	    ?ei("send email failed, reason ~p",[Error]),
	    exoweb_session_server:retreive(SessionId),
	    Error
    end.
 
%%--------------------------------------------------------------------
account(Email) when is_list(Email) ->
    [Name | _Rest] = string:tokens(Email, ".@-"),
    Name ++ integer_to_list(random()).
    %% Verify non-existing account ??

%%--------------------------------------------------------------------
url(Account, Email, Session) ->
    Host = case exoweb:external_hostname() of
	"localhost"= H -> H ++":" ++ integer_to_list(exoweb:port());
	_H -> ?WEBSITE %% ??
    end,
    "http://" ++ Host ++ "/confirm?account=" ++ Account ++
    "&email=" ++ Email ++ "&session=" ++ integer_to_list(Session).

%%--------------------------------------------------------------------
random() ->
    %% Initialize
    random:seed(now()),
    %% Retreive
    random:uniform(16#1000).
