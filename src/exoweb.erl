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
%%%-------------------------------------------------------------------
%%% @author  Malotte W Lönne <malotte@malotte.net>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    exoweb application.
%%%    Exodm web interface
%%%
%%% Created : 2013 by Malotte W Lönne
%%% @end
%%%-------------------------------------------------------------------

-module(exoweb).

-behaviour(application).
-include("exoweb.hrl").

%% Application api
-export([start/2, 
	 stop/1]).

%% Web api
-export([hostname/0, 
	 external_hostname/0, 
	 ip/0, 
	 port/0, 
	 servername/0, 
	 log_dir/0, 
	 docroot/0, 
	 template/0, 
	 top_dir/0, 
	 priv_dir/0, 
	 gettext_dir/0
        ]).

%% Mandatory anvironment
-define(ENV, [
	      %% exodm
	      exodm_url,
	      exodm_user,
	      exodm_password,
	      exodm_from, 
	      exodm_email,

	      %% smtp
	      smtp_relay, 
	      smtp_user,
	      smtp_password,
	      smtp_port
	     ]).

%% Shortcut API
-export([start/0,
	 start_dbg/0,
	 stop/0]).

-import(exoweb_lib, [get_env/2]).

%% Hardcoded in web pages !!!
-define(WSE_PORT, 19999).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the application.<br/>
%% Arguments are ignored, instead the options for the application server are 
%% retreived from the application environment (sys.config).
%%
%% @end
%%--------------------------------------------------------------------
-spec start(StartType:: normal | 
			{takeover, Node::atom()} | 
			{failover, Node::atom()}, 
	    StartArgs::term()) -> 
		   {ok, Pid::pid()} |
		   {ok, Pid::pid(), State::term()} |
		   {error, Reason::term()}.

start(_StartType, _StartArgs) ->
    ?ei("~p: start: arguments ignored.\n", [?MODULE]),
    case verify_env() of 
	Env when is_list(Env) ->  
	    ?dbg("start: environment ~p", [Env]),
	    exoweb_sup:start_link([]);
	{error, missing_env} ->
	    stop()
    end.
    

%%%===================================================================
%%% Web API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Used when setting up web server
%%
%% @end
%%--------------------------------------------------------------------
hostname() ->
    {ok,Host} = inet:gethostname(),
    Host.
external_hostname() -> get_env(external_hostname, hostname()).
ip()                -> get_env(ip, {127,0,0,1}).
port()              -> get_env(port, 8282).
servername()        -> get_env(servername, "localhost").
log_dir()           -> get_env(log_dir, code:lib_dir(?EXOWEB,log)).
docroot()           -> get_env(doc_root, code:lib_dir(?EXOWEB,www)).
template()          -> get_env(template, 
			       filename:join(
				 [code:lib_dir(?EXOWEB, templates),
				  "grid.html"])).
top_dir() ->
    filename:join(
      ["/"|lists:reverse(tl(lists:reverse(
			      string:tokens(filename:dirname(
					      code:which(?MODULE)),"/"))))]).
priv_dir() ->    top_dir()++"/priv".
gettext_dir() -> priv_dir().

%%--------------------------------------------------------------------
%% @doc
%% Stops the application.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(State::term()) -> ok | {error, Error::term()}.

stop(_State) ->
    ok.

%%--------------------------------------------------------------------
verify_env() ->
    case application:get_all_env(exoweb) of
	undefined -> {error, missing_env};
	Opts when is_list(Opts) -> mandatory(?ENV, Opts)
    end.

					
mandatory([], Opts) ->			   
    Opts;
mandatory([Env | Rest], Opts) ->	
    case proplists:get_value(Env, Opts) of
	undefined -> 
	    ?ee("~p: start: missing environment variable ~p.~n", 
		[?MODULE, Env]),
	    {error, missing_env};
	_Found -> 
	    mandatory(Rest, Opts)
    end.

%% @private
start() ->
    call([crypto, asn1, public_key, ssl,
	  sasl, gettext, lager, ale, gen_smtp, exoweb], 
	 start),
    wse_server:start(?WSE_PORT, [{name, exoweb_wse_server}]).

start_dbg() ->
    call([crypto, asn1, public_key, ssl,
	  sasl, gettext, lager, ale, gen_smtp], 
	 start),
    ale:debug_gl([exoweb, 
		  exoweb_sup, 
		  exoweb_yaws, 
		  exoweb_index,
		  exoweb_apply, 
		  exoweb_login, 
		  exoweb_yang, 
		  exoweb_user,
		  exoweb_data_if, 
		  exoweb_confirm, 
		  exoweb_session_server,
		  exoweb_cookie_handler,
		  exodm_json_api,
		  exoweb_device, 
		  exoweb_table, 
		  exoweb_js]),

    call([exoweb], start),
    wse_server:start(?WSE_PORT, [{name, exoweb_wse_server}]).

stop() ->
    wse_server:stop(exoweb_wse_server),
    yaws:stop(), 
    call([exoweb, gen_smtp, ale, lager, nprocreg, gettext],
	 stop).

call([], _F) ->
    ok;
call([App|Apps], F) ->
    ?ei("~p: ~p\n", [F,App]),
    case {F, application:F(App)} of
	{start, {error, {not_started,App1}}} ->
	    call([App1,App|Apps], F);
	{start, {error, {already_started,App}}} ->
	    call(Apps, F);
	{F, ok} ->
	    call(Apps, F);
	{_F, Error} ->
	    Error
    end.


