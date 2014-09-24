%%% -*- coding: latin-1 -*-
%%%---- BEGIN COPYRIGHT --------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2012, Rogvall Invest AB, <tony@rogvall.se>
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
%%% @author Malotte W L�nne <malotte@malotte.net>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    exoweb application supervisor.
%%% @end
%%% Created : 2012 by Malotte W L�nne
%%%-------------------------------------------------------------------
-module(exoweb_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, stop/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor. <br/>
%% Arguments are sent on to the supervisor.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args::list(term())) -> 
			{ok, Pid::pid()} | 
			ignore | 
			{error, Error::term()}.

start_link(Args) ->
    error_logger:info_msg("~p: start_link: args = ~p\n", [?MODULE, Args]),
    try supervisor:start_link({local, ?MODULE}, ?MODULE, Args) of
	{ok, Pid} ->
	    {ok, Pid, {normal, Args}};
	Error -> 
	    error_logger:error_msg("~p: start_link: Failed to start process, "
				   "reason ~p\n",  [?MODULE, Error]),
	    Error
    catch 
	error:Reason ->
	    error_logger:error_msg("~p: start_link: Try failed, reason ~p\n", 
				   [?MODULE,Reason]),
	    Reason
    end.

%%--------------------------------------------------------------------
%% @doc
%% Stops the supervisor.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(StartArgs::list(term())) -> ok | {error, Error::term()}.

stop(_StartArgs) ->
    exit(stopped).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
%% @private
init(_Args) ->
    error_logger:info_msg("~p: init: args = ~p,\n pid = ~p\n", 
			  [?MODULE, _Args, self()]),
    ExoS = exoweb_session_server,
    ExoC = exoweb_cookie_handler,
    %%ExoW = exoweb_inets,
    ExoSrv = {ExoS, {ExoS, start_link, [[]]}, permanent, 5000, worker, [ExoS]},
    ExoCook = {ExoC, {ExoC, start_link, [[]]}, permanent, 5000, worker, [ExoC]},
    %%ExoWeb = {ExoW, {ExoW, start_link, []}, permanent, 5000, worker, [ExoW]},
    Processes = [ExoSrv, ExoCook],
    %%Processes = [ExoSrv, ExoCook, ExoWeb],
    error_logger:info_msg("~p: About to start ~p\n", [?MODULE,Processes]),
    {ok, { {one_for_one, 0, 300}, Processes} }.

