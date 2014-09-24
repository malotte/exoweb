%%% -*- coding: latin-1 -*-
%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2013-2014 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% @author Malotte W Lönne <malotte@malotte.net>
%%% @doc
%%%    Exoweb session server for apply/confirm session.
%%% Created : December 2013 by Malotte W Lönne
%%% @end
-module(exoweb_session_server).
-behaviour(gen_server).

-include_lib("lager/include/log.hrl").
-include("exoweb.hrl").

%% general api
-export([start_link/1, 
	 stop/0]).

%% functional api
-export([store/1,
	 retreive/1,
	 read/1]).

%% gen_server callbacks
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).

%% test api
-export([dump/0]).

-import(exoweb_lib, [sec/0]).

-define(SERVER, ?MODULE). 

%% Loop data
-record(ctx,
	{
	  state = running,
	  sessions,
	  queue
	}).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server.
%% Loads configuration from File.
%% @end
%%--------------------------------------------------------------------
-spec start_link([]) -> 
			{ok, Pid::pid()} | 
			ignore | 
			{error, Error::term()}.

start_link(Opts) ->
    lager:info("~p: start_link: args = ~p\n", [?MODULE, Opts]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).


%%--------------------------------------------------------------------
%% @doc
%% Stops the server.
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok | {error, Error::term()}.

stop() ->
    gen_server:call(?SERVER, stop).


%%--------------------------------------------------------------------
%% @doc
%% Store session data
%%
%% @end
%%--------------------------------------------------------------------
-spec store(Session::#exoweb_session{}) -> 
		   Session::integer() | {error, Error::atom()}.

store(Session) when is_record(Session, exoweb_session) ->
    %% Or execute here, change ets to public ??
    gen_server:call(?SERVER, {store, Session}).

%%--------------------------------------------------------------------
%% @doc
%% Retrieve session data
%%
%% @end
%%--------------------------------------------------------------------
-spec retreive(Id::integer() | string()) -> 
		      {ok, Session::#exoweb_session{}} | 
		      {error, Error::atom()}.

retreive(Id) when is_list(Id) ->
    call(Id, fun retreive/1);
retreive(Id) when is_integer(Id) ->
    %% Or execute here, change ets to public??
    gen_server:call(?SERVER, {retreive, Id}).

%%--------------------------------------------------------------------
%% @doc
%% Read session data
%%
%% @end
%%--------------------------------------------------------------------
-spec read(Id::integer() | string()) -> 
		      {ok, Session::#exoweb_session{}} | 
		      {error, Error::atom()}.

read(Id) when is_list(Id) ->
    call(Id, fun read/1);
read(Id) when is_integer(Id) ->
    %% Or execute here, change ets to public ??
    gen_server:call(?SERVER, {read, Id}).

%%--------------------------------------------------------------------
%% @doc
%% Dumps data to standard output.
%%
%% @end
%%--------------------------------------------------------------------
-spec dump() -> ok | {error, Error::atom()}.

dump() ->
    gen_server:call(?SERVER, dump).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
-spec init(Args::list()) -> 
		  {ok, Ctx::#ctx{}} |
		  {stop, Reason::term()}.

init(_Args) ->
    lager:info("~p: init: args = ~p,\n pid = ~p\n", [?MODULE, _Args, self()]),
    SessionTable = ets:new(exoweb_session_table, 
			   [private, ordered_set, named_table,
			    {keypos, #exoweb_session.id}]),
    TimeQueue = ets:new(exoweb_queue_table, 
			[private, ordered_set, named_table]),
    {ok, #ctx {sessions = SessionTable, queue = TimeQueue}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages.
%% Request can be the following:
%% <ul>
%% <li> dump - Writes loop data to standard out (for debugging).</li>
%% <li> stop - Stops the application.</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-type call_request()::
	{store, Session::#exoweb_session{}} |
	{retreive, Id::integer()} |
	{read, Id::integer()} |
	dump |
	stop.

-spec handle_call(Request::call_request(), From::{pid(), Tag::term()}, Ctx::#ctx{}) ->
			 {reply, Reply::term(), Ctx::#ctx{}} |
			 {noreply, Ctx::#ctx{}} |
			 {stop, Reason::atom(), Reply::term(), Ctx::#ctx{}}.

handle_call({store, Session=#exoweb_session{}}, _From, 
	    Ctx=#ctx {sessions = Sessions, queue = Queue}) ->
    ?dbg("handle_call: store ~p", [Session]),
    <<Id:64>> = crypto:rand_bytes(8),
    
    %% Session will expire in 24 hours
    ets:insert(Queue, {sec() + 24*60*60, Id}),
    ets:insert(Sessions, Session#exoweb_session {id = Id}), 

    clean_tables(Ctx),

    {reply, Id, Ctx};

handle_call({retreive, Id}, _From, 
	    Ctx=#ctx {sessions = Sessions, queue = Queue}) ->
    ?dbg("handle_call: retreive ~p", [Id]),
    
    Reply = 
	case ets:lookup(Sessions, Id) of
	    [Session] ->
		ets:delete(Sessions, Id), 
		ets:match_delete(Queue, {'_', Id}),
		{ok, Session};
	    [] ->
		{error, not_found}
	end,
    clean_tables(Ctx),

    {reply, Reply, Ctx};

handle_call({read, Id}, _From, Ctx=#ctx {sessions = Sessions}) ->
    ?dbg("handle_call: read ~p", [Id]),
    
    Reply = 
	case ets:lookup(Sessions, Id) of
	    [Session] ->
		{ok, Session};
	    [] ->
		{error, not_found}
	end,
    clean_tables(Ctx),

    {reply, Reply, Ctx};

handle_call(dump, _From, Ctx=#ctx {sessions = Sessions, queue = Queue}) ->
    io:format("Ctx: ~p~n", [Ctx]),
    io:format("Queue:~n",[]),
    ets:foldl(fun(Q, _A) -> io:format("~p~n",[Q]) end, [], Queue),
    io:format("Session table:~n",[]),
    ets:foldl(fun(S, _A) -> io:format("~p~n",[S]) end, [], Sessions),
    {reply, ok, Ctx};

handle_call(stop, _From, Ctx) ->
    ?dbg("stop:",[]),
    {stop, normal, ok, Ctx};

handle_call(_Request, _From, Ctx) ->
    ?dbg("handle_call: unknown request ~p", [_Request]),
    {reply, {error,bad_call}, Ctx}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages.
%%
%% @end
%%--------------------------------------------------------------------
-type cast_msg()::
	term().

-spec handle_cast(Msg::cast_msg(), Ctx::#ctx{}) -> 
			 {noreply, Ctx::#ctx{}} |
			 {stop, Reason::term(), Ctx::#ctx{}}.

handle_cast(_Msg, Ctx) ->
    ?dbg("handle_cast: unknown msg ~p", [_Msg]),
    {noreply, Ctx}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages.
%% 
%% @end
%%--------------------------------------------------------------------
-type info()::
	term().

-spec handle_info(Info::info(), Ctx::#ctx{}) -> 
			 {noreply, Ctx::#ctx{}} |
			 {noreply, Ctx::#ctx{}, Timeout::timeout()} |
			 {stop, Reason::term(), Ctx::#ctx{}}.

handle_info(_Info, Ctx) ->
    ?dbg("handle_info: unknown info ~p", [_Info]),
    {noreply, Ctx}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
-spec terminate(Reason::term(), Ctx::#ctx{}) -> 
		       no_return().

terminate(_Reason, _Ctx=#ctx {state = State}) ->
    ?dbg("terminate: terminating in state ~p, reason = ~p",
	 [State, _Reason]),
    ok.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process ctx when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn::term(), Ctx::#ctx{}, Extra::term()) -> 
			 {ok, NewCtx::#ctx{}}.

code_change(_OldVsn, Ctx, _Extra) ->
    ?dbg("code_change: old version ~p", [_OldVsn]),
    {ok, Ctx}.


%%--------------------------------------------------------------------
%%
%% Internal
%% 
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc
%% Removes old sessions
%%
%% @end
%%--------------------------------------------------------------------
-spec clean_tables(Ctx::#ctx{}) -> ok.

clean_tables(_Ctx=#ctx {sessions = Sessions, queue = Queue}) ->
    %% Clean away old sessions
    Now = sec(),
    OldSessions = 
	ets:select(Queue, [{{'$1','$2'},[{'<', '$1', Now}], ['$2']}]),
    ?dbg("clean: old sessions ~p.", [OldSessions]),
    ets:select_delete(Queue, [{{'$1','$2'},[{'<', '$1', Now}], [true]}]),
    lists:foreach(fun(S) -> ets:delete(Sessions, S) end, OldSessions),
    ok.
    
%%--------------------------------------------------------------------
call(Id, F) ->
    try list_to_integer(Id) of
	I -> apply(F, [I])
    catch
	error: _E ->
	    ?dbg("call: ~p not integer.", [Id]),
	    {error, illegal_id}
    end.
