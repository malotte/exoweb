%%% -*- coding: latin-1 -*-
%%%---- BEGIN COPYRIGHT --------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2015, Rogvall Invest AB, <tony@rogvall.se>
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
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%  ExoDM WebService yang page.
%%%
%%% @end
-module (exoweb_yang).

-include("exoweb.hrl").

%% Callbacks for js
-export([event/1]).

%%--------------------------------------------------------------------
%% @doc 
%% Callback from js when an event has occured
%% @end
%%--------------------------------------------------------------------
-spec event({Tag::atom(), list(tuple())}) -> 
   ok | {error, Error::term()}.

event({load, Args} = Event) ->
    ?dbg("event: ~p",[Event]),
    exoweb_lib:load(yang, Args);
event({create, Args} = Event) ->
    ?dbg("event: ~p",[Event]),
    call(create, Args); 
event({delete, Args} = Event) ->
    ?dbg("event: ~p",[Event]),
    call(delete, Args); 
event({select, Args} = Event) ->
    ?dbg("event: ~p",[Event]),
    call(select, Args); 
event(Event) ->
    ?dbg("event: unknown event ~p",[Event]),
    ok.


    
%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------

call(Action, Args) ->
    Access = {proplists:get_value(user, Args),
	      proplists:get_value(password, Args)},
    Account = proplists:get_value(account, Args),
    Id = proplists:get_value(filename, Args),
    exoweb_data_if:Action({yang, Account, Id, Access}).
