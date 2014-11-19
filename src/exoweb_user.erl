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
%%%  ExoDM WebService user page.
%%%
%%% @end
-module (exoweb_user).

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
    exoweb_lib:load(user, Args);
event({create, Args} = Event) ->
    ?dbg("event: ~p",[Event]),
    call(create, Args); 
event({delete, Args} = Event) ->
    ?dbg("event: ~p",[Event]),
    call(delete, Args); 
event({select, Args} = Event) ->
    ?dbg("event: ~p",[Event]),
    call(read, Args); 
event({update, Args} = Event) ->
    ?dbg("event: ~p",[Event]),
    call(update, Args); 
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
    {value, {name, Id}, Rest} = lists:keytake(name, 1, Args),
    case Action of
	ActionWithAttrs when ActionWithAttrs == create;
			     ActionWithAttrs == update ->
	    exoweb_data_if:Action({user, Account, Id, attrs(Rest, []), Access});
	_OtherActions ->
	    exoweb_data_if:Action({user, Account, Id, Access})
    end.

attrs([], Attrs) ->
    ?dbg("attrs: ~p.", [Attrs]),
    Attrs;
attrs([{Name, Value} | Args], Attrs) ->
    case lists:member(Name, ?USER_ATTRS) of
	true -> attrs(Args,  [{atom_to_list(Name), Value} | Attrs]);
	false -> attrs(Args, Attrs)
    end.
    
