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
%%%  ExoDM WebService device page.
%%%
%%%
%%% @end
-module (exoweb_device).

-include("exoweb.hrl").

%% Callbacks for js
-export([event/1]).

%% Callback from exoweb_data_if
-export([attributes2fetch/0]).

%%--------------------------------------------------------------------
%% @doc 
%% Callback from js when an event has occured
%% @end
%%--------------------------------------------------------------------
-spec event({Action::atom(), Args::list(tuple())}) -> 
   ok | {error, Error::term()}.

event({load, Args} = Event) ->
    ?dbg("event: ~p",[Event]),
    exoweb_lib:load(device, Args);
event({create, Args} = Event) ->
    ?dbg("event: ~p",[Event]),
    call(create, Args); 
event({delete, Args} = Event) ->
    ?dbg("event: ~p",[Event]),
    call(delete, Args); 
event({select, Args} = Event) ->
    ?dbg("event: ~p",[Event]),
    call(select, Args); 
event({update, Args} = Event) ->
    ?dbg("event: ~p",[Event]),
    %% Update not implemented in exodm yet
    call(create, Args); 
event({lock, Args} = Event) ->
    ?dbg("event: ~p",[Event]),
    %% Lock not implemented in exodm yet
    %% call(lock, Args); 
    ok;
event({unlock, Args} = Event) ->
    ?dbg("event: ~p",[Event]),
    %% Unlock not implemented in exodm yet
    %% call(unlock, Args); 
    ok;
event(Event) ->
    ?dbg("event: unknown event ~p",[Event]),
    ok.


%%--------------------------------------------------------------------
%% @doc 
%% Specification of attributes to fetch from exodm, needed by
%% exoweb_data_if module.
%% @end
%%--------------------------------------------------------------------
-spec attributes2fetch() -> list().

attributes2fetch() ->
    [atom_to_list(A) || A <- ?DEVICE_ATTRS,A =/= 'device-id'].


%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
-spec call(Action::atom(), Args::list(tuple())) -> 
   ok | {error, Error::term()}.

call(Action, Args) ->
    Access = {proplists:get_value(user, Args),
	      proplists:get_value(password, Args)},
    Account = proplists:get_value(account, Args),
    {value, {'device-id', Id}, Rest} = lists:keytake('device-id', 1, Args),
    case Action of
	ActionWithAttrs when ActionWithAttrs == create;
			     ActionWithAttrs == update ->
	    %% Check msisdn format
	    case exoweb_lib:is_phone_no(proplists:get_value(msisdn, Args, "")) of
		true ->
		    exoweb_data_if:Action({device, Account, Id, 
					   attrs(Rest, []), Access});
		false ->
		    {error, illegal_msisdn}
	    end;
	_OtherActions ->
	    exoweb_data_if:Action({device, Account, Id, Access})
    end.
	
attrs([], Attrs) ->
    ?dbg("attrs: ~p.", [Attrs]),
    Attrs;
attrs([{Name, Value} | Args], Attrs) ->
    case lists:member(Name, ?DEVICE_ATTRS) of
	true -> attrs(Args,  [{atom_to_list(Name), Value} | Attrs]);
	false -> attrs(Args, Attrs)
    end.

