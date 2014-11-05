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

-import(exoweb_lib, [direction/3, fix_list/4]).

%%--------------------------------------------------------------------
%% @doc 
%% Callback from js when an event has occured
%% @end
%%--------------------------------------------------------------------
-spec event({Tag::atom(), list(tuple())}) -> 
   ok | {error, Error::term()}.

event({load, Args} = Event) ->
    ?dbg("event: ~p",[Event]),
    Access = {proplists:get_value(account, Args),
	      proplists:get_value(user, Args),
	      proplists:get_value(password, Args)},
    Rows = to_int(proplists:get_value(rows, Args, 10)), %% Mix of int and list
    ReqPage = proplists:get_value(page, Args, 1),
    LastPage = proplists:get_value(lastpage, Args, 0),
    LastId = proplists:get_value(lastid, Args, ""),
    ?dbg("fetch: lastid ~p, reqpage ~p, lastpage ~p, rows ~p.", 
	 [LastId, ReqPage, LastPage, Rows]),
    {ReqLastId, Direction} = direction(LastId, ReqPage, LastPage),
    ?dbg("fetch: reqlastid ~p, dir ~p.", [ReqLastId, Direction]),
    %% Fetch one extra to check if we reached end of data
    case exoweb_data_if:fetch({yang, Rows + 1, ReqLastId, Direction, Access}) of
	List when is_list(List) ->
	    ?dbg("fetch: ~p, list ~p.", [yang, List]),
	    {ok, fix_list(Rows, ReqPage, Direction, List)};
	{error, E} ->
	    ?ee("fetch: error ~p,", [E]),
	    []
    end;
event({select, Args} = Event) ->
    ?dbg("event: ~p",[Event]),
    Access = {proplists:get_value(user, Args),
	      proplists:get_value(password, Args)},
    Account = proplists:get_value(account, Args),
    Id = proplists:get_value(filename, Args),
    exoweb_data_if:read({yang, Account, Id, Access});
event({update, Args} = Event) ->
    ?dbg("event: ~p",[Event]),
    Access = {proplists:get_value(user, Args),
	      proplists:get_value(password, Args)},
    Account = proplists:get_value(account, Args),
    {value, {filename, Id}, _Rest} = lists:keytake(filename, 1, Args),
    case proplists:get_value(delete, Args, "false") of
	false -> 
	    ok;
	true -> 
	    exoweb_data_if:delete({yang, Account, Id, Access})
    end;
event({create, Args} = Event) ->
    ?dbg("event: ~p",[Event]),
    Access = {proplists:get_value(user, Args),
	      proplists:get_value(password, Args)},
    Account = proplists:get_value(account, Args),
    {value, {filename, Id}, _Rest} = lists:keytake(filename, 1, Args),
    exoweb_data_if:create({yang, Account, Id, Access});
event(Event) ->
    ?dbg("event: unknown event ~p",[Event]),
    ok.


%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------

attrs([], Attrs) ->
    ?dbg("attrs: ~p.", [Attrs]),
    Attrs;
attrs([{Name, Value} | Args], Attrs) ->
    case lists:member(Name, ?USER_ATTRS) of
	true -> attrs(Args,  [{atom_to_list(Name), Value} | Attrs]);
	false -> attrs(Args, Attrs)
    end.

to_int(I) when is_integer(I) -> I;
to_int(List) when is_list(List) -> ?l2i(List). 
    
