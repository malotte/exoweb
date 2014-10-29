%%% -*- coding: latin-1 -*-
%%% -*- mode: nitrogen -*-
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
%%% @author Malotte W L�nne <malotte@malotte.net>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%  ExoDM WebService device page.
%%%
%%%  Based on nitrogen, see documentation on http://nitrogenproject.com/
%%%
%%% @end
-module (exoweb_device).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nitrogen_elements/include/nitrogen_elements.hrl").
-include("exoweb.hrl").

%% Callbacks for js
-export([event/1]).

%% Callback rom exoweb_data_if
-export([attributes2fetch/0]).


-import(exoweb_lib, [dialog/1, postback/1, error_txt/1]).

	
%%--------------------------------------------------------------------
%% @doc 
%% Callback from js when an event has occured
%% @end
%%--------------------------------------------------------------------
-spec event({Tag::atom(), list(tuple())}) -> 
   ok | {error, Error::term()}.

event({load, Args} = Event) ->
    ?dbg("event: ~p",[Event]),
    Account = proplists:get_value(account, Args),
    User = proplists:get_value(user, Args),
    Pass = proplists:get_value(password, Args),
    Rows = to_int(proplists:get_value(rows, Args, 10)), %% Mix of int and list
    ReqPage = proplists:get_value(page, Args, 1),
    LastPage = proplists:get_value(lastpage, Args, 0),
    LastId = proplists:get_value(lastid, Args, ""),
    ?dbg("fetch: lastid ~p, reqpage ~p, lastpage ~p, rows ~p.", 
	 [LastId, ReqPage, LastPage, Rows]),
    {ReqLastId, Direction} = direction(LastId, ReqPage, LastPage),
    ?dbg("fetch: reqlastid ~p, dir ~p.", [ReqLastId, Direction]),
    %% Fetch one extra to check if we reached end of data
    case exoweb_data_if:fetch(device, Rows + 1, ReqLastId, Direction, {Account, User, Pass}) of
	List when is_list(List) ->
	    ?dbg("fetch: ~p, list ~p.", [device, List]),
	    {ok, fix_list(Rows, ReqPage, Direction, List)};
	{error, E} ->
	    ?ee("fetch: error ~p,", [E]),
	    []
    end;
event({select, Args} = Event) ->
    ?dbg("event: ~p",[Event]),
    Account = proplists:get_value(account, Args),
    User = proplists:get_value(user, Args),
    Pass = proplists:get_value(password, Args),
    Id = proplists:get_value('device-id', Args),
    exoweb_data_if:read(device, Account, Id, {User, Pass});
event({update, Args} = Event) ->
    ?dbg("event: ~p",[Event]),
    Account = proplists:get_value(account, Args),
    User = proplists:get_value(user, Args),
    Pass = proplists:get_value(password, Args),
    Id = proplists:get_value('device-id', Args),
    case proplists:get_value(delete, Args, "false") of
	false -> 
	    exoweb_data_if:create(device, Account, Id, attrs(Args, []), {User, Pass});
	true -> 
	    exoweb_data_if:delete(device, Account, Id, {User, Pass})
    end;
event({create, Args} = Event) ->
    ?dbg("event: ~p",[Event]),
    Account = proplists:get_value(account, Args),
    User = proplists:get_value(user, Args),
    Pass = proplists:get_value(password, Args),
    Id = proplists:get_value('device-id', Args),
    exoweb_data_if:create(device, Account, Id, attrs(Args, []), {User, Pass});
event(Event) ->
    ?dbg("event: unknown event ~p",[Event]),
    ok.


%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------

attributes2fetch() ->
    [atom_to_list(A) || A <- ?DEVICE_ATTRS,A =/= 'device-id'].

direction(LastId, ReqPage, LastPage) 
    when ReqPage == LastPage + 1 ->
    %% Next page
    {LastId, ascending};
direction(_LastId, ReqPage, LastPage) 
  when ReqPage > LastPage + 1 ->
    %% Last page, restart from end
    {"", descending};
direction(_LastId, 1, _LastPage) ->
    %% First page, restart from beginning
    {"", ascending};
direction(LastId, ReqPage, LastPage) 
  when ReqPage == LastPage - 1 ->
    %% Previous page
    {LastId, descending};
direction(LastId, ReqPage, LastPage) ->
    ?dbg("direction: ~p, ~p, ~p.", [LastId, ReqPage, LastPage]),
    %% Should not happen !!
    %% Only  here for trace output.
    throw(illegal_direction).

fix_list(_Rows, _ReqPage, _Direction, []) ->
    [];
fix_list(Rows, _ReqPage, Direction, List) ->
    ?dbg("fix_list: list ~p.", [List]),
    NoOfRecs = length(List),
    if NoOfRecs == Rows + 1  ->
	    %% More data available
	    %% Throw away extra record at end
	    lists:droplast(List);
       NoOfRecs < Rows + 1 andalso Direction  == ascending ->
	    %% Last data
	    List;
       NoOfRecs < Rows + 1 andalso Direction  == descending ->
	    %% First data 
	    List;
      true ->
	    ?dbg("fix_list: ~p, ~p.", [NoOfRecs, Rows]),
	    %% Should not happen !!
	    %% Only  here for trace output.
	    throw(illegal_no_of_recs)
    end.

attrs([], Attrs) ->
    ?dbg("attrs: ~p.", [Attrs]),
    Attrs;
attrs([{Name, Value} | Args], Attrs) ->
    case lists:member(Name, ?DEVICE_ATTRS) of
	true -> attrs(Args,  [{atom_to_list(Name), Value} | Attrs]);
	false -> attrs(Args, Attrs)
    end.

to_int(I) when is_integer(I) -> I;
to_int(List) when is_list(List) -> ?l2i(List). 
    
