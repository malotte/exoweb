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
	 delete_cookie/2,
	 wrapper/4]).
	 

-export([init_postdata/0,
	 fetch/2,
	 userdata2postdata/2]).

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
		  #exoweb_cookie{user = User, 
				 account = Account,
				 password = Pass}),
	    ?dbg("create_cookie: cookie ~p.", [WebCookie]),
	    %% Set cookie at client
	    ok = wse:set(WebSocket, wse:document(), "cookie", 
			 "id=" ++ integer_to_list(WebCookie));
	                  %% "id=" ++ integer_to_list(WebCookie) ++ "; path=/");
	                  %% If adding path, add in logout (controllers.js) 
	                  %% as well
	{error, illegal_user} = E ->
	    ?dbg("create_cookie: illegal user ~p.", [User]),
	    E
    end.

user2account(User) ->
    case string:tokens(User, "/") of
	[Account, _Name] -> Account;
	_O -> {error, illegal_user}
    end.
	     
%% Called at logout 
-spec delete_cookie(WebSocket::term(), WebCookie::string()) -> ok.
delete_cookie(WebSocket, WebCookie) ->
    ?dbg("delete_cookie: ~p.",[WebCookie]),
    case exoweb_cookie_handler:retreive(WebCookie) of
	{ok, _Cookie=#exoweb_cookie{}} ->
	    %% Delete cookie at client
	    ok = wse:set(WebSocket, wse:document(), "cookie", 
			 "id=; path=/");
	{error, Reason} ->
	    ?dbg("delete_cookie: cookie error ~p.", [Reason]),
	    {error, unknown_cookie}
    end.
  
%% Called when accessing exodm
wrapper(WebSocket, M, F, A) ->
    ?dbg("wrapper: ~p, ~p, ~p.",[M, F, A]),
    %% Check that cookie has not expired
    case wse:get(WebSocket, wse:document(), 'cookie') of
	{ok, Cookie} ->
	    verify_and_call(WebSocket, Cookie, M, F, A);
	_E ->
	    ?dbg("wrapper: no cookie ~p.",[_E]),
	    go_to_login(WebSocket)
    end.

verify_and_call(WebSocket, [], _M, _F, _A) ->
    ?dbg("wrapper: no id cookie.",[]),
    go_to_login(WebSocket);
verify_and_call(WebSocket, [CookieItem | List], M, F, A) ->
    case string:tokens(CookieItem, "=") of
	["id", CookieValue] ->
	    verify_and_call1(WebSocket, CookieValue, M, F, A);
	_Other ->
	    verify_and_call(WebSocket, List, M, F, A)
    end.

verify_and_call1(WebSocket, CookieValue, M, F, [{Event, Args}]) ->
    case exoweb_cookie_handler:read(CookieValue) of
	{ok, _Cookie=#exoweb_cookie{ user = User, 
				     account = Account,
				     password = PassWord}} ->
	    apply(M, F, [Event, [{user, User}, 
				 {account, Account},
				 {password, PassWord} |Args]]);
	{error, Reason} ->
	    ?dbg("wrapper: wrong cookie ~p.",[Reason]),
	    go_to_login(WebSocket)
    end.

go_to_login(WebSocket)	-> 
    wse:set(WebSocket, wse:window(), "location", "/login.html"),
    ok.

%%%===================================================================
%%% Javascript mumbo jumbo handling
%%%===================================================================
-spec init_postdata() -> PostData::tuple().

init_postdata() ->
    Account = wf:session(login_account),
    User = wf:user(),
    Pass = wf:session(login_password),
    Session = exoweb_session_server:store(
	#exoweb_session{account = Account, user = User, password = Pass}),

    {{<<"account">>, list_to_binary(Account)}, 
     {<<"user">>, list_to_binary(User)},
     {<<"session">>, list_to_binary(integer_to_list(Session))},
     {<<"lastid">>, <<"">>},
     {<<"lastpage">>, <<"0">>}}.


%%--------------------------------------------------------------------
-spec fetch(Table::atom() | list(),
	      Options::list(tuple())) ->
		  list() |
		  {error, Reason::atom()}.

fetch(Table, Opts) when is_list(Table) ->
    fetch(list_to_atom(Table), Opts);
fetch(Table, Opts) ->
    ?dbg("fetch: ~p, options ~p.", [Table, Opts]),
    SessionId = list_to_integer(proplists:get_value("session", Opts)),
    Account = proplists:get_value("account", Opts),
    User = proplists:get_value("user", Opts),
    Session = #exoweb_session {id = SessionId,
			       account = Account,
			       user = User},
    Rows = list_to_integer(proplists:get_value("rows", Opts, "10")),
    ReqPage = list_to_integer(proplists:get_value("page", Opts)),
    LastPage = list_to_integer(proplists:get_value("lastpage", Opts, "0")),
    LastId = proplists:get_value("lastid", Opts, ""),
    ?dbg("fetch: lastid ~p, reqpage ~p, lastpage ~p.", 
	 [LastId, ReqPage, LastPage]),
    {ReqLastId, Direction} = direction(LastId, ReqPage, LastPage),
    ?dbg("fetch: reqlastid ~p, dir ~p.", [ReqLastId, Direction]),
    PostData = [{<<"account">>, list_to_binary(Account)}, 
		{<<"user">>, list_to_binary(User)},
		{<<"session">>, list_to_binary(integer_to_list(SessionId))}],

    %% Fetch one extra to check if we reached end of data
    case exoweb_data_if:fetch(Table, Rows + 1, ReqLastId, Direction, Session) of
	List when is_list(List) ->
	    ?dbg("fetch: ~p, list ~p.", [Table, List]),
	    to_jqgrid(Table, PostData, Opts, Direction, List);
	{error, E} ->
	    ?ee("fetch: error ~p,", [E]),
	    empty_jqgrid()
    end.


%%--------------------------------------------------------------------
-spec userdata2postdata(Id::atom(),  
			{JsonData::string()}) ->
			   ok |
			   {error, Reason::atom()}.

userdata2postdata(Id, {JsonData}) 
  when is_list(JsonData) ->
    {ok, {struct, KeyValueList}} = exo_json:decode_string(JsonData),
    Page = integer_to_list(proplists:get_value("page", KeyValueList, 0)),
    case proplists:get_value("userdata", KeyValueList) of
	{struct, UserData} when is_list(UserData) ->
	    PostData = 
		[{list_to_binary(Name), list_to_binary(Value)} ||
		    {Name, Value} <- UserData ] ++ 
		[{<<"lastpage">>, list_to_binary(Page)}],
	    JsPostData = common:options_to_js(PostData),
	    ?dbg("userdata: js postdata ~p",[JsPostData]),
	    %% Update jqgrid with new postData
	    wf:wire(Id, postdata(Id, JsPostData));
	undefined ->
	    ?dbg("postdata: no userdata found !!",[]),
	    %% Don't change postdata
	    ok
    end.
	    

postdata(Id, JsPostData) ->
    wf:f("$(function(){$(obj('~s')).setGridParam({postData: ~s});})", 
	 [Id, JsPostData]).	  
  
%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% exodm to jqgrid conversion
%%--------------------------------------------------------------------
   
to_jqgrid(_Table, _PostData, _Opts, _Direction, []) ->
    ?dbg("to_jqgrid: Table ~p, list empty.", [_Table]),
    empty_jqgrid();
to_jqgrid(Table, PostData, Opts, Direction, List) ->
    ?dbg("to_jqgrid: Table ~p, list ~p.", [Table, List]),
    NoOfRecs = length(List),
    Rows = list_to_integer(proplists:get_value("rows", Opts)),
    Page = list_to_integer(proplists:get_value("page", Opts)),
    if NoOfRecs == Rows + 1  ->
	    %% More data available
	    %% Throw away extra record at end
	    %% (List is reversed by jq_format)
	    [_Xtra | JqList] = jq_format(Table, List, []),
	    jqgrid_data(Page + 2, Page, Rows, PostData, 
		      id(Table, hd(JqList)), JqList);
       NoOfRecs < Rows + 1 andalso Direction  == ascending ->
	    %% Last data
	    %% (List is reversed by jq_format)
	    JqList = jq_format(Table, List, []),
	    jqgrid_data(Page, Page, NoOfRecs, PostData, 
			id(Table, hd(List)), JqList);
       NoOfRecs < Rows + 1 andalso Direction  == descending ->
	    %% First data 
	    %% (List is reversed by jq_format)
	    JqList = jq_format(Table, List, []),
	    jqgrid_data(Page + 2, Page, NoOfRecs, PostData, 
			id(Table, hd(JqList)), JqList);
      true ->
	    ?dbg("to_jqgrid: ~p, ~p.", [NoOfRecs, Rows]),
	    %% Should not happen !!
	    %% Only  here for trace output.
	    throw(illegal_no_of_recs)
    end.

jq_format(_Table, [], Acc) ->
    Acc;
jq_format(user = Table, [{struct, KeyValueList} = Rec | Rest], Acc) ->
    ?dbg("jq_format: user ~p.", [Rec]),
    Name = proplists:get_value("name", KeyValueList),
    JqUser = 
	{struct, 
	 [{<<"id">>, Name},
	  {<<"cell">>, 
	   [list_to_binary(Name), list_to_binary(roles(KeyValueList))]}]},
    jq_format(Table, Rest, [JqUser | Acc]);
jq_format(file = Table, [FileName | Rest], Acc) ->
    ?dbg("jq_format: file ~p.", [FileName]),
    JqFile = 
	{struct, 
	 [{<<"id">>, FileName},
	  {<<"cell">>, 
	   [list_to_binary(FileName)]}]},
    jq_format(Table, Rest, [JqFile | Acc]);
jq_format(device = Table, 
	  [{struct, [{"device-id", Id},
		     {"attributes", 
		      {array, AttList}}]} = Rec | Rest], Acc) ->
    ?dbg("jq_format: device ~p.", [Rec]),
    JqDevice = 
	{struct, 
	 [{<<"id">>, Id},
	  {<<"cell">>, 
	   [list_to_binary(Id) | 
	    columns(?DEVICE_MODEL, compact(AttList))]}]},
    jq_format(Table, Rest, [JqDevice | Acc]).
   
id(_Table, {struct, [{<<"id">>, Id}, _Row]}) -> Id; %% json format
id(user, {struct, KeyValueList}) -> proplists:get_value("name", KeyValueList);
id(file, Id) -> Id;
id(device, {struct, [{"device-id", Id}, _Attrs]}) -> Id.


columns([_IndexCol| Model], AttList) ->
    lists:reverse(
      lists:foldl(
      fun({_ColName, Name, _}, Acc) ->
	      [list_to_binary(
		 proplists:get_value(atom_to_list(Name), AttList, 
				     ?UTXT("unknown"))) %% If not found
	       | Acc]
      end, [], Model)).

compact(AttList) ->
    [{Name, Value} ||{struct, [{"name", Name}, {"val", Value}]} <- AttList].


roles(KeyValueList) ->
    case proplists:get_value("roles", KeyValueList) of
	{array, List} -> exoweb_lib:roles2string(List);
	_Other ->""
    end.

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

jqgrid_data(Pages, Page, Recs, PostData, LastId, JqList) ->  
    ?dbg("jqgrid_data: jq devices ~p.", [JqList]),
    %% List was reversed by jq_format, now reversed again
    Rows = lists:reverse(JqList),
    {struct, [{<<"total">>, Pages},
	      {<<"page">>, Page},
	      {<<"records">>,  Recs},
	      {<<"userdata">>, {struct, 
				PostData ++
				[{<<"lastid">>, list_to_binary(LastId)}]}},
	      {<<"rows">>, Rows}]}.

empty_jqgrid() ->
    {struct, [{<<"total">>, 0},
	      {<<"page">>, 0},
	      {<<"records">>, 0 },
	      {<<"rows">>, []}]}.

