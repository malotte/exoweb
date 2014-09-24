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
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%  ExoDM WebService data interface.
%%%
%%% @end

-module(exoweb_data_if).

-include("exoweb.hrl").

%% Exodm access
-export([login/1,
	 contact/0]).
-export([read/1,
	 create/1,
	 delete/1,
	 update/1,
	 fetch/5]).

%% While testing
-export([opts/1]).

-import(exoweb_lib, [get_env/2]).

%%%===================================================================
%%% Exodm access
%%%===================================================================
login(#exoweb_user{name = Name, password = Pass}) ->
    case accounts(Name, {Name, Pass}) of
	[{A,_R}] ->
	    %% Only one account and role
	    %% wf:session(login_account, A),
	    ok;
	List when is_list(List), List =/= [] ->
	    {A, _R} = choose_account(List, List, hd(List)),
	    %% wf:session(login_account, A),
	    ok;
	[] ->
	    ?dbg("login_user: user account ~p not found.", [Name]),
	    {error, no_account_found};
	{error, "object-not-found"}->
	    ?dbg("login_user: user ~p not found.", [Name]),
	    {error, unknown_user};
	{error, _Reason} = E ->
	    ?dbg("login_user: ~p error ~p.", [Name, _Reason]),
	    E
   end.    

contact() ->
    case result(lookup_user("dummy", {"user","pass"}), result) of 
	{error, no_contact_with_host} -> false;
	{error, "Unauthorized"} -> true;
	_Other -> ?dbg("contact: unexpected ~p.", [_Other]), true %% ??
    end.

%%--------------------------------------------------------------------
-spec create(Record::#exoweb_account{} | 
		     #exoweb_user{} | 
		     #exoweb_device{} |
		     #exoweb_yang{} ) ->
		    ok |
		    {error, Reason::atom()}.

create(#exoweb_account{name = Name, email = Email, password = Pass}) ->
    case result(create_account(Name, Email, Pass), {item, "account-admin"}) of
	Admin when is_list(Admin) ->
	    %% Also device type for future device creation ??
	    case create({device_type, Name, {Admin, Pass}}) of
		ok -> 
		    ok;
		{error, _Reason} = E ->
		    %% Try to rollback
		    case result(delete_account(Name)) of
			ok -> ok;
			ERback -> ?ee("Rollback failed, ~p.",[ERback])
		    end,
		    E
	    end;
	{error, _Reason} = E ->
	    ?dbg("create: acount ~p, error ~p.", [Name, _Reason]),
	    E
    end;
	
create(#exoweb_user{name = Name, attributes = Attrs}) ->
    Account = wf:session(login_account),
    {value, {"role", Role}, OtherAttrs} = lists:keytake("role", 1, Attrs),
    case result(create_user(Name, OtherAttrs)) of
	ok ->
	    case result(add_account_access(Account, Role, Name)) of
		ok -> 
		    ok;
		{error, _Reason} = E ->
		    %% Try to rollback
		    case result(delete_user(Name)) of
			ok -> ok;
			ERback -> ?ee("Rollback failed ~p.",[ERback])
		    end,
		    E
	    end;
	{error, _Reason} = E ->
	    ?dbg("create: user ~p, error ~p.", [Name, _Reason]),
	    E
    end;
create(#exoweb_device{id = Id, attributes = Attrs}) ->
    Account = wf:session(login_account),
    result(create_device(Account, Id, Attrs));
create(#exoweb_yang{id = File}) when is_list(File) ->
    Account = wf:session(login_account),
    result(create_yang_module(Account, File));
create({device_type, Account, Access}) ->
    %% Find init-admin
    result(create_device_type(Account, Account, Access)).
					

%%--------------------------------------------------------------------
-spec delete(Record::#exoweb_device{}) ->
		    ok |
		    {error, Reason::atom()}.

delete(#exoweb_user{name = Name}) ->
    result(delete_user(Name));
delete(#exoweb_yang{id = File}) when is_list(File) ->
    Account = wf:session(login_account),
    result(delete_yang_module(Account, File));
delete(#exoweb_device{id = Id}) ->
    Account = wf:session(login_account),
    result(delete_device(Account, Id)).

%%--------------------------------------------------------------------
-spec update(Record::#exoweb_user{} | 
		     #exoweb_device{}) ->
		    ok |
		    {error, Reason::atom()}.

update(#exoweb_user{attributes = []}) ->
    {error, "no changes"};
update(#exoweb_user{name = Name, attributes = Updates}) ->
    %% Role change can fail but user update can only fail if
    %% user is missing so better start with role change
    case lists:keytake("role", 1, Updates) of
	{value, {'role', Role}, []} ->
	    %% Only role is changed
	    update_role(Name, Role);
	{value, {"role", Role}, Rest} ->
	    case update_role(Name, Role) of
		ok ->
		    result(update_user(Name, Rest));
		Error ->
		    ?dbg("update: user ~p, error ~p.", [Name, Error]),
		    Error
	    end;
	false ->
	    result(update_user(Name, Updates))
    end.

%%--------------------------------------------------------------------
-spec read(Record::record()) ->
		  ok |
		  {error, Reason::atom()}.

read(#exoweb_user{name = Name}) ->
    Account = wf:session(login_account),
    case result(lookup_user(Name),
		      {lookup, "users"}) of
	[] ->
	    ?dbg("read: user ~p not found !!.", [Name]),
	    {error, "Not found"};
	AttributesList when is_list(AttributesList) ->
	    ?dbg("attributes: ~p.", [AttributesList]),
	    Roles = [R || {A, R} <- accounts(Name), A == Account],
	    wf:session(roles, Roles),
	    set_attributes([{"role", exoweb_lib:roles2string(Roles)} | 
			    AttributesList]),
	    ok;
	{error, _Reason} = E ->
	    ?dbg("read: user ~p, error ~p.", [Name, _Reason]),
	    E
    end;
read(#exoweb_device{id = Id}) ->
    Account = wf:session(login_account),
    case result(lookup_device_attributes(Account, Id),
		      {list, "attributes"}) of
	[] ->
	    ?dbg("read: device ~p not found !!.", [Id]),
	    {error, "Not found"};
	AttributesStructList when is_list(AttributesStructList) ->
	    ?dbg("attributes: ~p.", [AttributesStructList]),
	    set_attributes(AttributesStructList),
	    ok;
	{error, _Reason} = E ->
	    ?dbg("read: device ~p, error ~p.", [Id, _Reason]),
	    E
    end.

%%--------------------------------------------------------------------
-spec fetch(Table::atom(),
	    Rows::integer(),
	    Last::string(),
	    Direction::ascending | descending,
	    Session::#exoweb_session{}) ->
		  Result::term() |
		  {error, Reason::atom()}.

fetch(user, Rows, Last, Direction, 
      Session=#exoweb_session {account = Account}) 
  when Direction == ascending; Direction == descending ->
    result(list_account_users(Account, Rows, Last, Direction, Session), 
	   {list, "users"});
fetch(file, Rows, Last, Direction, 
      Session=#exoweb_session {account = Account}) 
  when Direction == ascending; Direction == descending ->
    result(list_yang_modules(Account, Rows, Last, Direction, Session), 
	   {list, "yang-modules"});
fetch(device, Rows, Last, Direction, 
      Session=#exoweb_session {account = Account}) 
  when Direction == ascending; Direction == descending ->
    result(list_devices_attributes(Account, Rows, Last, Direction, Session), 
	   {list, "devices"}).


%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
	    
accounts(Name) ->
    accounts(Name, current_user).
accounts(Name, Access) ->
    case result(list_user_accounts(Name, Access), {list, "accounts"}) of
	AccountStructList when is_list(AccountStructList) ->
	    decode(AccountStructList, []);
	{error, _Reason} = E ->
	    E
    end.
	
%% How to choose account ???
%% 1 - inital-admin, 2 - admin, 3 - first in list
choose_account([], [], First) ->
    First;
choose_account([{A, ?INIT_ADMIN = R} | _Tail], _List, _First) ->
    {A, R};
choose_account([{_A, _R} | Rest], List, First) ->
    choose_account(Rest, List, First);
choose_account([], [{A, ?ADMIN = R} | _Tail], _First) ->
    {A, R};
choose_account([], [{_A, _R} | Rest], First) ->
    choose_account([], Rest, First).


update_role(_Name, no_change) ->
    ok;
update_role(Name, NewRole) ->
    Account = wf:session(login_account),
    OldRoles = [R || {A, R} <- accounts(Name), A == Account],
    case lists:member(NewRole, OldRoles) of
	true -> ok;
	false -> update_if_not_ia(lists:member(?INIT_ADMIN, OldRoles),
				  Account, Name, OldRoles, NewRole)
    end.

update_if_not_ia(true, _Account, _Name, _OldRoles, _NewRole) ->
    {error, "Role initial-admin can not be removed."};
update_if_not_ia(false, Account, Name, OldRoles, NewRole) ->
    update_roles(Account, Name, OldRoles, NewRole).

update_roles(Account, Name, OldRoles, NewRole) ->
    case remove_roles(Account, Name, OldRoles) of
	ok -> 
	    result(add_account_access(Account, NewRole, Name));
	E -> 
	    %% Rollback of remove ??
	    E
    end.

remove_roles(_Account, _Name, []) ->
    ok;
remove_roles(Account, Name, [Role | Rest]) ->
    ?dbg("remove_role:account ~p user ~p, role ~p.", [Account, Name, Role]),
    case result(remove_account_access(Account, Role, Name)) of
	ok -> remove_roles(Account, Name, Rest);
	E -> E
    end.	

%%--------------------------------------------------------------------
%% exodm interface    
%%--------------------------------------------------------------------
lookup_user(Name, Access) ->
    %% At login
    exodm_json_api:lookup_user(Name, opts(Access)).
lookup_user(Name) ->
    exodm_json_api:lookup_user(Name, opts(current_user)).
list_user_accounts(Name, Access) ->
    exodm_json_api:list_user_accounts(Name, opts(Access)).
create_account(Name, Email, Pass) ->
    exodm_json_api:create_account(Name, Email, Pass, Name, opts(root)).
delete_account(Name) ->
    exodm_json_api:delete_account(Name, opts(root)).
create_device_type(Acc, Name, Access) ->
    exodm_json_api:create_device_type(Acc, Name, "exodm", opts(Access)).
create_user(Name, Attrs) ->
    exodm_json_api:create_user(Name, Attrs, opts(current_user)).
delete_user(Name) ->
    exodm_json_api:delete_user(Name, opts(current_user)).
update_user(Name, Attributes) ->
    exodm_json_api:update_user(Name, Attributes, opts(current_user)).
list_account_users(Acc, Rows, Last, Direction, Access) -> 
    exodm_json_api:list_account_users(Acc, Rows, Last, 
				      atom_to_list(Direction), opts(Access)).
add_account_access(Acc, Role, Name) ->
    exodm_json_api:add_account_access(Acc, Role, [Name], 
				      opts(current_user)).
remove_account_access(Acc, Role, Name) ->
    exodm_json_api:remove_account_access(Acc, Role, [Name], 
					 opts(current_user)).
create_yang_module(Acc, File) ->
    exodm_json_api:create_yang_module(Acc, 
				      "user", 
				      filename:basename(File), 
				      File, 
				      opts(current_user)).
delete_yang_module(Acc, File) ->
    exodm_json_api:delete_yang_module(Acc, 
				      "user",
				      filename:basename(File), 
				      opts(current_user)).
list_yang_modules(Account, Rows, Last, Direction, Access) ->
    exodm_json_api:list_yang_modules(Account, "user", Rows, Last, 
				     atom_to_list(Direction), opts(Access)).
%%				     opts(Access)).

create_device(Acc, Id, Attrs) ->
    %% Ugly solution to exodmapi limitations
    %% Device type name same as account name 
    exodm_json_api:create_device(Id, Acc, Attrs ++ [{"account", Acc}], 
	opts(current_user)).
delete_device(Acc, Id) ->
    exodm_json_api:delete_devices(Acc, [Id], opts(current_user)).

list_devices_attributes(Acc, Rows, Last, Direction, Access) -> 
    Attrs = [atom_to_list(Name) || { _ColName, Name, _} <- 
				       ?DEVICE_MODEL, Name =/= 'device-id'],
    ?dbg("list_devices_attributes: ~p", [Attrs]),
    exodm_json_api:list_devices_attributes(Acc, Rows, Last, Attrs, "*", 
					   atom_to_list(Direction), 
					   opts(Access)).
lookup_device_attributes(Account, Id) ->
    %% Ugly solution to exodmapi limitations
    exodm_json_api:lookup_device_attributes(Account, Id,
	exoweb_device:attributes2fetch(),
	opts(current_user)).

result(ResultStruct) ->
    result(ResultStruct, ok).

result({error, econnrefused} = E, E) ->
    ?dbg("result: result ~p", [E]),
    ok;
result({error, econnrefused} = E, _Wanted) ->
    ?dbg("result: result ~p", [E]),
    {error, no_contact_with_host};
result(ResultStruct, Wanted) ->
    try 
	exodm_json_api:parse_result(ResultStruct, Wanted)
    catch
	error:_E ->
	    ?dbg("result: wanted ~p, result ~p, error ~p,", 
		 [Wanted, ResultStruct, _E]),
	    case exodm_json_api:parse_result(ResultStruct, result) of
		"ok" -> ok;
		Error -> Error
	    end
    end.
	
%%--------------------------------------------------------------------
%% exodm to exoweb conversion
%%--------------------------------------------------------------------
set_attributes([]) ->
    ok;
set_attributes([{struct, [{"name", Name}, {"val", Value}]} | Rest ]) 
  when is_integer(Value) ->
    %% Device keys
    wf:session(list_to_atom(Name),  integer_to_list(Value)),
    ?dbg("set_attributes: device keys ~p = ~p",[Name, Value]),
    set_attributes(Rest);
set_attributes([{struct, [{"name", Name}, {"val", Value}]} | Rest ]) ->
    %% Device
    ?dbg("set_attributes: device ~p = ~p",[Name, Value]),
    wf:session(list_to_atom(Name),  Value),
    set_attributes(Rest);
set_attributes([{Name, {array, Value}} | Rest ]) ->
    %% User alias
    ?dbg("set_attributes: user alias ~p = ~p",[Name, Value]),
    wf:session(list_to_atom(Name),  Value),
    set_attributes(Rest);
set_attributes([{Name, Value} | Rest ]) 
  when is_atom(Name)->
    %% User
    ?dbg("set_attributes: user ~p = ~p",[Name, Value]),
    wf:session(Name,  Value),
    set_attributes(Rest);
set_attributes([{Name, Value} | Rest ]) 
  when is_list(Name)->
    set_attributes([{list_to_atom(Name), Value} | Rest ]).

decode([], Acc) ->
    merge(Acc);
decode([{struct, [{"name", Name}, {"roles", {array, Roles}}]} | Rest ], Acc) ->
    ?dbg("decode: ~p, ~p.", [Name, Roles]),
    decode(Rest, [{Name, Roles} | Acc]).

merge(List) ->
    lists:usort(lists:append(lists:map(fun({AorU, Roles}) ->
					       [{AorU, R} || R <- Roles]
				       end, List))).


%%--------------------------------------------------------------------
%% exodm access
%%--------------------------------------------------------------------

opts(root) ->
    [{url, get_env(exodm_url, "")},
     {user, get_env(exodm_user, "")},
     {password, get_env(exodm_password, "")}];
opts(exoweb) -> %% Should be replaced with a user with only view access
    [{url, get_env(exodm_url, "")},
     {user, get_env(exodm_user, "")},
     {password, get_env(exodm_password, "")}];
opts(current_user) ->
    [{url, get_env(exodm_url, "")},
     {user,  wf:user()},
     {password, wf:session(login_password)}];
opts({User, Pass}) ->
    [{url, get_env(exodm_url, "")},
     {user,  User},
     {password, Pass}];
opts(#exoweb_session{id = SessionId, account = Account, user = User}) ->
    case exoweb_session_server:read(SessionId) of
	{ok, _Session=#exoweb_session {account = Account, 
				       user = User, 
				       password = Pass}} ->
	    ?dbg("opts: session ~p found.", [_Session]),
	    [{url, get_env(exodm_url, "")},
	     {user,  User},
	     {password, Pass}];
	{ok, OtherSession} ->
	    ?ee("opts: session mismatch ~p.", [OtherSession]),
	    [];
	{error, Reason} ->
	    ?ee("opts: session error ~p.", [Reason]),
	    []
    end.
