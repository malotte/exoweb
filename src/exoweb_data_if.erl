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
-export([login/2,
	 contact/0]).
-export([select/1,
	 create/1,
	 delete/1,
	 update/1,
	 fetch/1]).

%% While testing
-export([opts/1]).

-import(exoweb_lib, [get_env/2]).

%%%===================================================================
%%% Exodm access
%%%===================================================================
login(Name, Pass) ->
    ?dbg("login: user ~p, pass ~p.", [Name, Pass]),
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
-spec create(tuple()) ->
		    ok |
		    {error, Reason::atom()}.


create({account, Name, Email, Pass}) ->
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
create({device_type, Account, Access}) ->
    %% Find init-admin
    result(create_device_type(Account, Account, Access));
create({user, Account, Name, Attrs, Access}) ->
    {value, {"role", Role}, OtherAttrs} = lists:keytake("role", 1, Attrs),
    case result(create_user(Name, OtherAttrs, Access)) of
	ok ->
	    case result(add_account_access(Account, Role, Name, Access)) of
		ok -> 
		    ok;
		{error, _Reason} = E ->
		    %% Try to rollback
		    case result(delete_user(Name, Access)) of
			ok -> ok;
			ERback -> ?ee("Rollback failed ~p.",[ERback])
		    end,
		    E
	    end;
	{error, _Reason} = E ->
	    ?dbg("create: user ~p, error ~p.", [Name, _Reason]),
	    E
    end;
create({device, Account, Id, Attrs, Access}) ->
    result(create_device(Account, Id, Attrs, Access));
create({yang, Account, File, Access}) when is_list(File) ->
    ?dbg("create: yang ~p,  ~p.", [Account, File]),
    result(create_yang_module(Account, File, Access)).

%%--------------------------------------------------------------------
-spec delete(Record::tuple()) ->
		    ok |
		    {error, Reason::atom()}.

delete({device, Account, Id, Access}) ->
    result(delete_device(Account, Id, Access));
delete({user, _Account, Name, Access}) ->
    result(delete_user(Name, Access));
delete({yang, Account, File, Access}) when is_list(File) ->
    result(delete_yang_module(Account, File, Access)).
%%--------------------------------------------------------------------
-spec update(tuple()) ->
		    ok |
		    {error, Reason::atom()}.

update({user, _Account, _Name, [], _Access}) ->
    {error, "no changes"};
update({user, Account, Name, Updates, Access}) ->
    %% Role change can fail but user update can only fail if
    %% user is missing so better start with role change
    case lists:keytake("role", 1, Updates) of
	{value, {'role', Role}, []} ->
	    %% Only role is changed
	    update_role(Account, Name, Role, Access);
	{value, {"role", Role}, Rest} ->
	    case update_role(Account, Name, Role, Access) of
		ok ->
		    result(update_user(Name, Rest, Access));
		Error ->
		    ?dbg("update: user ~p, error ~p.", [Name, Error]),
		    Error
	    end;
	false ->
	    result(update_user(Name, Updates, Access))
    end.


%%--------------------------------------------------------------------
-spec select(Record::record()) ->
		  ok |
		  {error, Reason::atom()}.

select({user, Account, Name, Access}) ->
    case result(lookup_user(Name, Access),
		      {lookup, "users"}) of
	[] ->
	    ?dbg("read: user ~p not found !!.", [Name]),
	    {error, "Not found"};
	AttributesList when is_list(AttributesList) ->
	    ?dbg("attributes: ~p.", [AttributesList]),
	    Roles = [R || {A, R} <- accounts(Name, Access), A == Account],
	    ?dbg("roles: ~p.", [Roles]),
	    {ok, [{"role", exoweb_lib:roles2string(Roles)} | AttributesList]};
	{error, _Reason} = E ->
	    ?dbg("read: user ~p, error ~p.", [Name, _Reason]),
	    E
    end;

select({device, Account, Id, Access}) ->
    case result(lookup_device_attributes(Account, Id, Access),
		      {list, "attributes"}) of
	[] ->
	    ?dbg("select: device ~p not found !!.", [Id]),
	    {error, "Not found"};
	AttributesStructList when is_list(AttributesStructList) ->
	    ?dbg("select: ~p.", [AttributesStructList]),
	    {ok, AttributesStructList};
	{error, _Reason} = E ->
	    ?dbg("select: device ~p, error ~p.", [Id, _Reason]),
	    E
    end.
    
%%--------------------------------------------------------------------
-spec fetch(tuple()) ->
		  Result::term() |
		  {error, Reason::atom()}.

fetch({user, Rows, Last, Direction, {Account, User, Pass}}) 
  when Direction == ascending; Direction == descending ->
    result(list_account_users(Account, Rows, Last, Direction, {User, Pass}), 
	   {list, "users"});
fetch({device, Rows, Last, Direction, {Account, User, Pass}}) 
  when Direction == ascending; Direction == descending ->
    result(list_devices_attributes(Account, Rows, Last, Direction, {User, Pass}), 
	   {list, "devices"});
fetch({yang, Rows, Last, Direction, {Account, User, Pass}}) 
  when Direction == ascending; Direction == descending ->
    result(list_yang_modules(Account, Rows, Last, Direction, {User, Pass}), 
	   {list, "yang-modules"}).



%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
	    
accounts(Name, Access) ->
    case result(list_user_accounts(Name, Access), {list, "accounts"}) of
	AccountStructList when is_list(AccountStructList) ->
	    ?dbg("accounts: ~p.", [AccountStructList]),
	    decode(AccountStructList, []);
	{error, _Reason} = E ->
	    ?dbg("accounts: error ~p.", [_Reason]),
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


update_role(_Account, _Name, no_change, _Access) ->
    ok;
update_role(Account, Name, NewRole, Access) ->
    OldRoles = [R || {A, R} <- accounts(Name, Access), A == Account],
    case lists:member(NewRole, OldRoles) of
	true -> ok;
	false -> update_if_not_ia(lists:member(?INIT_ADMIN, OldRoles),
				  Account, Name, OldRoles, NewRole, Access)
    end.

update_if_not_ia(true, _Account, _Name, _OldRoles, _NewRole, _Access) ->
    {error, "Role initial-admin can not be removed."};
update_if_not_ia(false, Account, Name, OldRoles, NewRole, Access) ->
    update_roles(Account, Name, OldRoles, NewRole, Access).

update_roles(Account, Name, OldRoles, NewRole, Access) ->
    case remove_roles(Account, Name, OldRoles, Access) of
	ok -> 
	    result(add_account_access(Account, NewRole, Name, Access));
	E -> 
	    %% Rollback of remove ??
	    E
    end.

remove_roles(_Account, _Name, [], _Access) ->
    ok;
remove_roles(Account, Name, [Role | Rest], Access) ->
    ?dbg("remove_role:account ~p user ~p, role ~p.", [Account, Name, Role]),
    case result(remove_account_access(Account, Role, Name, Access)) of
	ok -> remove_roles(Account, Name, Rest, Access);
	E -> E
    end.	

%%--------------------------------------------------------------------
%% exodm interface    
%%--------------------------------------------------------------------
lookup_user(Name, Access) ->
    exodm_json_api:lookup_user(Name, opts(Access)).
list_user_accounts(Name, Access) ->
    Res = exodm_json_api:list_user_accounts(Name, opts(Access)), 
    ?dbg("list_user_accounts: name ~p access ~p, result ~p.", 
	 [Name, Access, Res]),
    Res.
create_account(Name, Email, Pass) ->
    exodm_json_api:create_account(Name, Email, Pass, Name, opts(root)).
delete_account(Name) ->
    exodm_json_api:delete_account(Name, opts(root)).
create_device_type(Acc, Name, Access) ->
    exodm_json_api:create_device_type(Acc, Name, "exodm", opts(Access)).
create_user(Name, Attrs, Access) ->
    exodm_json_api:create_user(Name, Attrs, opts(Access)).
delete_user(Name, Access) ->
    exodm_json_api:delete_user(Name, opts(Access)).
update_user(Name, Attributes, Access) ->
    exodm_json_api:update_user(Name, Attributes, opts(Access)).
list_account_users(Acc, Rows, Last, Direction, Access) -> 
    exodm_json_api:list_account_users(Acc, Rows, Last, 
				      atom_to_list(Direction), opts(Access)).
add_account_access(Acc, Role, Name, Access) ->
    exodm_json_api:add_account_access(Acc, Role, [Name], opts(Access)).
remove_account_access(Acc, Role, Name, Access) ->
    exodm_json_api:remove_account_access(Acc, Role, [Name], opts(Access)).
create_yang_module(Acc, File, Access) ->
    exodm_json_api:create_yang_module(Acc, 
				      "user", 
				      filename:basename(File), 
				      File, 
				      opts(Access)).
delete_yang_module(Acc, File, Access) ->
    exodm_json_api:delete_yang_module(Acc, 
				      "user",
				      filename:basename(File), 
				      opts(Access)).
list_yang_modules(Account, Rows, Last, Direction, Access) ->
    exodm_json_api:list_yang_modules(Account, "user", Rows, Last, 
				     atom_to_list(Direction), opts(Access)).

create_device(Acc, Id, Attrs, Access) ->
    %% Ugly solution to exodmapi limitations
    %% Device type name same as account name 
    exodm_json_api:create_device(Id, Acc, Attrs ++ [{"account", Acc}], 
	opts(Access)).
delete_device(Account, Id, Access) ->
    exodm_json_api:delete_devices(Account, [Id], opts(Access)).

list_devices_attributes(Acc, Rows, Last, Direction, Access) -> 
    Attrs = [atom_to_list(Name) || Name <-?DEVICE_LIST_ATTRS],
    ?dbg("list_devices_attributes: ~p", [Attrs]),
    exodm_json_api:list_devices_attributes(Acc, Rows, Last, Attrs, "*", 
					   atom_to_list(Direction), 
					   opts(Access)).
lookup_device_attributes(Account, Id, Access) ->
    %% Ugly solution to exodmapi limitations
    exodm_json_api:lookup_device_attributes(Account, Id,
	exoweb_device:attributes2fetch(),
	opts(Access)).

result(ResultStruct) ->
    result(ResultStruct, ok).

result({error, econnrefused} = E, E) ->
    ?dbg("result: result ~p", [E]),
    ok;
result({error, econnrefused} = E, _Wanted) ->
    ?dbg("result: result ~p", [E]),
    {error, no_contact_with_host};
result(ResultStruct, Wanted) ->
    ?dbg("result: wanted ~p, result ~p", [Wanted, ResultStruct]),
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
opts(_Access = {User, Pass}) ->
    ?dbg("opts:  access ~p.", [_Access]),
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
