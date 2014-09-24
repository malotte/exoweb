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
%%% @author Malotte W Lönne <malotte@malotte.net>
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

%% Callbacks for nitrogen
-export([main/0, 
         title/0, 
         layout/0, 
         event/1,
         event_invalid/1,
         jqgrid_event/1]).

%% Callback rom exoweb_data_if
-export([attributes2fetch/0]).

%% "Internal" exports
-export([create_device/1,
         fetch_device/1,
         delete_device/1]).

-import(exoweb_lib, [dialog/1, postback/1, error_txt/1]).

%%--------------------------------------------------------------------
%% @doc 
%% Nitrogen callback for creating page.
%% @end
%%--------------------------------------------------------------------
-spec main() -> list(nitrogen_element()).

main() -> 
    case wf:user() /= undefined of 
        true  -> main_authorized();
        false -> wf:redirect_to_login("/login")
    end.
%%--------------------------------------------------------------------
%% @doc 
%% Autorized access
%% @end
%%--------------------------------------------------------------------
-spec main_authorized() -> list(nitrogen_element()).

main_authorized() ->
    exoweb_lib:content_type_html(),
    exoweb_lib:template().

%%--------------------------------------------------------------------
%% @doc 
%% Nitrogen callback for creating title.
%% @end
%%--------------------------------------------------------------------
-spec title() -> list(nitrogen_element()).

title() -> exoweb_lib:title().

%%--------------------------------------------------------------------
%% @doc 
%% Nitrogen callback for creating page content.
%% @end
%%--------------------------------------------------------------------
-spec layout() -> list(nitrogen_element()).

layout() ->
    %% To make side updateable
    wf:comet(fun() -> dummy_loop() end),
    %% Hidden dummy to keep comet alive
    wf:wire(dummy, #hide{}),

    %% Adding attribute actions and validations. 
    %% Postbacks must be wired on change since default is only on return.
    lists:foreach(fun(Attr) ->
	?dbg("layout: wiring ~p.",[Attr]),
 	wf:wire(dialog(Attr), #event {type=change, postback=postback(Attr)}),
	field_validation(dialog(Attr), Attr)
	end, ?DEVICE_ATTRS),

    %% Adding button validators
    button_validation(but_create),
    button_validation(but_fetch),
    button_validation(but_delete),
    button_validation(but_update),

    #container_12 {
        body=[
	    #flash{},
	    #grid_12 { class=header, body=exoweb_lib:header(device) },
	    #grid_clear {},
	    #grid_12 {
		body=[
		    #h1 { 
			class=info,
			text=?UTXT("Device administration."),
			style="text-align: center",
			html_encode=false}
		]
	    },
	    #grid_clear {},
	    #grid_6 {
		alpha=true, %% First in row
		body=[
		    #panel { 
			id=devices,
			body=devices() 
		}]
	    },
	    #grid_6 {
		omega=true, %% Last in row
		body=[
		    #panel { 
			id=device_dialog,
			class=dialog, 
			body=device_dialog() 
		}]
	    },

	    #grid_clear {},
	    #panel { id=dummy }, 
	    #grid_12 { body=exoweb_lib:footer() }
    ]}.

%%--------------------------------------------------------------------
%% @doc 
%% Elements for device table.
%% @end
%%--------------------------------------------------------------------
-spec devices() -> list(nitrogen_element()).

devices() ->
    ?dbg("table: devices", []),
 
    %% Init session variables if not set
    lists:map(fun('is-connected' = Attr) ->
	              wf:session(Attr, 
	                   wf:session_default(Attr, ?UTXT("Unknown")));
	          (Attr) ->  
	             wf:session(Attr, wf:session_default(Attr, []))
	      end, ?DEVICE_ATTRS),

    %% Dynamic table creation
    ColNames = [ColName || {ColName, _Name, _} <- ?DEVICE_MODEL],
    ColModel = [[{name, Name} | Attrs] || 
        {_ColName, Name, Attrs} <- ?DEVICE_MODEL],
  
    TableId = device_table,
    Table =
    [
	#jqgrid{
	    id=TableId,
	    options=[
		{url, '/data/device'},
		{datatype, <<"json">>},
		{colNames, ColNames},
		{colModel, ColModel},
		{rowNum, 10},
		{sortname, 'device-id'},
		{viewrecords, false},
		{pginput, false},
		{sortorder, <<"asc">>},
		{caption, <<"Device table">>},
		{postData, exoweb_js:init_postdata()},
		{groupColumnShow, false},
		{loadonce, false},
		{scrollOffset, 0}, %% switch off scrollbar
		{autowidth, true} %% fill parent container on load
	    ],

	    actions = exoweb_table:table_actions(TableId)
	    
	}
    ],

    Table.


%%--------------------------------------------------------------------
%% @doc 
%% Elements for device dialog.
%% @end
%%--------------------------------------------------------------------
-spec device_dialog() -> list(nitrogen_element()).

device_dialog() ->
    ?dbg("device_dialog: building dialog.", []),

    Elements = attrs_to_nitrogen(?DEVICE_ATTRS, []),
    
    %% Dialog elements
    Dialog = Elements ++
    [
	#br {},
        #button { 
            id=but_create,
            text="Create", 
            handle_invalid=true,
            on_invalid=#alert{text="At least one validator failed"},
            postback=create },
	#button { 
            id=but_fetch,
            text="Fetch", 
            postback=fetch },
 	#button { 
            id=but_delete,
            text="Delete", 
            postback=delete },
        #button { 
            id=but_update,
            text="Update", 
            postback=update },
        #button { 
            id=but_clear,
            text="Clear", 
            postback=clear }
   ],
    
 
    Dialog.
  

%%--------------------------------------------------------------------
%% @doc 
%% Callback from nitrogen when an jqgrid event has occured
%% @end
%%--------------------------------------------------------------------
-spec jqgrid_event({PostBack::atom(), Data::term()}) -> ok.

jqgrid_event(Event) ->
    ?dbg("jqgrid_event: event ~p",[Event]),
    event(Event).


%%--------------------------------------------------------------------
%% @doc 
%% Callback from nitrogen when an invalid event has occured
%% @end
%%--------------------------------------------------------------------
-spec event_invalid(E::atom()) -> ok.

event_invalid(Event) ->
    ?dbg("event_invalid: ~p",[Event]),
    wf:wire(#alert{text=?UTXT("At least one validator failed.")}).
	
%%--------------------------------------------------------------------
%% @doc 
%% Callback from nitrogen when an event has occured
%% @end
%%--------------------------------------------------------------------
-spec event(E::atom()) -> ok.

event({select_row, {Id, "true"}} = Event) ->
    ?dbg("event: ~p",[Event]),
    wf:session('device-id', Id),
    device_action(fetch_device, Id, 
	"Device is fetched: ","Device fetch failed: " ), 
    ok;
event({select_row, {_Name, "false"}} = Event) ->
    ?dbg("event: ~p",[Event]),
    ok;
event({load_complete, JsonData} = Event) ->
    ?dbg("event: ~p",[Event]),
    exoweb_js:userdata2postdata(device_table, JsonData),
    ok;
event({postback, Attr} = Event)  ->
    %% A dialog field has been updated
    ?dbg("event: ~p",[Event]),
    Value = wf:q(dialog(Attr)),
    ?dbg("event: value ~p",[Value]),
    %% Disable update if device id is changed on selected
    if Attr == 'device-id' -> 
	?dbg("event: update disabled",[]),
	wf:wire(but_update, #disable{});
	true -> do_nothing
    end,
    ok;
event(Event) 
  when Event == create;
       Event == fetch;
       Event == update;
       Event == delete ->
    device_event(Event);
event(clear = Event) ->
    ?dbg("event: ~p",[Event]),
    clear_attributes(),
    wf:update(device_dialog, device_dialog()),
    ok;
event(logout = Event) ->
    ?dbg("event: ~p",[Event]),
    exoweb_lib:logout();
event(Event) when is_atom(Event) ->
    case atom_to_list(Event) of
	"postback-" ++ Attr -> event({postback, list_to_atom(Attr)});
	_Other -> ?dbg("event: unknown event ~p",[Event]), ok
    end;
event(Event) ->
    ?dbg("event: unknown event ~p",[Event]),
    ok.

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------

%% General check that a device has been choosen
device_event(Event) ->
    ?dbg("device_event: ~p",[Event]),
    case wf:q(dialog('device-id')) of
	"" ->
	    wf:wire(#alert {text=?UTXT("No device id given!")});
	Id ->
	    ?dbg("event: device ~p",[Id]),
	    do_event(Event, Id)
    end,
    wf:wire(but_update, #enable{}),
    %% Deselect row ???
    ok.

do_event(create, Id) ->
    device_action(create_device, Id, 
	"Device is created: ","Device creation failed: " );    
do_event(fetch, Id) ->
    device_action(fetch_device, Id, 
	"Device is fetched: ","Device fetch failed: " );    
do_event(delete, Id) ->
    device_action(delete_device, Id, 
	"Device is deleted: ","Device deletion failed: " );    
do_event(update, Id) ->
    %% Same as create so far
    device_action(create_device, Id, 
	"Device is updated: ","Device update failed: " ).

device_action(F, Id, OkTxt, NokTxt) ->
    case apply(?MODULE, F, [Id]) of
	ok -> 
	    ?dbg("event: ~p(~p) ok.", [F, Id]),
	    post_device_action(F, [?UTXT(OkTxt), Id]),
	    ok;
	{error, Reason} = E->
	    ?dbg("event: ~p(~p) failed, reason ~p.", 
		[F, Id, Reason]),
	   wf:wire(#alert {text=[?UTXT(NokTxt), error_txt(Reason)]}),
	   E
    end.

post_device_action(fetch_device, _Txt) ->
    wf:update(device_dialog, device_dialog()),
    wf:wire(but_delete, #enable{}),
    ok;
post_device_action(_F, Txt) ->   
    wf:wire(#alert {text=Txt}),
    wf:update(devices, devices()),
    ok.
    

%%--------------------------------------------------------------------
%% @doc 
%% 
%% @end
%%--------------------------------------------------------------------
-spec create_device(Id::string()) -> 
    ok.

%% @private
create_device(Id) ->
    exoweb_data_if:create(populate_device(Id)).

-spec delete_device(Id::string()) -> 
    ok.

%% @private
delete_device(Id) ->
    ?dbg("delete_device: ~p.",[Id]),
    exoweb_data_if:delete(#exoweb_device{id = Id}).

%% @private
-spec fetch_device(Id::string()) -> 
    ok.
fetch_device(Id) ->
    ?dbg("fetch_device: ~p.",[Id]),
    exoweb_data_if:read(#exoweb_device{id = Id}).

populate_device(Id) ->
    Attrs = [{atom_to_list(Attr), wf:q(dialog(Attr))} || 
         Attr <- ?DEVICE_ATTRS, Attr =/= 'device-id', Attr =/= 'is-connected'],
    ?dbg("populate_device:  ~p, ~p.",[Id, Attrs]),
    #exoweb_device{id = Id, attributes = Attrs}.


%%--------------------------------------------------------------------
field_validation(Trigger, 'device-id' = Attr) ->
   wf:wire(Trigger, dialog(Attr), #validate { validators=[
        #is_required { text=?UTXT("Required.") }]});
field_validation(Trigger, 'server-key' = Attr) ->
    wf:wire(Trigger, dialog(Attr), #validate { validators=[
        #is_required { text=?UTXT("Required.") },
        #is_integer { 
	    min=1, max=9999, 
	    text=?UTXT("Enter an integer between 1 and 9999.") }]});
field_validation(Trigger, 'device-key' = Attr) ->
    wf:wire(Trigger, dialog(Attr), #validate { validators=[
        #is_required { text=?UTXT("Required.") },
        #is_integer { 
	    min=1, max=9999, 
	    text=?UTXT("Enter an integer between 1 and 9999.") }]});
field_validation(Trigger, 'msisdn' = Attr) ->
  wf:wire(Trigger, dialog(Attr), #validate { validators=[
        #custom { 
	    tag=phone, function=fun exoweb_lib:is_phone_no/2, 
	    text=?UTXT("Must be a valid phone number.") }]});
field_validation(_, _) ->
    ok.


button_validation(Button) 
  when Button == but_delete;
       Button == but_fetch ->
    field_validation(Button, 'device-id');
button_validation(Button) 
  when Button == but_create ->
    lists:map(fun(Attr) ->
	          field_validation(Button, Attr)
              end, ?DEVICE_ATTRS);
button_validation(Button) 
  when Button == but_update ->
    ok.
   

attributes2fetch() ->
    [atom_to_list(A) || A <- ?DEVICE_ATTRS,A =/= 'device-id'].

clear_attributes() ->
    ?dbg("clear attributes:", [?DEVICE_ATTRS]),
    lists:map(fun(Attr) -> wf:session(Attr, "") end, ?DEVICE_ATTRS).

attrs_to_nitrogen([Attr], Acc) ->
    lists:reverse(attr_to_nitrogen(Attr, last) ++ Acc);
attrs_to_nitrogen([Attr, Next | Tail], Acc) ->
    attrs_to_nitrogen([Next | Tail], attr_to_nitrogen(Attr, Next) ++ Acc).


attr_to_nitrogen('is-connected' = Attr, last) ->
    %% Must be last !!!
    Value = wf:session(Attr),
    ?dbg("attr_to_nitrogen: ~p = ~p.",[Attr, Value]),
    [#span { 
	id=dialog(Attr),
	text=Value},
     #label { text=?UTXT(atom_to_list(Attr))}];
attr_to_nitrogen(Attr, Next) ->
    Value = wf:session(Attr),
    ?dbg("attr_to_nitrogen: ~p = ~p.",[Attr, Value]),
    [#textbox { 
	id=dialog(Attr),
	text=Value,
	postback = postback(Attr),
	next=dialog(Next)},
     #label {text=?UTXT(atom_to_list(Attr))}].
		  		      
%%--------------------------------------------------------------------
%% @private
%% Comet receive loop.
%% Update table when when data is changed.
%% @end
%%--------------------------------------------------------------------
-spec dummy_loop() -> no_return().

dummy_loop() ->
   receive
	'INIT' ->
            %% The init message is sent to the first process in a comet pool.
	    %% Nothing to do right now
	    ?dbg("dummy_loop: got event: INIT", []),
	    dummy_loop();
        {'EXIT', _, Message} -> 
	    ?dbg("dummy_loop: The user has left the page, message ~p.",
		[Message]),
	    exit(done);
	Other ->
	    ?dbg("dummy_loop: got event: ~p.", [Other]),
	    dummy_loop()
    after 
	?LOOP_TIMEOUT -> %% Nitrogen accumulator timeout ??
  	    %% ?dbg("dummy_loop: timeout, recursive call", []),
            wf:update(dummy, io_lib:format("~w",[sz_util:sec()])),
            wf:flush(),
            dummy_loop()
    end.
    
   
