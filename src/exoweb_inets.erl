%%% -*- coding: latin-1 -*-
%%%---- BEGIN COPYRIGHT --------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2013, Rogvall Invest AB, <tony@rogvall.se>
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
%%%-------------------------------------------------------------------
%%% @author  Malotte W Lönne <malotte@malotte.net>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    exoweb application.
%%%    inets interface
%%%
%%% Created : 2013 by Malotte W Lönne
%%% @end
%%%-------------------------------------------------------------------
-module(exoweb_inets).
-include_lib("inets/include/httpd.hrl").
-include_lib ("nitrogen_core/include/wf.hrl").
-include("exoweb.hrl").

%% API
-export([start_link/0, stop/0, do/1]).
  
%%--------------------------------------------------------------------
%% @doc 
%% This is the routing table for exodm web.
%% @end
%%--------------------------------------------------------------------
routes() ->
    [{"/",          exoweb_index},
     {"/favicon.ico", static_file},
     {"/apply",     exoweb_apply},
     {"/login",     exoweb_login},
     {"/user",      exoweb_user},
     {"/yang",      exoweb_yang},
     {"/device",    exoweb_device},
     {"/data",      static_file},
     {"/table",     exoweb_table},
     {"/about",     exoweb_about},
     {"/confirm",   exoweb_confirm},
     {"/images",    static_file},
     {"/nitrogen",  static_file},
     {"/core",      static_file},
     {"/elements",  static_file},
     {"/js",        static_file},
     {"/css",       static_file}
    ].

%%--------------------------------------------------------------------
%% @doc 
%% Start exodm web service.
%% @end
%%--------------------------------------------------------------------
start_link() ->
    ?dbg("start_link: set up links to nitrogen_core & nitrogen_elements", []),
    Nitrogen = filename:join([code:lib_dir(exoweb), "www", "nitrogen"]),
    Core = filename:join([code:lib_dir(exoweb), "www", "core"]),
    Elements = filename:join([code:lib_dir(exoweb), "www", "elements"]),
    Res1 = 
    case filelib:is_file(Core) of
	true -> ok;
	false -> os:cmd("ln -s " ++ code:lib_dir(nitrogen_core) ++ 
			    " " ++ Core)
    end,
    Res2 =
    case filelib:is_file(Elements) of
	true -> ok;
	false -> os:cmd("ln -s " ++ code:lib_dir(nitrogen_elements) ++ 
			    " " ++ Elements)
    end,
    Res3 = 
    case filelib:is_file(Nitrogen) of
	true -> ok;
	false -> os:cmd("ln -s " ++ code:lib_dir(nitrogen_core) ++ 
			    "/www " ++ Nitrogen)
    end,
    ?dbg("start_link: result:~n~p~n~p~n~p", [Res1, Res2, Res3]),

    ?dbg("start_link: starting inets",  []),
    inets:start(),
    {ok, Pid} = 
	case start_inets_httpd() of
	    {ok, _NewPid} = Reply -> 
		Reply;
	    {error, {already_started, Ref}} ->
		?dbg("start_link: inets already started."
		     "stop and restart.",  []),
		inets:stop(httpd, Ref),
		start_inets_httpd()
	end,
    link(Pid),
    {ok, Pid}.

%%--------------------------------------------------------------------
%% @doc 
%% Start httpd service.
%% @end
%%--------------------------------------------------------------------
start_inets_httpd() ->
    ?dbg("start_link: starting httpd service.",  []),
    inets:start(httpd,
                    [{port,          exoweb:port()},
                     {server_name,   exoweb:servername()},
	             {bind_address,  exoweb:ip()},
                     {server_root,   "."},
                     {document_root, exoweb:docroot()},
	             {keep_alive, true},
		     {keep_alive_timeout, 600},
		     {log_format, common},
		     {error_log,    "log/inets_error.log"},
		     {transfer_log, "log/inets_transfer.log"},
                     {modules,       [mod_log, mod_disk_log, ?MODULE]},
                     {mime_types,    [{"css",  "text/css"},
				      {"js",   "text/javascript"},
				      {"html", "text/html"},
				      {"png",  "image/png"},
				      {"jpg",  "image/jpeg"},
				      {"gif",  "image/gif"},
				      {"ico",  "image/x-icon"}]}
    ]). 
   
%%--------------------------------------------------------------------
%% @doc 
%% Stop exodm web service.
%% @end
%%--------------------------------------------------------------------
stop() ->
    httpd:stop_service({any, exoweb:port()}),
    ok.

%%--------------------------------------------------------------------
%% @doc 
%% httpd callback, see OTP documentation.
%% @end
%%--------------------------------------------------------------------
do(Info=#mod{method = "GET", 
	     request_uri = "/data/" ++ _File}) ->
    %% Get data
    data_do(Info);
do(Info) ->
    exoweb_do(Info).

%% Not needed any more ??
%% @private
nitrogen_do(_Info=#mod{http_version = HttpVersion,
		      parsed_header = Header,
		      request_uri = "/" ++ File}) ->
		
    %% Get nitrogen generic files from nitrogen_core/elements
    NitrogenFile = filename:join([code:lib_dir(exoweb), "www", File]),
    ?dbg("do: new file path ~p",  [NitrogenFile]),

    {Etag, Size, LastModified} = file_data(NitrogenFile),
    case lists:keyfind("if-none-match", 1, Header) of
	{"if-none-match", Etag} ->
	    ?dbg("do: file ~p already sent",  [NitrogenFile]),
	    {proceed, [{response, {response,
				   lists:keystore(code, 1, Header, {code, 304}),
				   nobody}}]};
	_NoMatch ->
	    send_file(NitrogenFile, Etag, Size, LastModified, HttpVersion)
    end.

data_do(Info=#mod{request_uri = "/data/" ++ File}) ->
    ?dbg("do: data file ~p",  [File]),
    Table = parse_uri(Info),
    %% ?dbg("do: data info ~p",  [Info]),
    case exoweb_js:fetch(Table, 
			 exoweb_lib:parse_options(http_uri:decode(File))) of
	{struct, Data} = JsonStruct when is_list(Data)->
	    Bin = iolist_to_binary(exo_json:encode(JsonStruct)),
	    ?dbg("do: Bin ~p",  [Bin]),
	    {proceed, [{response, {response,
				   [{code, 200},
				    {content_length, integer_to_list(byte_size(Bin))},
				    {content_type, "application/json"}],
				   [Bin]}}]};
	_Other ->
	    ?dbg("do: data error ~p",  [_Other]),
	    {proceed, [{response, {response,
				   [{code, 404}], 
				   nobody}}]}
    end.
    
exoweb_do(Info=#mod{request_uri = File}) ->
    %%?dbg("do: file ~p",  [File]),
    put(gettext_language, "sv"),
    RequestBridge = simple_bridge:make_request(inets_request_bridge, Info),
    ResponseBridge = simple_bridge:make_response(inets_response_bridge, Info),
    nitrogen:init_request(RequestBridge, ResponseBridge),
    replace_route_handler(),
    nitrogen:handler(exoweb_security_handler, exoweb_security_handler_callback),
    nitrogen:run().

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------

parse_uri(_Info=#mod{absolute_uri = "http://" ++ _Path = Uri}) ->
    {ok, {http, _Scheme, _Host, _Port, FullFile, _Query}} = 
       http_uri:parse(http_uri:decode(Uri)),
    case FullFile of
	"/$nitrogen/" ++ File -> File;
	"/nitrogen/" ++ File -> File;
	"/$elements/" ++ File -> File;
	"/plugins/" ++ File -> File;
	"/data/" ++ File -> File
    end;
parse_uri(Info=#mod{absolute_uri = Uri}) -> 
    %% Add "http://" to get well formed uri
    parse_uri(Info#mod{absolute_uri = "http://" ++ Uri}).



%%--------------------------------------------------------------------
%% @doc 
%% Transfers file to inets.
%% @end
%%--------------------------------------------------------------------
-spec send_file(File::string(),
		Etag::list(),
		Size::string(), 
		LastModified::list(),
		HttpVersion::string()) ->
		       {proceed, [{response, {response,
				   Header::list(), 
				   [Binary::binary()]}}]} |
		       {proceed, [{response, {response,
				   [{code, 404}], 
				   nobody}}]}.
send_file(File, Etag, Size, LastModified, HttpVersion) ->
    case file:read_file(File) of
	{ok, Binary} ->
	    ?dbg("send: file ~p read.",  [File]),
	    Header = 
		http_header(File, 200, Etag, Size, LastModified, HttpVersion),
	    ?dbg("send: header ~p.", [Header]),
	    {proceed, [{response, {response,
				   Header, 
				   [Binary]}}]};
	{error, Reason} ->
	    ?dbg("send: read of file ~p ,failed, reason ~p.",  [File, Reason]),
	    ?ee("sweb_inets: read of file ~s, failed, reason ~p",
		[File, Reason]),
	    %% This might happen if nitrogen has changed file names
	    %% or directory structure
	    {proceed, [{response, {response,
				   [{code, 404}], 
				   nobody}}]}
    end.

%%--------------------------------------------------------------------
%% @doc 
%% Creates http header.
%% @end
%%--------------------------------------------------------------------
-spec http_header(File::string(),
		Code::integer(),
		Etag::list(),
		Size::string(), 
		LastModified::list(),
		HttpVersion::string()) ->
		       Header::list().
http_header(File, Code, Etag, Size, LastModified, HttpVersion) ->
    lists:flatten([[{code, Code},
		    {cache_control, "max-age=3600"},
		    {content_length, Size}],
		   content_type(File),
		   LastModified,
		   etag(HttpVersion, Etag)]).

%%--------------------------------------------------------------------
content_type(File) ->
    [{content_type, content_type1(filename:extension(File))}].
content_type1(".html") ->
    "text/html";
content_type1(".js") ->
    "text/javascript";
content_type1(".css") ->
    "text/css";
content_type1(".png") ->
    "image/png";
content_type1(".jpg") ->
    "image/jpeg";
content_type1(".gif") ->
    "image/gif";
content_type1(".ico") ->
    "image/x-icon".

%%--------------------------------------------------------------------
etag("HTTP/1.1", Etag) -> [{etag, Etag}];
etag(_Version, _Etag) -> [].
    
%%--------------------------------------------------------------------
%% @doc 
%% Gets file info and converts to inets format.
%% @end
%%--------------------------------------------------------------------
-spec file_data(File::string()) ->
		       {Etag::string(), Size::string(), LastModified::list()}.
file_data(File)->
    {ok, FileInfo} = file:read_file_info(File), 
    LastModified = 
	case catch httpd_util:rfc1123_date(FileInfo#file_info.mtime) of
	    Date when is_list(Date) -> [{last_modified, Date}];
	    _ -> []
	end,
    Etag = httpd_util:create_etag(FileInfo),    
    Size = integer_to_list(FileInfo#file_info.size),
    {Etag, Size, LastModified}.

%%--------------------------------------------------------------------
%% @doc 
%% Change nitrogen route handler to table above.
%% @end
%%--------------------------------------------------------------------
replace_route_handler() ->
    wf_handler:set_handler(named_route_handler, routes()).

%% in your supervisor's loop function where you call 
%% nitrogen:run/0, you will need to add the following
%% code above your nitrogen:run/0 call
%%,
