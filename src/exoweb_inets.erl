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
%% Start exodm web service.
%% @end
%%--------------------------------------------------------------------
start_link() ->
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
                     {modules,       [?MODULE,
				      mod_alias, 
				      mod_auth, 
				      mod_esi, 
				      mod_actions, 
				      mod_cgi, 
				      mod_dir, 
				      mod_get, 
				      mod_head,
				      mod_log, 
				      mod_disk_log]},
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
do(ModData=#mod{method = "POST", 
		request_uri = "/fileUpload"}) ->
    ?dbg("do: fileUpload", []),
    do_fileupload(ModData);
do(ModData=#mod{method = M, 
		request_uri = Uri}) ->
    ?dbg("do: ~p, ~p", [M, Uri]),
    {proceed, ModData#mod.data}.

do_fileupload(ModData)->
    ?dbg("do_fileupload: ~p", [ModData]),
    {proceed, ModData#mod.data}.

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

