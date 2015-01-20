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
%%%-------------------------------------------------------------------
%%% @author  Malotte W Lönne <malotte@malotte.net>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    exoweb application.
%%%    yaws interface
%%%
%%% Created : 2015 by Malotte W Lönne
%%% @end
%%%-------------------------------------------------------------------
-module(exoweb_yaws).

-include_lib("yaws/include/yaws_api.hrl").
-include("exoweb.hrl").

%% API
-export([start_link/0, stop/0, out/1]).
  
-record(upload, {
          fd,
          filename,
          last,
	  account,
	  user,
	  password }).

-define(DIR, "/tmp/uploads/").

%%--------------------------------------------------------------------
%% @doc 
%% Start exodm web service.
%% @end
%%--------------------------------------------------------------------
start_link() ->
    ?dbg("start_link: set up links to wse.js and ej.js", []),
    WseWse = filename:join([code:lib_dir(wse), "priv", "wse.js"]),
    ExowebWse = filename:join([code:lib_dir(?EXOWEB), "www", "js", "wse.js"]),
    WseEj = filename:join([code:lib_dir(wse), "priv", "ej.js"]),
    ExowebEj = filename:join([code:lib_dir(?EXOWEB), "www", "js", "ej.js"]),
    Res1 = 
    case filelib:is_file(ExowebWse) of
	true -> ok;
	false -> os:cmd("ln -s " ++ WseWse ++ " " ++ ExowebWse)
    end,
    Res2 =
    case filelib:is_file(ExowebEj) of
	true -> ok;
	false -> os:cmd("ln -s " ++ WseEj ++ " " ++ ExowebEj)
    end,
    ?dbg("start_link: link result ~p, ~p",  [Res1, Res2]),
    ?dbg("start_link: starting yaws",  []),
    Id = "exoweb",
    GconfList = [{id, Id},
		 {logdir,exoweb:log_dir()}],
    Docroot = "/tmp",
    SconfList = [{port, exoweb:port()},
                 {servername, exoweb:servername()},
                 {listen, exoweb:ip()},
                 {docroot, exoweb:docroot()},
		 {appmods, [{"/fileupload", exoweb_yaws}]}],
    try yaws:start_embedded(Docroot, SconfList, GconfList, Id) of
	ok ->  ok
    catch
	error: E ->
	    ?dbg("start_link: starting yaws failed, error ~p",  [E]),
	    yaws:stop(), 
	    yaws:start_embedded(Docroot, SconfList, GconfList, Id)
    end,
    ?dbg("start_link: yaws started",  []),
    {ok, self()}.

%%--------------------------------------------------------------------
%% @doc 
%% Stop exodm web service.
%% @end
%%--------------------------------------------------------------------
stop() ->
    yaws:stop(),
    ok.


%%--------------------------------------------------------------------
%% @doc 
%% Callback from yaws when a file is uploaded
%% @end
%%--------------------------------------------------------------------
out(YawsArg=#arg{req = #http_request{method = 'POST'},
		 server_path = "/fileupload"}) ->
    ?dbg("out: POST ~p", [YawsArg]),
    out_i(YawsArg);
out(_Other) ->
   ?dbg("out: other ~p", [_Other]),
    [{status,404}].

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------

%% From yaws.hyber.org
out_i(A) when A#arg.state == undefined ->
    State = #upload{},
    authenticate(A, State);
out_i(A) ->
    authenticate(A, A#arg.state).



err() ->
    {ehtml,
     {p, [], "error"}}.

authenticate(A, State) ->
    H = A#arg.headers,
    C = H#headers.cookie,
    case yaws_api:find_cookie_val("id", C) of
	[] -> 
	    {error, illegal_cookie};
	Cookie ->
	    ?dbg("autenticate: Cookie ~p", [Cookie]),
	    case exoweb_cookie_handler:read(Cookie) of
		{ok, #exoweb_cookie{ user = User, 
				     account = Account,
				     password = PassWord}} ->
		    multipart(A, State#upload {user = User,
					       account = Account,
					       password = PassWord});
		{error, Reason} ->
		    ?dbg("cookie_check: wrong cookie ~p.",[Reason]),
		    {error, illegal_cookie}
	    end
    end.

multipart(A, State) ->
    Parse = yaws_api:parse_multipart_post(A),
    case Parse of
        {cont, Cont, Res} ->
            case addFileChunk(A, Res, State) of
                {done, Result} ->
		    ?dbg("multipart: done, result ~p", [Result]),
		    Result;
                {cont, NewState} ->
                    {get_more, Cont, NewState}
            end;
        {result, Res} ->
            case addFileChunk(A, Res, State#upload{last=true}) of
                {done, Result} ->
		    ?dbg("multipart: done, result ~p", [Result]),
                    Result;
                {cont, _} ->
                    err()
            end;
        {error, _Reason} ->
            err()
    end.



addFileChunk(A, [{part_body, Data}|Res], State) ->
    addFileChunk(A, [{body, Data}|Res], State);

addFileChunk(_A, [], State) when State#upload.last==true,
                                 State#upload.filename /= undefined,
                                 State#upload.fd /= undefined ->

    file:close(State#upload.fd),
    TmpFile = filename:join([?DIR,State#upload.filename]),
    Res = exoweb_yang:event({create, [{account, State#upload.account},
				      {user, State#upload.user},
				      {password, State#upload.password},
				      {filename, TmpFile}]}),
    file:delete(TmpFile),
    {done, Res};

addFileChunk(_A, [], State) when State#upload.last==true ->
    {done, err()};

addFileChunk(_A, [], State) ->
    {cont, State};

addFileChunk(A, [{head, {_Name, Opts}}|Res], State ) ->
    case lists:keysearch("filename", 1, Opts) of
        {value, {_, Fname0}} ->
            Fname = yaws_api:sanitize_file_name(basename(Fname0)),

            %% we must not put the file in the
            %% docroot, it may execute uploade code if the
            %% file is a .yaws file !!!!!
	    file:make_dir(?DIR),
	    case file:open([?DIR, Fname] ,[write]) of
		{ok, Fd} ->
		    S2 = State#upload{filename = Fname,
				      fd = Fd},
		    addFileChunk(A, Res, S2);
		_Err ->
		    {done, err()}
	    end;
	false ->
            addFileChunk(A,Res,State)
    end;

addFileChunk(A, [{body, Data}|Res], State)
  when State#upload.filename /= undefined ->
    case file:write(State#upload.fd, Data) of
        ok ->
            addFileChunk(A, Res, State);
        _Err ->
            {done, err()}
    end.


basename(FilePath) ->
    case string:rchr(FilePath, $\\) of
        0 ->
            %% probably not a DOS name
            filename:basename(FilePath);
        N ->
            %% probably a DOS name, remove everything after last \
            basename(string:substr(FilePath, N+1))
    end.
%%
