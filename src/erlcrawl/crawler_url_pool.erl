%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---

-module(crawler_url_pool).
-export([open/1, close/0, start/1, next/1, url2index/1]).

start(Urls) ->
    open("./urls.dets"),
    lists:foreach(fun(Url) -> url2index(Url) end, Urls),
    1.

open(File) ->
    io:format("dets opened:~p~n", [File]),
    Bool = filelib:is_file(File),
    case dets:open_file(?MODULE, [{file, File}]) of
	{ok, ?MODULE} ->
	    case Bool of
		true  -> void;
		false -> ok = dets:insert(?MODULE, {free,1})
	    end,
	    true;
	{error,_Reason} ->
	    io:format("cannot open dets table~n"),
	    exit(eDetsOpen)
    end.

close() -> dets:close(?MODULE).

url2index(Url) when is_binary(Url) ->
    case dets:lookup(?MODULE, Url) of
	[] ->
	    [{_,Free}] = dets:lookup(?MODULE, free),
	    ok = dets:insert(?MODULE,
		             [{Free,Url},{Url,Free},{free,Free+1}]),
	    Free;
	[{_,N}] ->
	    N
    end.

index2url(Index) when is_integer(Index) ->
    case dets:lookup(?MODULE, Index) of
	[]        -> error;
	[{_,Bin}] -> Bin
    end.

next(Cont)->
    lists:map(fun(X) -> index2url(Cont + X) end, [0,1,2,3,4,5,6,7,8,9]).
    
