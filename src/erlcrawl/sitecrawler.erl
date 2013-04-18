%%%-------------------------------------------------------------------
%%% File    : sitecrawler.erl
%%% Author  : VB <vb@bigmac.local>
%%% Description : 
%%%
%%% Created : 10 Jan 2008 by VB <vb@bigmac.local>
%%%-------------------------------------------------------------------
-module(sitecrawler).

-compile(export_all).

start() ->
    register(bigdaddy, spawn(fun() -> loop([]) end)).

stop() ->
    bigdaddy ! stop.

for(Max, Max, F) -> [F(Max)]; 
for(I, Max, F) -> [F(I)|for(I+1, Max, F)].

setup_pool(N) ->
    Pool = for(1,N, fun(X) -> spawn(fun() -> crawler(X) end) end),
    Pool.

rpc(Request) ->
    bigdaddy ! {self(), Request},
    receive
	{bigdaddy, Response} ->
	    Response
    end.

loop(X) ->
    receive
	stop ->
	    void;
	Any ->
	    io:format("Received:~p~n",[Any]),
	    loop(X)
    end.

crawler(I) ->
    receive
	stop ->
	    void;
	{crawl, Host, Throttle, Visited, Max} ->
	    
