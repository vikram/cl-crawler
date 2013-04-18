%%%-------------------------------------------------------------------
%%% File    : crawler_server.erl
%%% Author  : VB <vb@bigmac.local>
%%% Description : Main Server, manages what gets crawled and holds the pool
%%%
%%% Created :  7 Jan 2008 by VB <vb@bigmac.local>
%%%-------------------------------------------------------------------
-module(crawler_server).

-export([cold_start/2, 
	 start/1, 
	 next_set_of_urls/0,
	 schedule_stop/0,
	 should_i_stop/0,
	 outdir/0, 
	 stop/0]).

-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-import(filename, [join/2]).

start(Dir) ->
    io:format("starting ~p ~p~n",[?MODULE, Dir]),
    gen_server:start({local,?MODULE}, ?MODULE, Dir, []).

schedule_stop() ->
    gen_server:call(?MODULE, schedule_stop).

should_i_stop() ->
    gen_server:call(?MODULE, should_i_stop).

stop() ->
    gen_server:cast(?MODULE, stop).

next_set_of_urls()   -> gen_server:call(?MODULE, next_urls).
outdir()     -> gen_server:call(?MODULE, outdir).

%% Cold start is done ONCE *before* the server
%% is started

cold_start(OutputDir, StartUrls) ->
    case file:list_dir(OutputDir) of
	{ok, []} ->
	    OutputDir1 = join(OutputDir,"pages"),
	    crawler_misc:ensure_dir(OutputDir1),
	    crawler_url_pool:start(StartUrls);
	_ ->
	    exit({eDirNotEmptyOrMissing, OutputDir})
    end.

-record(env,{cont, nextCP, outdir, stop=false}).

init(Dir) ->
    crawler_url_pool:open(join(Dir,"urls.dets")),
    {ok, #env{outdir=Dir, cont=1, nextCP = 11}}.

handle_call(next_urls, _From, S) ->
    Cont = S#env.cont,
    case crawler_url_pool:next(Cont) of
	{urls, Urls, _} ->
	    {reply, {ok, Urls}, S};
	done ->
	    {reply, done, S}
    end;

handle_call(schedule_stop, _From, S) ->
    {reply, ack, S#env{stop=true}};
handle_call(should_i_stop, _From, S) ->
    {reply, S#env.stop, S};
handle_call(outdir, _From, S) ->
    {reply, S#env.outdir, S}.
handle_cast(stop, S) ->
    {stop, normal, S}.

terminate(Reason, _) ->
    crawler_url_pool:close(),
    io:format("stopping ~p~n",[Reason]).

    



    


