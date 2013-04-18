%%%-------------------------------------------------------------------
%%% File    : crawler_misc.erl
%%% Author  : VB <vb@bigmac.local>
%%% Description : holds the misc functions for this module
%%%
%%% Created :  7 Jan 2008 by VB <vb@bigmac.local>
%%%-------------------------------------------------------------------
-module(crawler_misc).
-export([mapreduce/4,docid/1,domain/1,regexp_matches/2,for/2,url_file_name/1,ensure_dir/1,read_lines/1]).
-export([extract_urls/1,normalize_urls/3]).
-import(lists, [filter/2, foreach/2, map/2, reverse/1]).

-define(URL_EXTRACTOR,
    [{full,"href=([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}|(((news|telnet|nttp|file|http|ftp|https)://)|(www|ftp)[-A-Za-z0-9]*\\.)[-A-Za-z0-9\\.]+)(:[0-9]*)?/[-A-Za-z0-9_\\$\\.\\+\\!\\*\\(\\),;:@&=\\?/~\\#\\%]*[^]'\\.}>\\),\\\"]", 6},
     {full_in, "href=\"([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}|(((news|telnet|nttp|file|http|ftp|https)://)|(www|ftp)[-A-Za-z0-9]*\\.)[-A-Za-z0-9\\.]+)(:[0-9]*)?/[-A-Za-z0-9_\\$\\.\\+\\!\\*\\(\\),;:@&=\\?/~\\#\\%]*[^]'\\.}>\\),\\\"]", 7},
     {relative,"href=(~/|/|\\./)([-A-Za-z0-9_\\$\\.\\+\\!\\*\\(\\),;:@&=\\?/~\\#\\%]|\\\\)+", 6},
     {relative_in,"href=\"(~/|/|\\./)([-A-Za-z0-9_\\$\\.\\+\\!\\*\\(\\),;:@&=\\?/~\\#\\%]|\\\\)+", 7}]).

-define(PROTOCOL,
	"[0-9a-z]+://").

-define(RELATIVE_URL,
	"(~/|/|\\./)([-A-Za-z0-9_\\$\\.\\+\\!\\*\\(\\),;:@&=\\?/~\\#\\%]|\\\\)+").

-define(DOMAIN_URL,
	"([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}|[a-z0-9]*(((news|telnet|nttp|file|http|ftp|https)://)|(www|ftp)[-A-Za-z0-9]*\\.)[-A-Za-z0-9\\.]+)(:[0-9]*)?").

-define(DOMAIN,
	"([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}|[a-z0-9]*[-A-Za-z0-9\\.]+)(:[0-9]*)?").

docid(Url) ->
     integer_to_list(erlang:phash2(Url)).

%%Hyperlink extractor and normalizer

extract_urls(Str) ->
    lists:flatmap(fun(X) -> extract_url_with_regex(X, Str) end,
          ?URL_EXTRACTOR).

extract_url_with_regex(Extractor,Str) ->
    {_,Regex,Start} = Extractor,
    extract_url(regexp_matches(Str,Regex),Start).

extract_url([],_)->
    [];
extract_url([H|T],Start) ->
    [string:sub_string(H,Start,length(H))|extract_url(T,Start)].

normalize_urls(Start, Urls, Within) ->
    lists:filter(fun(X)->length(X)>0 end, [normalize_url(Start, Within, Url) || Url <- Urls]).

normalize_url(Start, Within, Url) ->
    case string:sub_string(Url,1,1) of
	"/" -> Start ++ Url;
	_ -> case Within of
		 true -> case regexp:matches(Url, Start) of
			     {match, []} -> "";
			     _ -> Url
			 end;
		 false -> Url
	     end
    end.

%%----- mapreduce

%% F1(Pid, X) -> sends {Key,Val} messages to Pid
%% F2(Key, [Val], AccIn) -> AccOut

mapreduce(F1, F2, Acc0, L) ->
    S = self(),
    Pid = spawn(fun() -> reduce(S, F1, F2, Acc0, L) end),
    receive
	{Pid, Result} ->
	    Result
    end.

reduce(Parent, F1, F2, Acc0, L) ->
    process_flag(trap_exit, true),
    ReducePid = self(),
    %% Create the Map processes
    %%   One for each element X in L
    foreach(fun(X) -> 
		    spawn_link(fun() -> do_job(ReducePid, F1, X) end)
	    end, L),
    N = length(L),
    %% make a dictionary to store the Keys
    Dict0 = dict:new(),
    %% Wait for N Map processes to terminate
    Dict1 = collect_replies(N, Dict0),
    Acc = dict:fold(F2, Acc0, Dict1),
    Parent ! {self(), Acc}.

%% collect_replies(N, Dict)
%%     collect and merge {Key, Value} messages from N processes.
%%     When N processes have terminate return a dictionary
%%     of {Key, [Value]} pairs
collect_replies(0, Dict) ->
    Dict;
collect_replies(N, Dict) ->
    receive
	{Key, Val} ->
	    case dict:is_key(Key, Dict) of
		true ->
		    Dict1 = dict:append(Key, Val, Dict),
		    collect_replies(N, Dict1);
		false ->
		    Dict1 = dict:store(Key,[Val], Dict),
		    collect_replies(N, Dict1)
	    end;
	{'EXIT', _,  _Why} ->
	    collect_replies(N-1, Dict)
    end.

%% Call F(Pid, X)
%%   F must send {Key, Value} messsages to Pid
%%     and then terminate

do_job(ReducePid, F, X) ->
    F(ReducePid, X).

%%Some string functions

regexp_loop(_, Parts, _, []) ->
    lists:reverse(Parts);
regexp_loop(Str, Parts, _, Rem_Matches) ->
    {NextPt,PtLen} = hd(Rem_Matches),
    regexp_loop(Str,
        [string:substr(Str, NextPt, PtLen) | Parts],
        NextPt + PtLen,
        tl(Rem_Matches)).

regexp_matches(Str, Regex) ->
    {match, Matches} = regexp:matches(Str, Regex),
    regexp_loop(Str, [], 1, Matches).

%%Utilities

for(_,[])-> done;
for(F,[H|T]) -> F(H),
        for(F,[T]).

%%Crawler specific functions

domain(Url) ->
    [DomainUrl] = regexp_matches(Url, ?DOMAIN_URL),
    case regexp_matches(DomainUrl, ?DOMAIN) of
	["http:",Domain] ->
	    Domain;
	[Domain] ->
	    Domain;
	_ ->
	    []
    end.

url_file_name(Url) ->
    [DomainUrl] = regexp_matches(Url, ?DOMAIN_URL),
    Path = case regexp_matches(Url,?PROTOCOL) of
	       [Protocol] ->
		   string:substr(Url, length(Protocol ++ DomainUrl) + 1, length(Url) - length(Protocol ++ DomainUrl));
	       [] ->
		   string:substr(Url, length(DomainUrl) + 1, length(Url) - length(DomainUrl))
	   end,
    {ok, CleanPath, _} = regexp:gsub(Path,"[/?=\\.]",""),
    DomainUrl ++ "/" ++ CleanPath.

ensure_dir (Dir) ->
    DirExists = filelib:is_dir(Dir),
    if DirExists =:= false ->
	    io:format("Creating ~p~n",[Dir]),
	    file:make_dir(Dir);
       DirExists =:= true ->
	    ok
    end.

read_lines (File) ->
    {ok, Bin} = file:read_file(File),
    string:tokens(binary_to_list(Bin),"\r\n").
    

  
