%%%-------------------------------------------------------------------
%%% File    : crawler_urlknown.erl
%%% Author  : VB <vb@bigmac.local>
%%% Description : Figures out if the Url has already been visited or not.
%%%
%%% Created : 10 Jan 2008 by VB <vb@bigmac.local>
%%%-------------------------------------------------------------------
-module(crawler_urlknown).
-compile(export_all).
-import(lists, [reverse/1]).
-export([fetch/1,extract_urls/1,regexp_matches/2,crawl_within/1]).

UrlPool

LoadMonitor

WorkQueues

%%Try it with header
%%Connection value close
%%or Connection value Keep-Alive


crawl_within(Start) ->
    UrlTable = ets:new(urls, [ordered_set]),
    within_crawl(Start,UrlTable,normalize_urls(Start,crawl_page(UrlTable,Start),true)),
    ets:tab2file(UrlTable,"/Users/vb/houses/crawled/" ++ docid(Start) ++ ".start").

within_crawl(Start,UrlTable,Urls)->
    case Urls of
    [] -> done;
    [H|T] -> within_crawl(Start,UrlTable,T ++ normalize_urls(Start,crawl_page(UrlTable,H),true))
    end.

within_crawler(Start,Tab,[H|T]) ->
    Urls = [H|T],
    [First,Rest] = normalize_urls(Start,Urls,true),
    within_crawler(Start,Tab,Rest).

crawl_page(Tab,Url) ->
    Docid = insert_url(Tab, Url,fetched),
    Page = fetch(Url),
    file:write_file("/Users/vb/houses/crawled/" ++ Docid ++ ".html", Page),
    Urls = extract_urls(Page),
    for(fun(X)->insert_url(Tab,X,crawl) end,Urls),
    Urls.

%%Detect duplicates and filter unwanted URLs

isPageKnown(Page) ->
    %%detects duplicates
    false.

robotF(Url) ->
    fun (X) -> true end.

isUrlApproved(Url,RobotF) ->
    %%handles spider traps, robots.txt
    true.

analyze_page_insert_urls(Url, Page) ->
    IsKnown = isPageKnown(Page),
    RobotF = robotF(Url),
    Urls = lists:filter(fun(X)-> isUrlApproved(X, RobotF) end, normalize_urls(Url,extract_urls(Page),true)),
    for(fun(X)->insert_url(Tab,X,crawl) end,Urls).

