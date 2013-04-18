-module(crawler).

-export([start/0, stop/0, cold_start/0]).

-export([fetch/1,domains_to_crawl/0]).

-import(lists, [map/2]).

-define(USER_AGENT, "Mozilla/5.0 (Macintosh; U; Intel Mac OS X; en-US; rv: 1.8.1.3) Gecko/20070309 Firefox/2.0.0.3").
-define(ACCEPT_ENCODING, "gzip").
-define(ACCEPT_CHARSET, "utf-8").
-define(ACCEPT, "text/xml,application/xml,application/xhtml+xml,text/html;q= 0.9,text/plain;q=0.8").
-define(ACCEPT_LANGUAGE,"en-us,en;q=0.5").

cold_start() ->
    crawler_misc:ensure_dir(output_dir()),
    crawler_server:cold_start(output_dir(), test_domains_to_crawl()).

%% Note /home/joe/bigIndex Must be created first

output_dir()    -> "/Users/vb/houses/repos".
test_domains_to_crawl() -> ["www.nigelevans.co.uk","www.mortimersaylesbury.co.uk/","www.paulandcompany.co.uk"].

domains_to_crawl() ->
    domains_to_crawl("a.urls").
domains_to_crawl(UrlFile) ->
    crawler_misc:read_lines("/Users/vb/houses/" ++ UrlFile).

start() ->
    crawler_server:start(output_dir()),
    spawn_link(fun() -> worker() end).

stop() ->
    io:format("Scheduling a stop~n"),
    crawler_server:schedule_stop().

possibly_stop() ->
    case crawler_server:should_i_stop() of
	true ->
	    io:format("Stopping~n"),
	    crawler_server:stop(),
	    exit(stopped);
    	false ->
	    void
    end.

sleep(T) ->
    receive
    after T -> true
    end.

worker() ->
    possibly_stop(),
    case crawler_server:next_set_of_urls() of
	{ok, Urls} ->
	    fetch_these_urls(Urls),
	    possibly_stop(),
	    sleep(1000),
	    worker();
	done ->
	    true
    end.

fetch_these_urls(Urls) ->
    OutDir = filename:join(output_dir(), "pages"),
    crawler_misc:ensure_dir(OutDir),
    F1 = fun(Pid, Url) -> fetch(Pid, Url, OutDir) end,
    F2 = fun(Key, Val, Acc) -> handle_result(Key, Val, OutDir, Acc) end,
    crawler_misc:mapreduce(F1, F2, 0, Urls).

handle_result(Key, Vals, OutDir, Acc) ->
    add_urls_to_pool(OutDir, Key, Vals),
    Acc + 1.

add_urls_to_pool(OutDir, _, Urls) ->
    OutFile = filename:join(OutDir, "pool"),
    case file:open(OutFile, [write,binary,raw,append]) of
	{ok, S} ->
	    file:pwrite(S, 0, Urls),
	    file:close(S);
	{error, E} ->
	    exit({ebadFileOp, OutFile, E})
    end.

%%Fetching the page

fetch(Pid, Url, OutDir) ->
    Page = fetch(Url),
    crawler_misc:ensure_dir(filename:join(OutDir, crawler_misc:domain(Url))),
    OutFile = filename:join(OutDir, crawler_misc:url_file_name(Url)),
    file:write_file(OutFile, Page),
    Pid ! crawler_misc:normalize_urls(Url,crawler_misc:extract_urls(Page),false).

fetch(Url) ->
   {_Http, Host, _Port, _File} = parse(Url),
   {ok, {_StatusLine, Headers, Body}} = http:request(get,
        {Url,
         [{"Host", Host},
          {"User-Agent", ?USER_AGENT},
      {"Accept-Encoding", ?ACCEPT_ENCODING},
          {"Accept-Charset", ?ACCEPT_CHARSET},
          {"Accept", ?ACCEPT},
          {"Accept-Language", ?ACCEPT_LANGUAGE}]},
        [],
        [{body_format, binary}]),
    binary_to_list(get_body(Headers, Body)).

get_body(Headers, Body) ->
   case lists:keysearch("content-encoding", 1, Headers) of
       {value, {_, Value}} when Value =:= "gzip" -> zlib:gunzip(Body);
       _ -> Body
   end.

%%----------------------------------------------------------------------
%% parse(URL) -> {http, Site, Port, File} |
%%               {file, File}             | {error,Why}
%% (primitive)

parse([$h,$t,$t,$p,$:,$/,$/|T]) ->  parse_http(T);
parse([$f,$t,$p,$:,$/,$/|_T])    ->  {error, no_ftp};
parse([$f,$i,$l,$e,$:,$/,$/|F]) ->  {file, F};
parse(_X)                        ->  {error, unknown_url_type}.

parse_http(X) ->
   case string:chr(X, $/) of
   0 ->
       %% not terminated by "/" (sigh)
       %% try again
       parse_http(X ++ "/");
   N ->
       %% The Host is up to the first "/"
       %% The file is everything else
       Host = string:substr(X, 1, N-1),
       File = string:substr(X, N, length(X)),
       %% Now check to see if the host name contains a colon
       %% i.e. there is an explicit port address in the hostname
       case string:chr(Host, $:) of
       0 ->
           %% no colon
           Port = 80,
           {http, Host, Port, File};
       M ->
           Site = string:substr(Host,1,M-1),
           case (catch list_to_integer(
                 string:substr(Host, M+1, length(Host)))) of
           {'EXIT', _} ->
               {http, Site, 80, File};
           Port ->
               {http, Site, Port, File}
           end
       end
   end.
