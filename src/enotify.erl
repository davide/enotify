-module(enotify).
-export([start/0, start/1, stop/0, init/1]).
-export([add_watch/0, echo1/1, echo2/1, echo3/1, test/0, add_watch/3, remove_watch/1, foo/1, bar/1, call_port/2]).

start() ->
    ExtPrg =
	"priv/win32/executable/enotify" ++
	case os:type() of
	    {win32,nt} -> ".exe";
	    win32 -> ".exe"
			 ; _ -> ""
	end,
    start(ExtPrg).

start(ExtPrg) ->
    spawn(?MODULE, init, [ExtPrg]).

stop() ->
    ?MODULE ! stop.

foo(X) ->
    call_port(foo, {X}).
bar(Y) ->
    call_port(bar, {Y}).

add_watch() ->
    call_port(watch_dir, {"c:/Davide Marquês", 7, 1}).

add_watch(Dir, NotifyFilter, WatchSubdir) ->
    call_port(add_watch, {Dir, NotifyFilter, WatchSubdir}).

remove_watch(WatchID) ->
    call_port(add_watch, {WatchID}).

echo1(Dir) ->
    call_port(echo1, {Dir}).

echo2(Dir) ->
    Bin = unicode:characters_to_binary(Dir),
    call_port(echo2, {Bin}).

echo3(Dir) ->
    DirUTF16 = unicode:characters_to_binary(Dir, utf8, {utf16,little}),
    call_port(echo3, {DirUTF16}).

test() ->
    call_port(test, {}).

call_port(Func, Args) ->
    ?MODULE ! {call, self(), Func, Args},
    receive
        {?MODULE, Result} ->
            Result
    end.

init(ExtPrg) ->
    register(?MODULE, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}, binary, exit_status]),
%    loop(Port) % DEV-MODE
    try loop(Port)
    catch
	T:Err ->
	    error_logger:error_msg("catch: ~p:~p~n", [T, Err])
    end.

loop(Port) ->
    receive
        {call, Caller, _Func, Args} when not is_tuple(Args) ->
			Caller ! invalid_args;
		{call, Caller, Func, Args} ->
	    Binary = term_to_binary({Func, Args}),
	    io:format("Sending to port:~n~p ~p~n~p~n", [Func, Args, Binary]),
	    erlang:port_command(Port, Binary),
	    receive
		{Port, {data, undef}} ->
		    Caller ! undefined_function;
		{Port, {data, Data}} ->
		    Caller ! {?MODULE, binary_to_term(Data)};
		{Port, {exit_status, Status}} when Status > 128 ->
		    exit({port_terminated, Status});
		{Port, {exit_status, Status}} ->
		    exit({port_terminated, Status});
		{'EXIT', Port, Reason} ->
		    exit(Reason);
		%%   following two lines used for development and testing only
		Other ->
		    io:format("received: ~p~n", [Other])
	    after 5000 ->
			Caller ! no_reply_in_5_secs,
		    loop(Port)
	    end,
            loop(Port);
        stop ->
            erlang:port_close(Port),
            exit(normal);
        {'EXIT', Port, _Reason} ->
            exit(port_terminated);
	{Port, {data, Data}} ->
	    handle_port_message(binary_to_term(Data)),
	    loop(Port);
	_Other ->
	    io:format("received (unexpected): ~s~n", [_Other]),
	    loop(Port)
    end.

handle_port_message({File, Action}) ->
    io:format("file: ~ts, action: ~p~n", [File, Action]);
handle_port_message({WatchID, Dir, File, Action}) ->
    %Dir = unicode:characters_to_binary(DirUTF16, {utf16,little}, utf8),
    %%io:format("~w~n", [DirUTF16]),
    %File = unicode:characters_to_binary(FileUTF16, {utf16,little}, utf8),
    io:format("watchID: ~p, action: ~p, dir: ~ts, file: ~ts~n", [WatchID, Action, Dir, File]);
handle_port_message(Data) ->
	io:format("file: ~ts~n", [Data]),
	%io:format("unknown port message: ~w~n", [Data]),
	ok.
