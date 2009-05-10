-module(enotify).
-export([start/1, stop/0, init/1]).
-export([call_port/2, ping/0, foo/1, bar/1, echo_string/1, echo_binary/1, add_watch/0, add_watch/3, remove_watch/1]).

%% -------------------------------------------------------------------------
%%                          Basic Test functions
%% -------------------------------------------------------------------------
ping() ->
    call_port(ping, {}).

foo(X) ->
    call_port(foo, {X}).
bar(Y) ->
    call_port(bar, {Y}).

% Echoes strings captured using erl_iolist_to_string.
% Notice: will fail when given Unicode strings!
echo_string(Arg) when is_list(Arg)  ->
    call_port(echo_string, {Arg}).

% Echoes strings passed/and captured as a binary.
% Should have no problems dealing with Unicode values.
echo_binary(Arg) when is_list(Arg) ->
    Bin = unicode:characters_to_binary(Arg),
    case call_port(echo_binary, {Bin}) of
	Resp when is_binary(Resp) ->
	    Str = unicode:characters_to_list(Resp),
	    io:format("~ts~n", [Str]);
	Other -> Other
    end.

%% -------------------------------------------------------------------------
%%                         eNotify API functions
%% -------------------------------------------------------------------------
% Watch ./Какво for changes (you might want to create the directory first ;))
add_watch() ->
    BinDir = unicode:characters_to_binary("./" ++ [1050,1072,1082,1074,1086]),
    call_port(add_watch, {BinDir, 7, 1}).

add_watch(Dir, NotifyFilter, WatchSubdir) ->
    BinDir = unicode:characters_to_binary(Dir),
    IntWatchSubdir = 
	case WatchSubdir of
	    true -> 1;
	    1 -> 1;
	    _ -> 0
	end,
    call_port(add_watch, {BinDir, NotifyFilter, IntWatchSubdir}).

remove_watch(WatchID) ->
    call_port(remove_watch, {WatchID}).

%% -------------------------------------------------------------------------
%%                            Main functions
%% -------------------------------------------------------------------------
start(ExtPrg) ->
    spawn(?MODULE, init, [ExtPrg]).

stop() ->
    ?MODULE ! stop.

init(ExtPrg) ->
    register(?MODULE, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}, binary, exit_status]),
    Timeout = 5000,
%    loop(Port, Timeout) % DEV-MODE
    try loop(Port, Timeout)
    catch
	T:Err ->
	    error_logger:error_msg("catch: ~p:~p~n", [T, Err])
    end.

call_port(Func, Args) ->
    ?MODULE ! {call, self(), Func, Args},
    receive
        {?MODULE, Result} ->
            Result;
	{?MODULE, timeout, Timeout} ->
	    {timeout, Timeout}
    end.

loop(Port, Timeout) ->
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
	    after Timeout ->
			Caller ! {?MODULE, timeout, Timeout},
		    loop(Port, Timeout)
	    end,
            loop(Port, Timeout);
        stop ->
            erlang:port_close(Port),
            exit(normal);
        {'EXIT', Port, _Reason} ->
            exit(port_terminated);
	{Port, {data, Data}} ->
	    handle_port_message(binary_to_term(Data)),
	    loop(Port, Timeout);
	_Other ->
	    io:format("received (unexpected): ~s~n", [_Other]),
	    loop(Port, Timeout)
    end.

handle_port_message({WatchID, Action, DirUTF8, FileUTF8}) ->
    Dir = unicode:characters_to_list(DirUTF8),
    File = unicode:characters_to_list(FileUTF8),
    io:format("watchID: ~p, action: ~p, dir: ~ts, file: ~ts~n", [WatchID, Action, Dir, File]);
handle_port_message(Data) ->
	io:format("file: ~ts~n", [Data]),
	%io:format("unknown port message: ~w~n", [Data]),
	ok.

%% -------------------------------------------------------------------------
%%                 Deprecated (but SCM worthy) functions
%% -------------------------------------------------------------------------

%% This function was used to pass Windows' WideStrings to the C++ code
%% responsible for tracking FS changes. The strings were sent using the
%% expected encoding, so only a \0 terminator needed to be added to get
%% valid wstrings.
%% The function was deprecated (currently there's no support on the C side)
%% after figuring out how to do the UTF8-UTF16 conversions in C++.
%echo_binary_wstrings(Dir) ->
%    DirUTF16 = unicode:characters_to_binary(Dir, utf8, {utf16,little}),
%    call_port(echo3, {DirUTF16}).

%And the corresponding handle_port_message/1 clause...
%handle_port_message({WatchID, DirUTF16, FileUTF16, Action}) ->
%    Dir = unicode:characters_to_binary(DirUTF16, {utf16,little}, utf8),
%    File = unicode:characters_to_binary(FileUTF16, {utf16,little}, utf8),
%    io:format("watchID: ~p, action: ~p, dir: ~p, file: ~p~n", [WatchID, Action, Dir, File]);
