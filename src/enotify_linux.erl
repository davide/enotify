-module(enotify_linux).
-export([
	 port_init/2,
	 port_terminate/2,
	 pre_port_command/1,
	 post_port_command/1,
	 handle_port_event/1
	]).

port_init(Port, Timeout) ->
    case call_port(Port, Timeout, {open}) of
 	{ok, _Fd} -> ok;
        Other -> Other
    end.

port_terminate(Port, Timeout) ->
    case call_port(Port, Timeout, {close}) of
	{ok, _Fd} ->
	    ok;
	Other -> Other
    end.

pre_port_command({add_watch, Dir, Mask, WatchSubdir}) ->
    {command, {add, Dir, Mask}};
pre_port_command({remove_watch, WatchId}) ->
    {command, {remove, WatchId}}.

post_port_command(timeout) -> timeout;
post_port_command(What) -> What.

handle_port_event(Msg) ->
    io:format("Got ~p~n", [Msg]).

call_port(Port, Timeout, Command) ->
    erlang:port_command(Port, term_to_binary(Command)),
    receive
	{Port, {data, Data}} ->
	  binary_to_term(Data);
	{Port, {exit_status, _Status}} = Other ->
	    Other
    after Timeout ->
    	  timeout
    end.



