%%%-------------------------------------------------------------------
%%% File    : enotify.erl
%%% Author  :  <>
%%% Description : 
%%%
%%% Created : 14 May 2009 by  <>
%%%-------------------------------------------------------------------
-module(enotify).

-behaviour(gen_server).

%% API
-export([start_link/2,
	 add_watch/3,
	 remove_watch/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {port, timeout, port_module}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link(ExtProg, PortModule) -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(ExtProg, PortModule) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ExtProg, PortModule], []).

%%--------------------------------------------------------------------
%% Function: add_watch(Dir, Filters, WatchSubdir) -> {ok,WatchID} | {error,Reason}
%% Description: Adds a new directory monitor
%%--------------------------------------------------------------------
add_watch(Dir, Filters, WatchSubdir) when is_boolean(WatchSubdir) ->
    gen_server:call(?MODULE, {add_watch, Dir, Filters, WatchSubdir}).

%%--------------------------------------------------------------------
%% Function: remove_watch(WatchId) -> Any
%% Description: Removed a directory monitor
%%--------------------------------------------------------------------
remove_watch(WatchId) when is_integer(WatchId) ->
    gen_server:call(?MODULE, {remove_watch, WatchId}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([ExtPrg, PortModule]) ->
    process_flag(trap_exit, true),
    %io:format("~p ~p~n", [ExtPrg, PortModule]),
    Timeout = 5000,
    Port = open_port({spawn, ExtPrg}, [{packet, 2}, binary, exit_status]),
    ok = PortModule:port_init(Port, Timeout),
    {ok, #state{port=Port, timeout=Timeout, port_module=PortModule}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(Req, _From, State) ->
    #state{port = Port, timeout = Timeout, port_module = PortModule} = State,
    case PortModule:pre_port_command(Req) of
	{response, Response} ->
	    {reply, Response, State};
	{command, Command} ->
	    %io:format("pre_port_command: ~p~n", [Command]),
	    port_command(Port, term_to_binary(Command)),
	    case collect_response(Port, Timeout) of
		{response, Response} -> 
		    {reply, PortModule:post_port_command(Response), State};
		timeout -> 
		    {stop, port_timeout, State}
	    end
    end.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({_Port, {exit_status, Status}}, State) ->
    {stop, {port_terminated, Status}, State};
handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    {stop, {port_terminated, Reason}, State};
handle_info({Port, {data, Data}}, #state{port = Port, port_module = PortModule} = State) ->
    PortModule:handle_port_event(binary_to_term(Data)),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate({port_terminated, _Reason}, _State) ->
    ok;
terminate(_Reason, #state{port = Port, port_module=PortModule, timeout=Timeout} = _State) ->
    PortModule:port_terminate(Port, Timeout),
    port_close(Port).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
collect_response(Port, Timeout) ->
    receive
        {Port, {data, Data}} ->
            {response, binary_to_term(Data)}
    after Timeout -> 
            timeout
    end.
