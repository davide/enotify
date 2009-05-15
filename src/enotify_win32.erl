-module(enotify_win32).
-export([
	 port_init/2,
	 port_terminate/2,
	 pre_port_command/1,
	 post_port_command/1,
	 handle_port_event/1
	]).

port_init(_Port, _Timeout) ->
    ok.
port_terminate(_Port, _Timeout) ->
    ok.

pre_port_command({add_watch, Dir, Filters, WatchSubdir}) ->
    case create_mask(Filters) of
	{ok, Mask} ->
	    BinDir = unicode:characters_to_binary(Dir),
	    {command, {add_watch, {BinDir, Mask, watch_subdir(WatchSubdir)}}};
	Other ->
	    {response, Other}
    end.

post_port_command(timeout) -> timeout;
post_port_command(What) -> What.

handle_port_event({WatchID, Action, DirUTF8, FileUTF8}) ->
    Dir = unicode:characters_to_list(DirUTF8),
    File = unicode:characters_to_list(FileUTF8),
    handle_fs_event(WatchID, Action, Dir, File).


watch_subdir(true) -> 1;
watch_subdir(false) -> 0.

%% -------------------------------------------------------------------------
%%                Stuff for setting up filters on folders
%% -------------------------------------------------------------------------
-define(WIN32_FILE_NAME, 1).
-define(WIN32_DIR_NAME, 2).
-define(WIN32_ATTRIBUTES, 4).
-define(WIN32_SIZE, 8).
-define(WIN32_LAST_WRITE, 10).
-define(WIN32_LAST_ACCESS, 20).
-define(WIN32_CREATION, 40).
-define(WIN32_SECURITY, 100).

valid_filters() ->
    [{file_name, ?WIN32_FILE_NAME, "File creation, renaming and deleting."},
     {dir_name, ?WIN32_DIR_NAME, "Directory creation, renaming and deleting."},
     {attributes, ?WIN32_ATTRIBUTES, "Attribute changes."},
     {size, ?WIN32_SIZE, "File size changes (when these are flushed to disk)."},
     {last_write, ?WIN32_LAST_WRITE, "Last write-time changes (when these are flushed to disk)."},
     {last_access, ?WIN32_LAST_ACCESS, "Last access-time changes."},
     {creation, ?WIN32_CREATION, "Creation time changes."},
     {security, ?WIN32_SECURITY, "Security-descriptor changes."}].

filter_value(Filter) ->
    filter_data(Filter, 2).
filter_desc(Filter) ->
    filter_data(Filter, 3).
filter_data(Filter, N) ->
    ValidFilters = valid_filters(),
    case lists:keyfind(Filter, 1, ValidFilters) of
	false -> Filter;
	Tuple -> element(N, Tuple)
    end.

create_mask(All) when All =:= [all]; All =:= all ->
    ValidFilters = valid_filters(),
    Mask = lists:foldl(
	     fun({_,Value,_}, PartialMask) ->
		     PartialMask bor Value
	     end,
	     0, ValidFilters),
    {ok, Mask};
create_mask(Filters) ->
    BitsOrFilter = lists:foldl(
		     fun(Filter,L) ->
			     [filter_value(Filter)|L]
		     end,
		     [], Filters),
    case lists:filter(fun(X) -> not is_integer(X) end, BitsOrFilter) of
	[] ->
	    Mask = lists:foldl(
		     fun(Filter, PartialMask) ->
			     PartialMask bor filter_value(Filter)
		     end,
		     0,
		     Filters),
	    {ok, Mask};
	UnknowFilters ->
	    {unknown_filters, UnknowFilters}
    end.

%% -------------------------------------------------------------------------
%%           Stuff for sorting through the received FS events
%% -------------------------------------------------------------------------

% Since when something happens we can't distinguish between files and folder
% events a good approach might be to create two monitors for files/folders
-define(EVENT_ADDED, 1).
-define(EVENT_REMOVED, 2).
-define(EVENT_MODIFIED, 3).
-define(EVENT_RENAMED_OLD_NAME, 4).
-define(EVENT_RENAMED_NEW_NAME, 5).

handle_fs_event(_WatchID, ?EVENT_ADDED, Dir, File) ->
    io:format("File created: ~ts/~ts~n", [Dir, File]);
handle_fs_event(_WatchID, ?EVENT_REMOVED, Dir, File) ->
    io:format("File deleted: ~ts/~ts~n", [Dir, File]);
handle_fs_event(_WatchID, ?EVENT_MODIFIED, Dir, File) ->
    io:format("File modified: ~ts/~ts~n", [Dir, File]);
handle_fs_event(_WatchID, ?EVENT_RENAMED_OLD_NAME, Dir, File) ->
    io:format("In dir ~ts, file renamed from ~ts", [Dir, File]);
handle_fs_event(_WatchID, ?EVENT_RENAMED_NEW_NAME, _Dir, File) ->
    io:format("to ~ts", [File]);
handle_fs_event(_, Action, _, _) ->
    io:format("~s: unkown action ~p~n", [?MODULE_STRING, Action]).

