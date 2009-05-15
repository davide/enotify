-module(enotify_app).

-behavior(application).

%% application callbacks
-export([start/2, 
         stop/1]).

start(_Type, [Build] = _Args) ->
    case port_config(os:type(), Build) of
	{error, _} = Ret ->
	    Ret;
	{PortModule, ExtPrg} ->
	    enotify_sup:start_link(ExtPrg, PortModule)
    end.

port_config({win32,_}, msvc) ->
    {enotify_win32, "priv/win32/enotify/msvc/Release/enotify.exe"};
port_config({win32,_}, Build) ->
    {enotify_win32, "priv/win32/enotify/" ++ atom_to_list(Build) ++ "/enotify.exe"};
port_config(OS, Build) ->
    {error, {unsupported_system, OS, Build}}.

stop(_State) ->
    ok.

