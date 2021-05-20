% Monitor double.erl
-module(monitor).
-export([start/0, monitor/0]).

% Start double-process, then spawn monitor process loop
start() ->
    double:start(),
    spawn(?MODULE, monitor, []).

% Monitor double, receive error messages
% If error is received, demonitor old process and restart double
monitor() ->
    MonitorRef = erlang:monitor(process, double),
    receive 
        {'DOWN', MonitorRef, process, Pid, Reason} ->
            io:format("crash at ~p because ~p", [Pid, Reason]),
            demonitor(MonitorRef),
            double:start(),
            monitor()               % Loop monitor process
    end.