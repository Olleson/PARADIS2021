% Makes a ring of N-amount of links/chains/workers/processes
-module(ring).
-export([start/2]).

% N = Amount of processes, M = Amount of times around ring
% Value = 0, Acc = 1
start(N, M) -> ring(make_processes(N), {M, 0, 1}).

%%% Receive a ring, value and next workers, if at final worker, return value back to ring
process() ->
    receive
        {Ring, Value, []} ->        % If at final pid / Tail is empty. Plussing by 1 because value needs to go up regardless of receive.
            Ring ! Value + 1;
        {Ring, Value, [H|T]} ->     % If there is another worker
            H ! {Ring, Value + 1, T}
    end,
    process().

%%% Stops when Acc is higher than user inputted M
ring(Processes, {M, Value, Acc}) when Acc > M ->
    [exit(Pid, kill) || Pid <- Processes], 
    Value;
ring([H|T], {M, Value, Acc}) ->
    H ! {self(), Value, T},
    receive
        V -> ring([H|T], {M, V, Acc+1})
    end.

%%% Make N-amount of processes to use in ring/2
make_processes(N) ->
    lists:map(fun(_) -> spawn(?MODULE, process, []) end, lists:seq(1, N)).

% start(N, M) ->
%     {ok, spawn(fun() -> init(N, M) end)}.