% Scuffed generic worker module
-module(gen_worker).
-export([start/2, stop/1, async/2, await/1, await_all/1]).
% Define a function that implements gen_worker
-callback handle_work({Fun :: term(), Value :: term()}) -> Result :: term().

%%% Server functions
% Callback = Module that gets transported into gen_worker
start(Callback, Max) -> spawn(fun() -> init(Callback, Max) end).
        
init(Callback, Max) -> loop(make_workers(Max, Callback)).

% Asynchronous
stop(Pid) -> 
    Pid ! {stop},
    exit(Pid, kill).

% Asynchronous
async(Pid, W) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, W, async},
    Ref.

% Synchronous
await(Ref) ->
    collect({Ref, 0}).

% Synchronous
await_all(Refs) ->
    collect({Refs, []}).

% Loops with Worker list state
loop([Worker|T]) ->
    receive 
        {From, Ref, W, async} ->
            Worker ! {From, Ref, W},
            loop(T ++ [Worker]);
        {stop} ->
            [exit(Pid, kill) || Pid <- [Worker|T]]
    end.

worker(Mod) ->
    receive
        {From, Ref, W} ->
            try Mod:handle_work(W) of
                Result -> 
                    From ! {self(), Ref, Result}
            catch
                _:_ -> 
                    From ! {self(), Ref, error}
            end
    end,
    worker(Mod).

%%% Private functions
make_workers(Max, Mod) ->
    MaxWorkers = lists:seq(1, Max),
    lists:map(fun(_) -> 
                spawn(?MODULE, worker, [Mod]) 
                end, 
                MaxWorkers).

collect({[], Results}) ->
    lists:reverse(Results);
collect({Ref, 0}) ->
    receive
        {_From, Ref, error} ->
            error;
        {_From, Ref, Result} ->
            case Result of
                {result, R} ->
                    R;
                _ ->
                    no_result
            end
    end;
collect({[H|T], List}) ->
    receive
        {_From, _Ref, error} ->
            error;
        {_From, H, Result} ->
            case Result of
                {result, R} ->
                    collect({T, [R|List]});
                _ ->
                    collect({T, List})
            end
    end.