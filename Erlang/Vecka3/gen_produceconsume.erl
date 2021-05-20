% State machine for producing and consuming to prevent starvation
-module(gen_produceconsume).
-export([start/2, stop/1, consume/1, produce/2]).
-callback handle_produce(Input :: term()) -> {task, Task :: term()}.
-callback handle_consume(Input :: term()) -> Reply :: term().

start(Callback, T) -> spawn(fun() -> producing(Callback, T, []) end).

% Empty queue, only allows produce
producing(Mod, Limit, Queue) ->
    receive
        {produce, {Pid, Ref, T}} ->
            Pid ! {produce, Ref},
            produce_consume(Mod, Limit, Queue ++ [Mod:handle_produce(T)])
    end.

% Not full, not empty queue, allows both consume and produce 
produce_consume(Mod, Limit, []) ->
    producing(Mod, Limit, []);
produce_consume(Mod, Limit, Queue) when length(Queue) == Limit ->
    consuming(Mod, Limit, Queue);
produce_consume(Mod, Limit, [H|T]) ->
    receive
        {produce, {Pid, Ref, Task}} ->
            Pid ! {produce, Ref},
            produce_consume(Mod, Limit, [H|T] ++ [Mod:handle_produce(Task)]);
        {consume, {_Pid, _Ref}} ->
            Mod:handle_consume(H),
            produce_consume(Mod, Limit, T)
    end.

% Full queue, only allows consume
consuming(Mod, Limit, [H|T]) ->
    receive
        {consume, {_Pid, _Ref}} ->
            Mod:handle_consume(H),
            produce_consume(Mod, Limit, T)
    end.

%%% Interface
stop(Pid) -> exit(Pid, kill).

produce(Pid, T) ->
    Ref = make_ref(),
    Pid ! {produce, {self(), Ref, T}},
    receive
        {produce, Ref} ->
            ok
    end.

consume(Pid) ->
    Ref = make_ref(),
    Pid ! {consume, {self(), Ref}}.