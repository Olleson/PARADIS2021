% Does not handle errors
-module(distribute).
-export([start/2, start/1, init/1, distribute/1, bucket/1, get/1]).

start(B1, B2) -> 
    spawn(?MODULE, init, [[B1, B2]]).

start(L) ->
    spawn(?MODULE, init, [L]).

init(L) -> 
    Buckets = [spawn(?MODULE, bucket, [V])|| V <- L],   % Spawn buckets with list comprehension
    distribute(Buckets).                                % Distribute buckets in loop

distribute([H|T]) ->
    receive
        {Sender, Ref, get_value} ->
            H ! {Sender, Ref, get_value}                % Sends Sender's pid directly to Bucket
    end,
    distribute(T ++ [H]).

bucket(Value) ->
    receive 
        {Sender, Ref, get_value} ->
            Sender ! {self(), Ref, Value}               % Send directly to user's Pid (shell)
    end,
    bucket(Value).

get(Pid) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, get_value},
    receive
        {_Sender, Ref, Value} ->
            Value
    end.