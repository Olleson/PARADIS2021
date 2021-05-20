-module(pmap_gen_worker).
-behaviour(gen_worker).
-export([handle_work/1, ordered/2]).

handle_work({Fun, V}) ->
    {result, Fun(V)}.

ordered(Fun, L) ->
    WorkPool = gen_worker:start(?MODULE, 2),

    Refs = [gen_worker:async(WorkPool, {Fun, V}) || V <- L],
    % Refs = [gen_worker:async(WorkPool, {Fun, L})],

    Result = gen_worker:await_all(Refs),
    % Result = gen_worker:await(Ref),

    gen_worker:stop(WorkPool),

    Result.

% test inputs:
% c(gen_worker), c(pmap_gen_worker).
% pmap_gen_worker:ordered(fun (X) -> X*2 end, [1,2,3]). 
% outcome -> [2, 4, 6]

