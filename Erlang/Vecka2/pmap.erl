% Parallelized map functions
-module(pmap).
-export([unordered/2, unordered/3, ordered/3]).

worker() ->
    receive
    {From, Fun, X} ->
        From ! {self(), Fun(X)}
    end,
    worker().

% Unordered/2 implementation
% Distributes work among L-amount of workers and collects without guaranteed order
unordered(Fun, L) ->
    Workers = lists:map(fun(_) -> spawn(fun worker/0) end, L),
    give_work(Fun, L, Workers, self()),
    collect([], length(L)).
    %collect(Workers, []).

give_work(_, [], _, _) ->
    [];
give_work(Fun, [H|T], [Worker|Workers], From) ->
    Worker ! {From, Fun, H},
    give_work(Fun, T, Workers, From).

% Does not guarantee order as it receives from anyone
collect(R, L_length) when length(R) == L_length ->
    lists:reverse(R);
collect(List, L_length) ->
    receive
    {_, R} ->
        collect([R|List], L_length)
    after 1000 ->
        collect(List, L_length)
    end.

%% ordered list
% collect([], R) ->
%     lists:reverse(R);
% collect([Worker|Workers], List) ->
%     receive
%     {Worker, R} ->
%         collect(Workers, [R|List])
%     end.

% Unordered/3 implementation, uses unordered/2's give_work
unordered(Fun, L, MaxWorkers) ->
    if MaxWorkers > length(L) ->
        Max = lists:seq(1, length(L));
    true ->
        Max = lists:seq(1, MaxWorkers)
    end,
    Workers = lists:map(fun(_) -> spawn(fun worker/0) end, Max),
    give_work(Fun, L, Workers, self(), Workers),
    collect([], length(L)).

give_work(_, [], _, _, _) ->
    [];
give_work(Fun, List, [], From, W) ->    % if workers are empty, recurse with the saved W to start over
    give_work(Fun, List, W, From, W);
give_work(Fun, [H|T], [Worker|Workers], From, W) ->
    Worker ! {From, Fun, H},
    give_work(Fun, T, Workers, From, W).

% Ordered/3 implementation, uses unordered/3's give_work
ordered(Fun, L, MaxWorkers) ->
    if MaxWorkers > length(L) ->
        Max = lists:seq(1, length(L));
    true ->
        Max = lists:seq(1, MaxWorkers)
    end,
    Workers = lists:map(fun(_) -> spawn(fun worker/0) end, Max),
    give_work(Fun, L, Workers, self(), Workers),
    collect(Workers, [], Workers, length(L)).

% Guarantees order as it recurses through worker pids to collect
collect(_, R, _, List_length) when length(R) == List_length ->  % if result's list is as long as original list, return it
    lists:reverse(R);
collect([], List, W, List_length) ->                            % if workers are empty, recurse with saved W to start over
    collect(W, List, W, List_length);
collect([Worker|Workers], List, W, List_length) ->
    receive
        {Worker, R} ->
            collect(Workers, [R|List], W, List_length)
    after 1000 ->
        collect(Workers, List, W, List_length)
    end.