-module(babes).
-behaviour(gen_worker).
-export([handle_work/1, detect_babes/1]).

%% Dope handle_work: detect whether a person is a babe
handle_work(Name) ->
    case Name of
        simon -> {result, Name};
        _ -> no_result
    end.

detect_babes(Names) ->
    %% Start a work-pool with 2 workers
    WorkPool = gen_worker:start(?MODULE, 2),

    %% Schedule the work asynchronously
    Refs = [gen_worker:async(WorkPool, Name) || Name <- Names],

    %% Await the result
    Result = gen_worker:await_all(Refs),

    %% Stop our work pool
    gen_worker:stop(WorkPool),

    %% Return the result
    %% It will be a list of only the names that were determined to be babes,
    %% aka it excludes any names that handle_work returned no_result for!
    Result.