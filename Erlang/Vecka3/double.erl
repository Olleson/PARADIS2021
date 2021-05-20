-module(double).
-export([start/0, double/0, double/1]).

start() ->
    Pid = spawn_link(?MODULE, double, []),
    register(?MODULE, Pid),
    {ok, Pid}.

double() ->
    receive
        {From, Ref, N} -> 
            From ! {Ref, N * 2}
    end,
    double().

double(Term) ->
    Ref = make_ref(),
    double ! {self(), Ref, Term},
    receive
        {Ref, Result} -> 
            Result
    after 5000 ->
        double(Term)
    end.
    
