% Double a term
-module(double).
-export([start/0, double/0, double/1]).

% Spawn double process, register under name "double"
start() ->
    Pid = spawn(?MODULE, double, []),
    register(double, Pid).

% Receive message and send operation to shell
double() ->
    receive
        {From, Ref, N} -> 
            From ! {Ref, N * 2},
            double()               % Loop
    end.

% Send operation to double receive result
double(Term) ->
    Ref = make_ref(),
    double ! {self(), Ref, Term},
    receive
        {Ref, Result} -> 
            Result                 % Send Result to shell, flush() to see result
    after 5000 ->
        double(Term)               % Retry after 5000 ms
    end.