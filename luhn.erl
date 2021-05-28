%% [author] : John Melody Me <Johnmelody@dingtalk.com>
%% [project] : Luhn Algorithm 

-module(luhn).
-import(string, [len/1, chr/2, str/2,  fac/1]).
-export([check_luhn/1, for/2, start/1, log/1]).

%% Luhn Algorithm, the modulus 10 or mod 10 algorithm, 
%% is a simple checksum formula used to validate a variety 
%% of identification numbers, such as credit card numbers, 
%% IMEI numbers, Canadian Social Insurance Numbers.

%% Inspection Card Number
check_luhn(CARD_NUMBER) ->
    Fn = fun() ->
        io:fwrite("Card Number: ~p~n", [CARD_NUMBER]),
        Len = len(CARD_NUMBER),
        log(Len),
        for(Len - 1, 1) end,
    Fn().

for(0, _) ->
    []; 
    for (N, Term) when N >= 0 ->
        
        [Term|for(N-1, Term)].

log(MSG) ->
    io:fwrite("The Length Of The Card Number: ~p~n", [MSG]).

%% Run Init Anonymous Function 
start(CARD) ->
    Fn = fun() ->
        io:fwrite("Luhn Algorithm \n"),
        check_luhn(CARD) end,
    Fn().
