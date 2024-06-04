```erlang
%% [作者] ： 郑子强 <johnmelodymel@qq.com>
%% [项目] ： 鲁恩算法

-module(luhn).
-author("Cheng Tze Keong").

-import(lists, [len/1, chr/2, str/2,  fac/1, persistent_term/2, droplast/1, foldr/3, last/1]).
-export([check_luhn/1, for/2, start/1, log/1, valid/1, create/1]).

-export_type([digit/0, valid_char/0]).
-compile(export_all).

%% 鲁恩算法，也称模10算法，是一种简单的校验和公式，用于验证各种身份证号码，如信用卡号，IMEI号码等。

%% 测试
% -include_lib("include/proper.hrl").

%% 类型
%% @type digit(). 数字是一个单一的十进制数字。
-type digit() :: 0..9.

%% @type valid_char(). 有效字符是`"0123456789 "'中的一个。
-type valid_char() :: 48..57 | 32.

%% @type parity(). 奇偶性是`even'或`odd'。
-type parity() :: even | odd.

%% 检查卡号
check_luhn(卡号) ->
    Fn = fun() ->
        io:fwrite("卡号: ~p~n", [卡号]),
        Len = len(卡号),
        % persistent_term:put(IS_SECOND, false),
        log(Len) end,
    Fn().

%% @doc 给定一个`字符串'，如果它代表一个有效的号码，则返回`true'，根据鲁恩公式，否则返回`false'。
-spec valid(字符串::string()) -> Valid::boolean().
valid(S) -> 0 =:= rem10(do_luhn(S, odd)).

%% @doc 给定一个`字符串'，计算其校验位并将其附加到`字符串'。
-spec create(字符串::string()) -> Result::string().
create(S) -> S ++ [$0 + rem10(10 - do_luhn(S, even))].

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @doc 等价于{@link check_digit/1}`(P, '{@link do_luhn/3}`(S, 0, 0)'。
-spec do_luhn(字符串::string(), 奇偶性::parity()) -> CheckDigit::digit().
do_luhn(S, P) -> check_digit(P, do_luhn(S, 0, 0)).

%% @doc 给定一个`字符串'，`OddAcc'和`EvenAcc'，返回两种`odd'和`even'奇偶性的校验位，作为元组。
%% @see check_digit/1
%% @see do_luhn/2
-spec do_luhn(字符串, 奇数累加, 偶数累加) -> {OddDigit,EvenDigit} when
    字符串    :: string(),
    OddAcc    :: non_neg_integer(),
    EvenAcc   :: non_neg_integer(),
    OddDigit  :: digit(),
    EvenDigit :: digit().
do_luhn([C|Cs], O, E) when $0 =< C, C =< $9 ->
  C0 = C - $0,
  F  = fun (P) -> (fun (X) when X > 9 -> X - 9; (X) -> X end)(parity(P)*C0) end,
  do_luhn(Cs, E+F(odd), O+F(even));
do_luhn([_|Cs], O, E) -> do_luhn(Cs, O, E);
do_luhn([], O, E)     -> {O,E}.

%% @doc 返回给定{@link partity/0. parity} `P'的数值，即对于`奇数'为`1'，对于`偶数'为`2'。
-spec parity(P::parity()) -> K::1 | 2.
parity(奇数)  -> 1;
parity(偶数) -> 2.

%% @doc 给定`奇偶性'和`{OddAcc,EvenAcc}=OddEvenTuple'，返回适当的`CheckDigit'。
%% @see do_luhn/3
-spec check_digit(奇偶性, 奇数偶数元组) -> CheckDigit when
    奇偶性       :: parity(),
    OddEvenTuple :: {non_neg_integer(),non_neg_integer()},
    CheckDigit   :: digit().
check_digit(P, OE) -> rem10(element(parity(P), OE)).

%% @doc 返回`X rem 10'。
-spec rem10(non_neg_integer()) -> digit().
rem10(X) -> X rem 10.

% for_loop(I, N, _) when I - 1 >= 0 -> 
%     if persistent_term:get(IS_SECOND) = false ->
%         io:write("c").

for(0, _) ->
    []; 
    for (N, Term) when N - 1 >= 0 ->
        
        [Term|for(N-1, Term)].

log(消息) ->
    io:fwrite("卡号长度: ~p~n", [消息]).

%% 运行初始化匿名函数
start(CARD) ->
    Fn = fun() ->
        io:fwrite("鲁恩算法 \n"),
        check_luhn(CARD) end,
    Fn().
%%% 类型

%% @doc 一个{@link digit(). digit}s列表的PropEr类型生成器，
%% 使得`length(List) >= 3'和`hd(List) > 0'。
%% @see pos_digit/0
