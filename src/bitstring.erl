%%
%% Description: Crontab in Erlang.
%% Copyright (c) 2014 ShionRyuu <shionryuu@outlook.com>.
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%
-module(bitstring).
-author("ShionRyuu").

%% API
-export([
    set/2,
    unset/2,
    is_set/2,
    list_set/2,
    range_set/4
]).

%% set the bit
set(Bits, I) ->
    Bits bor (1 bsl I).

%% unset the bit
unset(Bits, I) ->
    Bits band (bnot (1 bsl I)).

%% is bit set
is_set(Bits, I) ->
    Bits band (1 bsl I) =/= 0.

list_set(Bits, List) when is_list(List) ->
    lists:foldl(fun(I, Acc) -> bitstring:set(Acc, I) end, Bits, List);
list_set(Bits, List) ->
    erlang:error(badarg, [Bits, List]).

range_set(Bits, Min, Max, Span) ->
    list_set(Bits, lists:seq(Min, Max, Span)).

%% ============================================================================
%% Tests
%% ============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
set_test() ->
    1 = set(0, 0), %% 0001
    8 = set(0, 3), %% 1000
    ok.

unset_test() ->
    1 = unset(3, 1), %% 0011
    3 = unset(7, 2), %% 0111
    ok.

list_set_test() ->
    255 = range_set(0, 0, 7, 1), %% 1111 1111
    11 = list_set(0, [0, 1, 3]), %% 1011
    ok.

is_set_test() ->
    Bits = range_set(0, 0, 59, 1), %%
    [is_set(Bits, I) || I <- lists:seq(0, 59, 1)],
    ok.

-else.
-endif.
