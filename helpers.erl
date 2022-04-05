-module(helpers).

-export([init/0, test_header/1, make_lenght/1, performance/2, read_performance/1]).

-define(TEST_FILE, "example.bin").
-define(TEST_FILE1, "example1.bin").
-define(TEST_DATA, <<"This is testing string as payload data! have fun!">>).
-define(TEST_DATA1,
    <<"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.">>
).

init() ->
    {ok, F} = file:open(?TEST_FILE, [write]),
    file:write(F, test_header(?TEST_DATA)),
    {ok, F1} = file:open(?TEST_FILE1, [write]),
    file:write(F1, test_header(?TEST_DATA1)).

test_header(Data) ->
    {Lenght, ExtendedLenght} = make_lenght(size(Data)),
    MaskingKey = <<"abcd">>,
    <<1:1, 0:3, 1:4, 1:1, Lenght:7, ExtendedLenght/binary, MaskingKey/binary, Data/binary>>.

make_lenght(Size) when Size < 126 ->
    {Size, <<>>};
make_lenght(Size) when Size < 65535 ->
    {126, <<Size:16>>};
make_lenght(Size) when Size < 18446744073709551615 ->
    {127, <<Size:64>>}.

calc_loop(_Fun, 0) ->
    ok;
calc_loop(Fun, N) ->
    Fun(),
    calc_loop(Fun, N - 1).

performance(Fun, N) ->
    %%in microseconds
    {Time, _} = timer:tc(fun() -> calc_loop(Fun, N) end),
    Time / N.

read_performance(File) ->
    {ok, Bin} = file:read_file(File),
    performance(fun() -> reader:parse_header(Bin) end, 1000).
