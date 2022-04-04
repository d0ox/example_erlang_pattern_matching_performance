-module(helpers).

-export([init/0, test_header/1, make_lenght/1, performance/2, read_performance/1]).

-define(TEST_FILE, "example.bin").
-define(TEST_DATA, <<"This is testing string as payload data! have fun!">>).

init() ->
    {ok, F} = file:open(?TEST_FILE, [write]),
    file:write(F, test_header(?TEST_DATA)).

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
