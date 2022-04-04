-module(reader).

-export([read/1, parse_header/1]).

-record(header, {
    fin,
    rsv1,
    rsv2,
    rsv3,
    optcode,
    mask,
    payload_length,
    masking_key,
    data
}).

read(File) ->
    {ok, Bin} = file:read_file(File),
    parse_header(Bin).

parse_header(<<FIN:1, RSV1:1, RSV2:1, RSV3:1, OPTCODE:4, MASK:1, Lenght:7, REST/binary>>) ->
    parse_data(
        #header{
            fin = FIN,
            rsv1 = RSV1,
            rsv2 = RSV2,
            rsv3 = RSV3,
            optcode = OPTCODE,
            mask = MASK,
            payload_length = Lenght
        },
        mask_size(MASK),
        length_size(Lenght),
        REST
    ).

parse_data(H, MS, 0, Bin) ->
    L = H#header.payload_length,
    %    io:format("A: ~p~n", [H]),
    <<MaskingKey:MS/binary, Data:L/binary, _/binary>> = Bin,
    H#header{masking_key = MaskingKey, data = Data};
parse_data(H, MS, LS, Bin) ->
    %    io:format("B: ~p~n", [H]),
    <<L:LS, MaskingKey:MS/binary, Data:L/binary, _/binary>> = Bin,
    H#header{payload_length = L, masking_key = MaskingKey, data = Data}.

length_size(127) ->
    64;
length_size(126) ->
    16;
length_size(L) when L < 126 -> 0.

mask_size(1) -> 4;
mask_size(0) -> 0.
