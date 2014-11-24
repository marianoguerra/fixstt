-module(fixstt).
-export([to_binary/1, from_binary/1, now_ms/0, format/1]).

-include("fixstt.hrl").

to_binary(#fixstt{id=Id, lat=Lat, lng=Lng, date=Date, ref=Ref, type=Type, msg=Msg}) ->
    Len = size(Msg),
    pad_to_entry_size(<<Id:64/big-unsigned-integer,
                        Lat:64/big-float,
                        Lng:64/big-float,
                        Date:64/big-unsigned-integer,
                        Len:16/big-unsigned-integer,
                        Ref:64/big-unsigned-integer,
                        Type:16/big-unsigned-integer,
                        Msg/big-binary>>).

from_binary(<<Id:64/big-unsigned-integer,
              Lat:64/big-float,
              Lng:64/big-float,
              Date:64/big-unsigned-integer,
              Len:16/big-unsigned-integer,
              Ref:64/big-unsigned-integer,
              Type:16/big-unsigned-integer,
              Msg:Len/big-binary,
              _Padding/bitstring>>) ->
    #fixstt{id=Id, lat=Lat, lng=Lng, date=Date, len=Len, ref=Ref, type=Type, msg=Msg}.

format(#fixstt{id=Id, lat=Lat, lng=Lng, date=Date, len=Len, ref=Ref, type=Type, msg=Msg}) ->
    io_lib:format("#fixstt{id=~p, lat=~p, lng=~p, date=~p, len=~p, ref=~p, type=~p, msg=~p}",
                  [Id, Lat, Lng, Date, Len, Ref, Type, Msg]).


now_ms() ->
    {Mega, Sec, Micro} = erlang:now(),
    ((Mega * 1000000 + Sec) * 1000000 + Micro) div 1000.

pad_to_entry_size(Binary) ->
    case size(Binary) of
        ?FIXSTT_ENTRY_SIZE_BYTES -> Binary;
        N when N < ?FIXSTT_ENTRY_SIZE_BYTES ->
            PaddingSize = (?FIXSTT_ENTRY_SIZE_BYTES - N) * 8,
            <<Binary:N/binary, 0:PaddingSize>>
    end.
