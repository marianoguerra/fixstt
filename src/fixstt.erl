-module(fixstt).
-export([to_binary/1, to_binary/2, from_binary/1, now_ms/0, format/1, get/2,
         set/3, new/1, new/3]).

-include("fixstt.hrl").

to_binary(FixStt) ->
    to_binary(FixStt, ?FIXSTT_DEFAULT_ENTRY_SIZE_BYTES).

to_binary(#fixstt{id=Id, lat=Lat, lng=Lng, date=Date, ref=Ref, type=Type, msg=Msg}, EntrySize) ->
    Len = size(Msg),
    pad_to_entry_size(<<Id:64/big-unsigned-integer,
                        Lat:64/big-float,
                        Lng:64/big-float,
                        Date:64/big-unsigned-integer,
                        Len:16/big-unsigned-integer,
                        Ref:64/big-unsigned-integer,
                        Type:16/big-unsigned-integer,
                        Msg/big-binary>>,
                      EntrySize).

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
    {Mega, Sec, Micro} = os:timestamp(),
    ((Mega * 1000000 + Sec) * 1000000 + Micro) div 1000.

get(#fixstt{id=Val}, id) -> Val;
get(#fixstt{lat=Val}, lat) -> Val;
get(#fixstt{lng=Val}, lng) -> Val;
get(#fixstt{date=Val}, date) -> Val;
get(#fixstt{ref=Val}, ref) -> Val;
get(#fixstt{type=Val}, type) -> Val;
get(#fixstt{msg=Val}, msg) -> Val.

set(Entry, id, Val) -> Entry#fixstt{id=Val};
set(Entry, lat, Val) -> Entry#fixstt{lat=Val};
set(Entry, lng, Val) -> Entry#fixstt{lng=Val};
set(Entry, date, Val) -> Entry#fixstt{date=Val};
set(Entry, ref, Val) -> Entry#fixstt{ref=Val};
set(Entry, type, Val) -> Entry#fixstt{type=Val};
set(Entry, msg, Val) -> Entry#fixstt{msg=Val}.

new(Msg) -> new(Msg, 9001, 9001).

new(Msg, Lat, Lng) ->
    #fixstt{msg=Msg, lat=Lat, lng=Lng, date=now_ms(), len=size(Msg)}.


pad_to_entry_size(Binary, EntrySize) ->
    case size(Binary) of
        EntrySize -> Binary;
        N when N < EntrySize ->
            PaddingSize = (EntrySize - N) * 8,
            <<Binary:N/binary, 0:PaddingSize>>
    end.
