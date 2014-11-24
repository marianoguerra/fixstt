-module(fixstt_SUITE).
-compile(export_all).

-include("fixstt.hrl").

all() -> [binary_rountrip].

binary_rountrip(_) ->
    Id = 42,
    Lat = 12.34,
    Lng = 14.15,
    Date = fixstt:now_ms(),
    Ref = 57,
    Type = 92,
    Msg = <<"Hello World!!">>,
    Record = #fixstt{id=Id, lat=Lat, lng=Lng, date=Date, ref=Ref, type=Type, msg=Msg},
    FixStt = fixstt:to_binary(Record),
    512 = size(FixStt),
    RecordBack = fixstt:from_binary(FixStt),
    ct:print("~s = ~s?", [fixstt:format(Record), fixstt:format(RecordBack)]),
    RecordBack = Record#fixstt{len=size(Msg)}.

