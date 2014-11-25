-module(fixstt_SUITE).
-compile(export_all).

-include("fixstt.hrl").
-include("private/fixsttio.hrl").

-include_lib("common_test/include/ct.hrl").

all() -> [binary_rountrip, initial_state_is_correct, can_write_one, can_write_two,
         write_1_read_first, write_1_read_many,
         error_opening_200byte_file, error_opening_700byte_file,
         open_existing_file_1_entry, open_existing_file_2_entries,
         iterate_empty, iterate_1, iterate_2, iterate_2_stop_first].

init_per_testcase(TestName, Config) ->
    FileName = atom_to_list(TestName),
    DataDir = ?config(priv_dir, Config),
    Path = filename:join([DataDir, FileName]),
    {ok, Io} = fixsttio:open(Path),
    [{fixsttio, Io}|Config].

end_per_testcase(_TestName, Config) ->
    Io = ?config(fixsttio, Config),
    {ok, _} = fixsttio:close(Io),
    ok.

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
    RecordBack = Record#fixstt{len=size(Msg)}.

error_opening_200byte_file(Config) ->
    FileName = "200bytes",
    DataDir = ?config(data_dir, Config),
    Path = filename:join([DataDir, FileName]),
    {error, {size_not_multiple_of_record_size, 200, 512}} = fixsttio:open(Path).

error_opening_700byte_file(Config) ->
    FileName = "700bytes",
    DataDir = ?config(data_dir, Config),
    Path = filename:join([DataDir, FileName]),
    {error, {size_not_multiple_of_record_size, 700, 512}} = fixsttio:open(Path).

open_existing_file_1_entry(Config) ->
    FileName = "one_entry",
    DataDir = ?config(data_dir, Config),
    Path = filename:join([DataDir, FileName]),
    {ok, Io} = fixsttio:open(Path),
    1 = Io#fixsttio.head_id,
    1 = Io#fixsttio.tail_id,
    512 = Io#fixsttio.size,
    512 = Io#fixsttio.position,
    1 = Io#fixsttio.last_id,
    512 = Io#fixsttio.record_size.

open_existing_file_2_entries(Config) ->
    FileName = "two_entries",
    DataDir = ?config(data_dir, Config),
    Path = filename:join([DataDir, FileName]),
    {ok, Io} = fixsttio:open(Path),
    1 = Io#fixsttio.head_id,
    2 = Io#fixsttio.tail_id,
    1024 = Io#fixsttio.size,
    512 = Io#fixsttio.position,
    1 = Io#fixsttio.last_id,
    512 = Io#fixsttio.record_size.

initial_state_is_correct(Config) ->
    Io = ?config(fixsttio, Config),
    0 = Io#fixsttio.head_id,
    0 = Io#fixsttio.tail_id,
    0 = Io#fixsttio.size,
    0 = Io#fixsttio.position,
    undefined = Io#fixsttio.last_id,
    512 = Io#fixsttio.record_size.

sample_status() ->
    Now = fixstt:now_ms(), Msg = <<"hi there...">>, Ref = 12, Type = 9,
    Lat = 12.34, Lng = 43.21,
    #fixstt{lat=Lat, lng=Lng, date=Now, ref=Ref, type=Type, msg=Msg}.

write_1(Io) ->
    fixsttio:append(Io, sample_status()).

can_write_one(Config) ->
    Io = ?config(fixsttio, Config),
    {ok, Io1, Id} = write_1(Io),
    1 = Id,
    1 = Io1#fixsttio.head_id,
    1 = Io1#fixsttio.tail_id,
    512 = Io1#fixsttio.size,
    0 = Io1#fixsttio.position,
    undefined = Io1#fixsttio.last_id,
    512 = Io1#fixsttio.record_size.

can_write_two(Config) ->
    Io = ?config(fixsttio, Config),
    {ok, Io1, Id1} = write_1(Io),
    {ok, Io2, Id2} = write_1(Io1),
    1 = Id1,
    2 = Id2,
    1 = Io2#fixsttio.head_id,
    2 = Io2#fixsttio.tail_id,
    1024 = Io2#fixsttio.size,
    0 = Io2#fixsttio.position,
    undefined = Io2#fixsttio.last_id,
    512 = Io2#fixsttio.record_size.

write_1_read_n(Config, N) ->
    Io = ?config(fixsttio, Config),
    R = sample_status(),
    {ok, Io1, Id1} = fixsttio:append(Io, R),
    {ok, Io2, [R1]} = fixsttio:read(Io1, Id1, N),
    Id1 = R1#fixstt.id,
    true = R#fixstt.lat == R1#fixstt.lat,
    true = R#fixstt.lng == R1#fixstt.lng,
    true = R#fixstt.msg == R1#fixstt.msg,
    true = R#fixstt.ref == R1#fixstt.ref,
    true = R#fixstt.type == R1#fixstt.type,
    true = R#fixstt.date == R1#fixstt.date,
    1 = Id1,
    1 = Io2#fixsttio.head_id,
    1 = Io2#fixsttio.tail_id,
    512 = Io2#fixsttio.size,
    512 = Io2#fixsttio.position,
    1 = Io2#fixsttio.last_id,
    512 = Io2#fixsttio.record_size.

write_1_read_first(Config) ->
    write_1_read_n(Config, 1).

write_1_read_many(Config) ->
    write_1_read_n(Config, 10).

iterate_empty(Config) ->
    Io = ?config(fixsttio, Config),
    {ok, 42} = fixsttio:iterate(Io, fun (Entry, 42) ->
                                            Entry = notreachable
                                    end, 42).

iterate_1(Config) ->
    Io = ?config(fixsttio, Config),
    R = sample_status(),
    {ok, Io1, Id1} = fixsttio:append(Io, R),
    RId = fixstt:set(fixstt:set(R, id, Id1), len, 11),
    {ok, RId} = fixsttio:iterate(Io1, fun (Entry, _Accum) ->
                                             {continue, Entry}
                                    end, []).

iterate_2(Config) ->
    Io = ?config(fixsttio, Config),
    R = sample_status(),
    {ok, Io1, Id1} = fixsttio:append(Io, R),
    {ok, Io2, Id2} = fixsttio:append(Io1, R),
    R1Id = fixstt:set(fixstt:set(R, id, Id1), len, 11),
    R2Id = fixstt:set(fixstt:set(R, id, Id2), len, 11),
    {ok, [R2Id, R1Id]} = fixsttio:iterate(Io2, fun (Entry, Accum) ->
                                             {continue, [Entry|Accum]}
                                    end, []).

iterate_2_stop_first(Config) ->
    Io = ?config(fixsttio, Config),
    R = sample_status(),
    {ok, Io1, Id1} = fixsttio:append(Io, R),
    {ok, Io2, _Id2} = fixsttio:append(Io1, R),
    R1Id = fixstt:set(fixstt:set(R, id, Id1), len, 11),
    {ok, R1Id} = fixsttio:iterate(Io2, fun (Entry, 42) ->
                                             {stop, Entry}
                                    end, 42).

