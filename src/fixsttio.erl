-module(fixsttio).
-export([open/1, open/2, append/2, read/3, close/1, iterate/3]).

-include("fixstt.hrl").
-include("private/fixsttio.hrl").

read(Io=#fixsttio{head_id=HeadId, tail_id=TailId}, FirstId, Count) ->
    if FirstId < HeadId orelse FirstId > TailId -> {error, outofbound};
       true ->
           case seek_to_id(Io, FirstId) of
               {ok, Io1} -> read_next_n(Io1, Count);
               Other -> Other
           end
    end.

open(Path) ->
    open(Path, []).

open(Path, Opts) ->
    RecordSize = proplists:get_value(record_size, Opts, 512),
    TailId = proplists:get_value(tail_id, Opts, 0),
    case file:open(Path, [raw, binary, read, append]) of
        {ok, Handle} ->
            Io = #fixsttio{handle=Handle, position=0, record_size=RecordSize, tail_id=TailId},
            init_state(Io);
        Other -> Other
    end.

append(Io=#fixsttio{handle=Handle, size=Size, head_id=HeadId, tail_id=TailId, record_size=RecordSize},
      FixStt=#fixstt{}) ->
    RecordId = TailId + 1,
    FixSttWithId = FixStt#fixstt{id=RecordId},
    Bin = fixstt:to_binary(FixSttWithId, RecordSize),
    case file:write(Handle, Bin) of
        ok ->
            NewSize = Size + size(Bin),
            NewHeadId = if HeadId == 0 -> RecordId;
                           true -> HeadId
                        end,
            {ok, Io#fixsttio{size=NewSize, head_id=NewHeadId, tail_id=RecordId}, RecordId};
        Other -> Other
    end.

close(Io=#fixsttio{handle=nil}) ->
    {ok, Io};
close(Io=#fixsttio{handle=Handle}) ->
    case file:close(Handle) of
        ok -> {ok, Io#fixsttio{handle=nil}};
        Other -> Other
    end.

% not called fold since the return value to control continue/stop is different
iterate(Io, Fun, Acc0) ->
    {ok, Io1} = seek(Io, bof),
    do_iterate(Io1, Fun, Acc0).

%% Private API

seek(Io=#fixsttio{handle=Handle}, Location) ->
    case file:position(Handle, Location) of
        {ok, NewPosition} -> {ok, Io#fixsttio{position=NewPosition}};
        Other -> Other
    end.

init_head_id(Io) ->
    case seek(Io, bof) of
        {ok, Io1} ->
            case read_next(Io1) of
                {ok, Io2, HeadRecord} ->
                    HeadId = HeadRecord#fixstt.id,
                    {ok, Io2#fixsttio{head_id=HeadId}};
                Other -> Other
            end;
        Other -> Other
    end.

init_tail_id(Io=#fixsttio{record_size=RecordSize}) ->
    case seek(Io, {eof, -RecordSize}) of
        {ok, Io1} ->
            case read_next(Io1) of
                {ok, Io2, TailRecord} ->
                    TailId = TailRecord#fixstt.id,
                    {ok, Io2#fixsttio{tail_id=TailId}};
                Other -> Other
            end;
        Other -> Other
    end.

init_state(Io=#fixsttio{handle=Handle, record_size=RecordSize}) ->
    % don't use seek here since we want to check some things
    case file:position(Handle, eof) of
        {ok, NewPosition} ->
            if NewPosition == 0 ->
                   {ok, Io#fixsttio{position=0, size=0}};
               (NewPosition rem RecordSize) /= 0 ->
                   {error, {size_not_multiple_of_record_size, NewPosition, RecordSize}};
               true ->
                   Io1 = Io#fixsttio{size=NewPosition, position=NewPosition},
                   case init_tail_id(Io1) of
                       {ok, NewIo} -> init_head_id(NewIo);
                       Other -> Other
                   end
            end;
        Other -> Other
    end.

read_next(Io=#fixsttio{handle=Handle, position=Position, record_size=RecordSize}) ->
    case file:read(Handle, RecordSize) of
        {ok, Data} ->
            FixStt = fixstt:from_binary(Data),
            RecordId = FixStt#fixstt.id,
            NewPosition = Position + size(Data),
            NewIo = Io#fixsttio{position=NewPosition, last_id=RecordId},
            {ok, NewIo, FixStt};
        Other -> Other
    end.

read_next_n(Io, Count) -> read_next_n(Io, Count, []).

read_next_n(Io, 0, Accum) ->
    {ok, Io, lists:reverse(Accum)};
read_next_n(Io, Count, Accum) ->
    case read_next(Io) of
        {ok, NewIo, Record} -> read_next_n(NewIo, Count - 1, [Record|Accum]);
        eof -> read_next_n(Io, 0, Accum);
        Other -> Other
    end.

% this assumes the Id is within range
seek_to_id(Io=#fixsttio{last_id=LastId}, Id) when LastId + 1 == Id ->
    {ok, Io};
seek_to_id(Io=#fixsttio{head_id=HeadId, record_size=RecordSize}, Id) ->
    IdOffsetFromHead = Id - HeadId,
    BytesOffsetFromHead = IdOffsetFromHead * RecordSize,
    seek(Io, {bof, BytesOffsetFromHead}).

do_iterate(Io, Fun, Acc0) ->
    case read_next(Io) of
        {ok, Io1, Entry} ->
            case Fun(Entry, Acc0) of
                {continue, Acc1} -> do_iterate(Io1, Fun, Acc1);
                {stop, Acc1} -> {ok, Acc1};
                {error, Reason} -> {error, Reason}
            end;
        eof -> {ok, Acc0};
        Other -> Other
    end.

