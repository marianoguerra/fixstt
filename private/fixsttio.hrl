-record(fixsttio, {handle=nil,
                   % file read position
                   position=0,
                   % file size
                   size,
                   % size of a record
                   record_size=512,
                   % the id of the last writen record or the value set at open
                   % if the file is empty
                   tail_id=0,
                   % the id of the first record on the file
                   head_id=0,
                   % id of the last read entry
                   last_id}).
