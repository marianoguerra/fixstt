fixstt
======

library to write/read fixed length status messages.

this library is developed to be used for `flaviodb <https://github.com/marianoguerra/flaviodb>`_
but can be used for any other purpose, flexibility isn't its strength :)

description
-----------

a fixstt entry contains the following fields:

* id:  64 bit integer, unique increasing id that identifies this message in this stream
* lat: 64 bit float, optional latitude  (double) set it to 9001 when no data
* lng: 64 bit float, optional longitude (double) set it to 9001 when no data
* date: 64 bit integer UTC unix timestamp in milliseconds when the record was created
* len: 16 bit integer, contains the length of the msg without padding
* ref: 64 bit integer, user defined meaning, for example can be used to store an id to extra data, or an offset in another file where extra data is stored
* type: 16 bit integer, can be used to define the msg type, payload content type etc
* msg: zero padded payload to fill the total msg size (default 512 bytes)

this means the header is 44 bytes long, in erlang syntax:: 

    <<Id:64/big-unsigned-integer,
    Lat:64/big-float,
    Lng:64/big-float,
    Date:64/big-unsigned-integer,
    Len:16/big-unsigned-integer,
    Ref:64/big-unsigned-integer,
    Type:16/big-unsigned-integer,
    Msg:Len/big-binary,
    _Padding/bitstring>>
