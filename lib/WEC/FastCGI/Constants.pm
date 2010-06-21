package WEC::FastCGI::Constants;
use 5.008;
use warnings;
use strict;

use Exporter::Tidy
    RecordTypes		=> [qw(BEGIN_REQUEST ABORT_REQUEST END_REQUEST
                               PARAMS_STREAM STDIN_STREAM STDOUT_STREAM
                               STDERR_STREAM DATA_STREAM
                               GET_VALUES GET_VALUES_RESULT
                               UNKNOWN_TYPE)],

    ProtocolStatus	=> [qw(REQUEST_COMPLETE CANT_MPX_CONN OVERLOADED
                               UNKNOWN_ROLE LOST_CONNECTION)],
    Roles		=> [qw(RESPONDER AUTHORIZER FILTER)],
    Flags		=> [qw(KEEP_CONN)],
    other		=> [qw(VERSION MAX_REQUEST PRE_RECORD)];

use constant {
    VERSION=> 1,
    MAX_REQUEST	=> 65535,
    # Number of bytes needed to determine full record length
    PRE_RECORD	=> 8,

    # Message types
    BEGIN_REQUEST	=>  1,
    ABORT_REQUEST	=>  2,
    END_REQUEST		=>  3,
    PARAMS_STREAM	=>  4,
    STDIN_STREAM	=>  5,
    STDOUT_STREAM	=>  6,
    STDERR_STREAM	=>  7,
    DATA_STREAM		=>  8,
    GET_VALUES		=>  9,
    GET_VALUES_RESULT	=> 10,
    UNKNOWN_TYPE	=> 11,

    # protocolStatus values
    REQUEST_COMPLETE	=> 0,
    CANT_MPX_CONN	=> 1,
    OVERLOADED		=> 2,
    UNKNOWN_ROLE	=> 3,
    # This one is non-standard, WEC::Client specific
    LOST_CONNECTION	=> 254,

    # Role values
    RESPONDER	=> 1,
    AUTHORIZER	=> 2,
    FILTER	=> 3,

    # Mask for flags component of FCGI_BeginRequestBody
    KEEP_CONN  => 1,
};

1;
