{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Console.Posix.Syslog;

interface

uses
  System.SysUtils,
  Posix.Base;

const
  _PATH_LOG = '/dev/log';

  LOG_EMERG   = 0;
  LOG_ALERT   = 1;
  LOG_CRIT    = 2;
  LOG_ERR     = 3;
  LOG_WARNING = 4;
  LOG_NOTICE  = 5;
  LOG_INFO    = 6;
  LOG_DEBUG   = 7;
  LOG_PRIMASK = $07;

  LOG_KERN      = 0 shl 3;
  LOG_USER      = 1 shl 3;
  LOG_MAIL      = 2 shl 3;
  LOG_DAEMON    = 3 shl 3;
  LOG_AUTH      = 4 shl 3;
  LOG_SYSLOG    = 5 shl 3;
  LOG_LPR       = 6 shl 3;
  LOG_NEWS      = 7 shl 3;
  LOG_UUCP      = 8 shl 3;
  LOG_CRON      = 9 shl 3;
  LOG_AUTHPRIV  = 10 shl 3;
  LOG_FTP       = 11 shl 3;
  LOG_LOCAL0    = 16 shl 3;
  LOG_LOCAL1    = 17 shl 3;
  LOG_LOCAL2    = 18 shl 3;
  LOG_LOCAL3    = 19 shl 3;
  LOG_LOCAL4    = 20 shl 3;
  LOG_LOCAL5    = 21 shl 3;
  LOG_LOCAL6    = 22 shl 3;
  LOG_LOCAL7    = 23 shl 3;

  LOG_NFACILITIES = 24;
  LOG_FACMASK     = $03f8;
  INTERNAL_NOPRI  = $10;
  INTERNAL_MARK   = LOG_NFACILITIES shl 3;

 const
  LOG_PID    = $01;
  LOG_CONS   = $02;
  LOG_ODELAY = $04;
  LOG_NDELAY = $08;
  LOG_NOWAIT = $10;
  LOG_PERROR = $20;

procedure closelog; cdecl;
  external libc name _PU + 'closelog';

procedure openlog(__ident: MarshaledAString; __option: LongInt; __facilit: Longint); cdecl;
  external libc name _PU + 'openlog';

function setlogmask(__mask: Longint): Longint; cdecl;
  external libc name _PU + 'setlogmask';

procedure _syslog(__pri: Longint; __fmt: MarshaledAString; args: array of const); cdecl;
  external libc name _PU + 'syslog';

procedure syslog(__pri: Longint; __fmt: string); overload;

procedure syslog(__pri: Longint; __fmt: string; args: array of const); overload;

implementation

procedure syslog(__pri: Longint; __fmt: string); overload;
var
  LMarshaller: TMarshaller;
begin
  _syslog(__pri, MarshaledAString(LMarshaller.AsAnsi(__fmt, CP_UTF8).ToPointer), []);
end;

procedure syslog(__pri: Longint; __fmt: string; args: array of const); overload;
begin
  syslog(__pri, Format(__fmt, args));
end;

end.
