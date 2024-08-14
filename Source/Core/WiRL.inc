{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2023 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}

{$IF CompilerVersion >= 27} // Delphi XE6
  {$DEFINE HAS_SYSTEM_JSON}
  // https://quality.embarcadero.com/browse/RSP-11620
  {$DEFINE CUSTOM_ATTRIBUTE_BUG}
{$ENDIF}

{$IF CompilerVersion >= 28} // Delphi XE7
  {$DEFINE HAS_NEW_ARRAY}
  {$DEFINE HAS_NET_ENCODING}
  {$DEFINE HAS_SYSTEM_THREADING}
{$ENDIF}

{$IF CompilerVersion >= 29} // Delphi XE8
  {$DEFINE HAS_NETHTTP_CLIENT}
{$ENDIF}

{$IF CompilerVersion >= 30} // Delphi 10.0 Seattle
  {$DEFINE HAS_HMAC_HASH}
  {$DEFINE HAS_GENERIC_CREATE}
{$ENDIF}

{$IF CompilerVersion >= 31} // Delphi 10.1 Berlin
  {$DEFINE HAS_UTF8CHAR}
  {$DEFINE NO_ARRAY_HELPER_BUG}
{$ENDIF}

{$IF CompilerVersion >= 33} // Delphi 10.3 Rio
  {$DEFINE HAS_NEW_PIDS}
  {$DEFINE HAS_NEW_JSON}
  {$DEFINE HAS_MANAGED_RECORD}
{$ENDIF}

{$IF CompilerVersion >= 34} // Delphi 10.4 Sydney
  {$DEFINE HAS_FORM_DATA_FIELD_WITH_MEDIA_TYPE}
{$ENDIF}

{$IF CompilerVersion >= 35} // Delphi 11.0 Alexandria
  {$DEFINE HAS_NEW_ANDROID_PID}
  {$DEFINE HAS_NO_REF_COUNT}      // TNoRefCountObject class
  {$UNDEF CUSTOM_ATTRIBUTE_BUG}
{$ENDIF}

{$IF CompilerVersion >= 36} // Delphi 12.0 Athens
  {$DEFINE HAS_ARRAY_TO_STRING}
{$ENDIF}

