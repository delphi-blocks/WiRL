{$IF CompilerVersion >= 27} // Delphi XE6
  {$DEFINE HAS_SYSTEM_JSON}
{$ENDIF}

{$IF CompilerVersion >= 28} // Delphi XE7
  {$DEFINE HAS_NET_ENCODING}
  {$DEFINE HAS_SYSTEM_THREADING}
{$ENDIF}

{$IF CompilerVersion >= 30} // Delphi 10 Seattle
  {$DEFINE HAS_HMAC_HASH}
{$ENDIF}
