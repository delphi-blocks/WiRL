{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Filters.Attributes;

interface

uses
  System.SysUtils, System.Classes,
  WiRL.Core.Attributes;

type
  [NameBinding]
  PoweredByWiRLAttribute = class(TCustomAttribute);

  [NameBinding]
  ContentEncodingAttribute = class(TCustomAttribute);

  //  PoweredByWiRLAttribute = class(NameBindingAttribute);

implementation

end.
