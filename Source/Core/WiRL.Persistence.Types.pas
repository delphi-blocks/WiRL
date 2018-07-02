{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2018 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Persistence.Types;

interface

uses
  System.Classes, System.SysUtils, System.TypInfo;

{$SCOPEDENUMS ON}

type
  ENeonException = class(Exception);

type
  TNeonCase = (LowerCase, UpperCase, PascalCase, CamelCase, SnakeCase, CustomCase);
  TNeonMemberType = (Unknown, Prop, Field, Indexed);
  TNeonMembers = (Unknown, Standard, Fields, Properties);
  TNeonVisibility = set of TMemberVisibility;

type
  TCaseFunc = reference to function (const AString: string): string;

implementation

end.
