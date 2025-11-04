{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2025 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit PetStore.Resources.OpenAPI;

interface

uses
  System.Classes, System.SysUtils,

  WiRL.Core.OpenAPI.Resource,
  WiRL.Core.Registry,
  WiRL.Core.Attributes;

type
  // 1. You must inherit a resource from the base class TOpenAPIResourceCustom
  // 2. You must decide the path of this new resource
  [Path('openapi')]
  TDocumentationResource = class(TOpenAPIResourceCustom);

implementation

end.
