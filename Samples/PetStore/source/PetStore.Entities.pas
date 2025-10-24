{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2025 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit PetStore.Entities;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.Types,

  Neon.Core.Types,
  Neon.Core.Attributes,
  Neon.Core.Persistence.JSON.Schema;

type
  TOrderStatus = (placed, approved, delivered);

  [JsonSchema('title=Order')]
  TOrder = record
    Id: Int64;
    PetId: Int64;
    Quantity: Integer;
    ShipDate: TDateTime;
    Status: TOrderStatus;
    Complete: Boolean;
  end;

  [JsonSchema('title=Category')]
  TCategory = record
    Id: Int64;
    Name: string;
  end;

  [JsonSchema('title=user')]
  TUser = record
    Id: Int64;
    Username: string;
    FirstName: string;
    LastName: string;
    Email: string;
    Password: string;
    Phone: string;
    UserStatus: Integer;
  end;

  [JsonSchema('title=Tag')]
  TTag = record
    Id: Int64;
    Name: string;
  end;

  TPetStatus = (available, pending, sold);

  [JsonSchema('title=Pet')]
  TPet = record
    Id: Int64;
    Name: string;
    Category: TCategory;
    PhotoUrls: TArray<string>;
    Tags: TArray<TTag>;
    Status: TPetStatus;
  end;

  TPetArray = TArray<TPet>;

  [JsonSchema('title=ApiResponse')]
  TApiResponse = record
    Code: Integer;
    &Type: string;
    Message: string;
  end;


implementation

end.
