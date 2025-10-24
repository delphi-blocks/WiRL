{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2025 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit PetStore.Resources;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.SysUtils, System.JSON, System.Rtti,

  WiRL.Core.OpenAPI.Resource,
  WiRL.Engine.REST,
  WiRL.Core.Application,
  WiRL.Core.Registry,
  WiRL.Core.Exceptions,
  WiRL.Core.Attributes,
  WiRL.Core.Application.Worker,
  WiRL.Core.MessageBody.Default,
  WiRL.Core.Auth.Context,
  WiRL.Core.Auth.Resource,
  WiRL.http.Accept.MediaType,
  WiRL.http.URL,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.Core.Context,
  WiRL.Core.Context.Server,
  WiRL.Core.Injection,

  PetStore.Entities;

type
  TMyClass = class
  private
    FValue: Integer;
    FInfo: string;
  public
    property Value: Integer read FValue write FValue;
    property Info: string read FInfo write FInfo;
  end;

  /// <summary>
  ///   Everything about your Pets
  /// </summary>
  [Path('/pet')]
  TPetResource = class(TObject)
  public

    /// <summary>
    ///   Finds Pets by tags
    /// </summary>
    /// <param name="ATags" required="true">
    ///   Tags to filter by
    /// </param>
    /// <method id="findPetsByTags">
    ///   Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
    /// </method>
    /// <response code="200" name="OK">
    ///   Successful operation
    /// </response>
    /// <response code="400" name="BadRequest" error="Error">
    ///   Bad Request
    /// </response>
    [GET, Path('/findByTags'), Produces(TMediaType.APPLICATION_JSON)]
    function GetPetByTags([QueryParam('tags')] ATags: TArray<string>): TPetArray;

    /// <summary>
    ///   Finds Pets by status
    /// </summary>
    /// <param name="AStatus" required="true">
    ///   Status values that need to be considered for filter
    /// </param>
    /// <method id="findPetsByStatus">
    ///   Multiple status values can be provided with comma separated strings.
    /// </method>
    /// <response code="200" name="OK">
    ///   Successful operation
    /// </response>
    /// <response code="400" name="BadRequest" error="Error">
    ///   Bad Request
    /// </response>
    [GET, Path('/findByStatus'), Produces(TMediaType.APPLICATION_JSON)]
    function GetPetByStatus([QueryParam('status')] AStatus: TPetStatus): TArray<TPet>;

    /// <summary>
    ///   Find pet by ID
    /// </summary>
    /// <param name="APetID" required="false">
    ///   ID of pet to return
    /// </param>
    /// <method id="getPetById">
    ///   Returns a single pet
    /// </method>
    /// <response code="200" name="OK">
    ///   Successful operation
    /// </response>
    /// <response code="400" name="BadRequest" error="Error">
    ///   Bad Request
    /// </response>
    [GET, Path('{petId}'), Produces(TMediaType.APPLICATION_JSON)]
    function GetPetById([PathParam('petId')] APetID: Int64): TPet;

    /// <summary>
    ///   Updates a pet in the store with form data.
    /// </summary>
    /// <param name="APetID">
    ///   ID of pet that needs to be updated
    /// </param>
    /// <param name="AName">
    ///   Name of pet that needs to be updated
    /// </param>
    /// <param name="AStatus">
    ///   Status of pet that needs to be updated
    /// </param>
    /// <method id="updatePetWithForm">
    ///   Updates a pet resource based on the form data.
    /// </method>
    /// <response code="200" name="OK">
    ///   Successful operation
    /// </response>
    /// <response code="400" name="BadRequest" error="Error">
    ///   Bad Request
    /// </response>
    [POST, Path('{petId}'), Produces(TMediaType.APPLICATION_JSON)]
    function UpdatePetForm(
      [PathParam('petId')] APetID: Int64;
      [QueryParam('name')] const AName: string;
      [QueryParam('status')] AStatus: TPetStatus
    ): TPet;

    /// <summary>
    ///   Upload an image.
    /// </summary>
    /// <param name="APetID">
    ///   ID of pet that needs to be updated
    /// </param>
    /// <param name="AImage">
    ///   Image in binary format
    /// </param>
    /// <param name="AData">
    ///   Additional Metadata
    /// </param>
    /// <method id="uploadFile">
    ///   Upload an image of the pet.
    /// </method>
    /// <response code="200" name="OK">
    ///   Successful operation
    /// </response>
    /// <response code="400" name="BadRequest" error="Error">
    ///   Bad Request
    /// </response>
    [POST, Path('{petId}/uploadImage'), Produces(TMediaType.APPLICATION_JSON), Consumes(TMediaType.APPLICATION_OCTET_STREAM)]
    function UploadImage(
      [PathParam('petId')] APetID: Int64;
      [QueryParam('additionalMetadata')] const AData: string;
      [BodyParam] AImage: TStream
    ): TApiResponse;

    /// <summary>
    ///   Add a new pet to the store
    /// </summary>
    /// <param name="APet">
    ///   Create a new pet
    /// </param>
    /// <method id="addPet">
    ///   Add a new **pet** to the store...
    /// </method>
    /// <response code="200" name="OK">
    ///   Successful operation
    /// </response>
    /// <response code="400" name="BadRequest" error="Error">
    ///   Bad Request
    /// </response>
    [POST, Produces(TMediaType.APPLICATION_JSON), Consumes(TMediaType.APPLICATION_JSON)]
    function InsertPet([BodyParam] APet: TPet): TPet;

    /// <summary>
    ///   Update an existing pet
    /// </summary>
    /// <param name="APet">
    ///   Update an existent pet in the store
    /// </param>
    /// <method id="updatePet">
    ///   Update an existing pet by Id
    /// </method>
    /// <response code="200" name="OK">
    ///   Successful operation
    /// </response>
    /// <response code="400" name="BadRequest" error="Error">
    ///   Bad Request
    /// </response>
    [PUT, Produces(TMediaType.APPLICATION_JSON), Consumes(TMediaType.APPLICATION_JSON)]
    function UpdatePet([BodyParam] APet: TPet): TPet;


    /// <summary>
    ///   Deletes a pet
    /// </summary>
    /// <param name="APetId">
    ///   Update an existent pet in the store
    /// </param>
    /// <method id="deletePet">
    ///   Delete a pet
    /// </method>
    /// <response code="404" name="Not Found" error="Error">
    ///   Pet not found
    /// </response>
    [DELETE, Path('{petId}')]
    procedure DeletePet([PathParam('petId')] APetId: Integer);
  end;


  [Path('/store')]
  TStoreResource = class

    /// <summary>
    ///   Find purchase order by ID.
    /// </summary>
    /// <param name="AOrderID">
    ///   ID of order that needs to be fetched
    /// </param>
    /// <method id="getOrderById">
    ///   For valid response try integer IDs with value <= 5 or > 10. Other values will generate exceptions.
    /// </method>
    /// <response code="404" name="Not Found" error="Error">
    ///   Order not found
    /// </response>
    [GET, Path('order/{orderId}'), Produces(TMediaType.APPLICATION_JSON)]
    function GetOrderById([PathParam('orderId')] AOrderID: Int64): TOrder;
  end;

  /// <summary>
  ///   Operations about user
  /// </summary>
  [Path('/user')]
  TUserResource = class
    [GET, Produces(TMediaType.APPLICATION_JSON)]
    function GetUser: TUser;
  end;

  /// <summary>
  ///   Auth resource
  /// </summary>
  /// <security name="http-bearer" type="http" scheme="bearer" format="JWT">
  ///
  /// </security>
  [Path('auth')]
  TBasicAuthResource = class(TWiRLAuthBasicResource)
  private
  protected
    function Authenticate(const AUserName, APassword: string): TWiRLAuthResult; override;
  end;

  [Path('openapi')]
  TDocumentationResource = class(TOpenAPIResourceCustom);


  // The class factory is responsable to create the context.
  // It will be released by the system unless it's annotated
  // with the Singleton attribute
  TMyClassFactory = class(TInterfacedObject, IContextHttpFactory)
  public
    function CreateContextObject(const AObject: TRttiObject;
      AContext: TWiRLContextHttp): TValue;
  end;



implementation

uses
  System.DateUtils, System.StrUtils, System.IOUtils,

  WiRL.Core.JSON,
  WiRL.http.Accept.Language;

{ TStoreResource }

function TStoreResource.GetOrderById(AOrderID: Int64): TOrder;
begin
  Result.Id := AOrderID;
  Result.PetId := 123;
  Result.Quantity := 1;
  Result.ShipDate := Now + 1;
  Result.Status := TOrderStatus.approved;
  Result.Complete := True;
end;

{ TBasicAuthResource }

function TBasicAuthResource.Authenticate(const AUserName, APassword: string): TWiRLAuthResult;
begin
  Result.Success := SameText(APassword, 'mypassword');
  Result.Roles := 'admin,manager,user'.Split([','])
end;

{ TUserResource }

function TUserResource.GetUser: TUser;
begin
  Result.Id := 123;
end;

{ TPetResource }

procedure TPetResource.DeletePet(APetId: Integer);
begin
  if APetId = 0 then
    raise EWiRLBadRequestException.Create('Error Message');
end;

function TPetResource.GetPetById(APetID: Int64): TPet;
begin
  Result.Id := 123;
end;

function TPetResource.GetPetByStatus([QueryParam('status')] AStatus: TPetStatus): TArray<TPet>;
begin
  Result := [];
end;

function TPetResource.GetPetByTags(ATags: TArray<string>): TPetArray;
begin
  Result := [];
end;

function TPetResource.InsertPet(APet: TPet): TPet;
begin
  Result.Id := APet.Id;
end;

function TPetResource.UpdatePet(APet: TPet): TPet;
begin
  Result.Id := APet.Id;
end;

function TPetResource.UpdatePetForm(APetID: Int64; const AName: string; AStatus: TPetStatus): TPet;
begin
  Result.Id := APetID;
  Result.Name := AName;
  Result.Status := AStatus;
end;

function TPetResource.UploadImage(APetID: Int64; const AData: string; AImage: TStream): TApiResponse;
begin
  Result.Code := 0;
  Result.&Type := 'pet';
  Result.Message := 'image uploaded';
end;

{ TMyClassFactory }

function TMyClassFactory.CreateContextObject(const AObject: TRttiObject;
  AContext: TWiRLContextHttp): TValue;
begin
  var ctx := AContext.FindContextDataAs<TWiRLAuthContext>;
  if Assigned(ctx) then
    Result := TMyClass.Create;
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TPetResource>;
  {
  TWiRLResourceRegistry.Instance.RegisterResource<TStoreResource>;
  TWiRLResourceRegistry.Instance.RegisterResource<TUserResource>;
  TWiRLResourceRegistry.Instance.RegisterResource<TBasicAuthResource>;
  TWiRLResourceRegistry.Instance.RegisterResource<TDocumentationResource>;
  }
  TWiRLContextInjectionRegistry.Instance.RegisterFactory<TMyClass>(TMyClassFactory);

end.
