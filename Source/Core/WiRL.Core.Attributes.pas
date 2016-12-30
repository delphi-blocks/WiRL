{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Attributes;

{$I WiRL.inc}

interface

uses
  System.SysUtils, System.Classes, System.Rtti, System.Generics.Collections,
  
  WiRL.Core.Declarations, 
  WiRL.Core.Request,
  WiRL.Core.Utils;

type

{$REGION 'JAX-Like Attributes'}

  PathAttribute = class(TCustomAttribute)
  private
    FValue: string;
  public
    constructor Create(const AValue: string);
    property Value: string read FValue write FValue;
  end;

  HttpMethodAttribute = class(TCustomAttribute)
  private
  protected
  public
    function Matches(const ARequest: TWiRLRequest): Boolean; virtual;
  end;

  GETAttribute     = class(HttpMethodAttribute)
  public
    function Matches(const ARequest: TWiRLRequest): Boolean; override;
  end;

  POSTAttribute    = class(HttpMethodAttribute)
  public
    function Matches(const ARequest: TWiRLRequest): Boolean; override;
  end;

  PUTAttribute     = class(HttpMethodAttribute)
  public
    function Matches(const ARequest: TWiRLRequest): Boolean; override;
  end;

  DELETEAttribute  = class(HttpMethodAttribute)
  public
    function Matches(const ARequest: TWiRLRequest): Boolean; override;
  end;

  PATCHAttribute   = class(HttpMethodAttribute)
  public
    function Matches(const ARequest: TWiRLRequest): Boolean; override;
  end;

  HEADAttribute    = class(HttpMethodAttribute)
  public
    function Matches(const ARequest: TWiRLRequest): Boolean; override;
  end;

  OPTIONSAttribute = class(HttpMethodAttribute)
  public
    function Matches(const ARequest: TWiRLRequest): Boolean; override;
  end;

  /// <summary>
  ///   A list of media types. Each entry may specify a single type or consist of a comma separated list of types. E.g.
  ///   {"text/html, application/pdf"}. <br />Use of the comma-separated form allows definition of a common string
  ///   constant for use on multiple targets
  /// </summary>
  ConsumesAttribute = class(TCustomAttribute)
  private
    FValue: string;
  public
    constructor Create(const AValue: string);
    /// <summary>
    ///   A list of media types
    /// </summary>
    property Value: string read FValue write FValue;
  end;

  /// <summary>
  ///   A list of media types. Each entry may specify a single type or consist of a comma separated list of types. E.g.
  ///   {"text/html, application/pdf"}. <br />Use of the comma-separated form allows definition of a common string
  ///   constant for use on multiple targets
  /// </summary>
  ProducesAttribute = class(TCustomAttribute)
  private
    FValue: string;
  public
    constructor Create(const AValue: string);
    /// <summary>
    ///   A list of media types
    /// </summary>
    property Value: string read FValue write FValue;
  end;

  MethodParamAttribute = class(TCustomAttribute)
  private
    FValue: string;
  public
    constructor Create(const AValue: string = '');

    property Value: string read FValue write FValue;
  end;

  PathParamAttribute = class(MethodParamAttribute)
  private
    FParamIndex: Integer;
  public
    property ParamIndex: Integer read FParamIndex write FParamIndex;
  end;
  QueryParamAttribute = class(MethodParamAttribute);
  FormParamAttribute = class(MethodParamAttribute);
  HeaderParamAttribute = class(MethodParamAttribute);
  CookieParamAttribute = class(MethodParamAttribute);
  BodyParamAttribute = class(MethodParamAttribute)
  public
    constructor Create;
  end;

  ContextAttribute = class(TCustomAttribute);

  AuthorizationAttribute = class(TCustomAttribute);

  PermitAllAttribute = class(AuthorizationAttribute);
  DenyAllAttribute = class(AuthorizationAttribute);
  RolesAllowedAttribute = class(AuthorizationAttribute)
  private
    FRoles: TStringList;
  public
    constructor Create(const ARoles: string); overload; virtual;
    constructor Create(const ARoles: TArray<string>); overload; virtual;
    destructor Destroy; override;

    property Roles: TStringList read FRoles;
  end;

  SingletonAttribute = class(TCustomAttribute);

  PreMatchingAttribute = class(TCustomAttribute);

  NameBindingAttribute = class(TCustomAttribute);

  PriorityAttribute = class(TCustomAttribute)
  private
    FValue: Integer;
  public
    constructor Create(Value: Integer);
    property Value: Integer read FValue write FValue;
  end;

{$ENDREGION}

{$REGION 'WiRL-specific Attributes'}

  LoginRequiredAttribute = class(TCustomAttribute);

  URLParamAttribute = class(TCustomAttribute)
  private
    FPosition: Integer;
    FDefaultValue: TValue;
  public
    constructor Create(APosition: Integer; const ADefaultValue: TValue); overload;

    constructor Create(APosition: Integer); overload;
    constructor Create(APosition: Integer; const ADefaultValue: string); overload;
    constructor Create(APosition: Integer; ADefaultValue: Integer); overload;
    constructor Create(APosition: Integer; ADefaultValue: Double); overload;
    constructor Create(APosition: Integer; ADefaultValue: Boolean); overload;

    property Position: Integer read FPosition write FPosition;
    property DefaultValue: TValue read FDefaultValue write FDefaultValue;
  end;

  ContentTypeAttribute = class(TCustomAttribute)
  private
    FContentType: string;
  public
    constructor Create(const AContentType: string);
    property ContentType: string read FContentType;
  end;

  RestAttribute = class(TCustomAttribute)
  end;
{$ENDREGION}

implementation

{ URLParamAttribute }

constructor URLParamAttribute.Create(APosition: Integer; const ADefaultValue: string);
begin
  Create(APosition, TValue.From<string>(ADefaultValue));
end;

constructor URLParamAttribute.Create(APosition: Integer; ADefaultValue: Integer);
begin
  Create(APosition, TValue.From<Integer>(ADefaultValue));
end;

constructor URLParamAttribute.Create(APosition: Integer; const ADefaultValue: TValue);
begin
  inherited Create;
  FPosition := APosition;
  FDefaultValue := ADefaultValue;
end;

constructor URLParamAttribute.Create(APosition: Integer; ADefaultValue: Double);
begin
  Create(APosition, TValue.From<Double>(ADefaultValue));
end;

constructor URLParamAttribute.Create(APosition: Integer;
  ADefaultValue: Boolean);
begin
  Create(APosition, TValue.From<Boolean>(ADefaultValue));
end;

constructor URLParamAttribute.Create(APosition: Integer);
begin
  Create(APosition, TValue.Empty);
end;

{ ContentTypeAttribute }

constructor ContentTypeAttribute.Create(const AContentType: string);
begin
  inherited Create;
  FContentType := AContentType;
end;

{ PathAttribute }

constructor PathAttribute.Create(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

{ ConsumesAttribute }

constructor ConsumesAttribute.Create(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

{ ProducesAttribute }

constructor ProducesAttribute.Create(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

{ MethodParamAttribute }

constructor MethodParamAttribute.Create(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

{ RolesAllowedAttribute }

constructor RolesAllowedAttribute.Create(const ARoles: string);
begin
  Create(ARoles.Split([',']));
end;

constructor RolesAllowedAttribute.Create(const ARoles: TArray<string>);
var
  LRole: string;
begin
  inherited Create;

  FRoles := TStringList.Create;

  for LRole in ARoles do
    FRoles.Add(LRole.Trim);
end;

destructor RolesAllowedAttribute.Destroy;
begin
  FRoles.Free;
  inherited;
end;

{ BodyParamAttribute }

constructor BodyParamAttribute.Create;
begin
  inherited Create('body');
end;

{ HttpMethodAttribute }

function HttpMethodAttribute.Matches(const ARequest: TWiRLRequest): Boolean;
begin
  Result := False;
end;

{ GETAttribute }

function GETAttribute.Matches(const ARequest: TWiRLRequest): Boolean;
begin
  Result := ARequest.Method = TWiRLMethod.GET;
end;

{ POSTAttribute }

function POSTAttribute.Matches(const ARequest: TWiRLRequest): Boolean;
begin
  Result := ARequest.Method = TWiRLMethod.POST;
end;

{ PUTAttribute }

function PUTAttribute.Matches(const ARequest: TWiRLRequest): Boolean;
begin
  Result := ARequest.Method = TWiRLMethod.PUT;
end;

{ DELETEAttribute }

function DELETEAttribute.Matches(const ARequest: TWiRLRequest): Boolean;
begin
  Result := ARequest.Method = TWiRLMethod.DELETE;
end;

{ PATCHAttribute }

function PATCHAttribute.Matches(const ARequest: TWiRLRequest): Boolean;
begin
  Result := ARequest.Method = TWiRLMethod.PATCH;
end;

{ HEADAttribute }

function HEADAttribute.Matches(const ARequest: TWiRLRequest): Boolean;
begin
  Result := ARequest.Method = TWiRLMethod.HEAD;
end;

{ OPTIONSAttribute }

function OPTIONSAttribute.Matches(const ARequest: TWiRLRequest): Boolean;
begin
  Result := ARequest.Method = 'OPTIONS';
end;

{ PriorityAttribute }

constructor PriorityAttribute.Create(Value: Integer);
begin
  FValue := Value;
end;

end.
