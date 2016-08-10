(*
  Copyright 2015-2016, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit MARS.Core.Response;

interface

uses
  System.SysUtils, System.Classes;

type
  TMARSResponse = class
  protected
    function GetContent: string; virtual; abstract;
    function GetContentStream: TStream; virtual; abstract;
    function GetCustomHeaders: TStrings; virtual; abstract;
    function GetDate: TDateTime; virtual; abstract;
    function GetExpires: TDateTime; virtual; abstract;
    function GetLastModified: TDateTime; virtual; abstract;
    procedure SetContent(const Value: string); virtual; abstract;
    procedure SetContentStream(const Value: TStream); virtual; abstract;
    procedure SetCustomHeaders(const Value: TStrings); virtual; abstract;
    procedure SetDate(const Value: TDateTime); virtual; abstract;
    procedure SetExpires(const Value: TDateTime); virtual; abstract;
    procedure SetLastModified(const Value: TDateTime); virtual; abstract;
    function GetStatusCode: Integer; virtual; abstract;
    procedure SetStatusCode(const Value: Integer); virtual; abstract;
    function GetContentType: string; virtual; abstract;
    procedure SetContentType(const Value: string); virtual; abstract;
  public
    procedure SetCustomHeader(const Name, Value: string);
    property Date: TDateTime read GetDate write SetDate;
    property Expires: TDateTime read GetExpires write SetExpires;
    property LastModified: TDateTime read GetLastModified write SetLastModified;
    property Content: string read GetContent write SetContent;
    property ContentStream: TStream read GetContentStream write SetContentStream;
    property CustomHeaders: TStrings read GetCustomHeaders write SetCustomHeaders;
    property StatusCode: Integer read GetStatusCode write SetStatusCode;
    property ContentType: string read GetContentType write SetContentType;

  {
    procedure SendRedirect(const URI: string); virtual; abstract;
    procedure SendStream(AStream: TStream); virtual; abstract;
    function Sent: Boolean; virtual;
    procedure SetCookieField(Values: TStrings; const ADomain, APath: string;
      AExpires: TDateTime; ASecure: Boolean);
    procedure SetCustomHeader(const Name, Value: string);
    property Cookies: TCookieCollection read FCookies;
    property HTTPRequest: TWebRequest read FHTTPRequest;
    property Version: string index 0 read GetStringVariable write SetStringVariable;
    property ReasonString: string index 1 read GetStringVariable write SetStringVariable;
    property Server: string index 2 read GetStringVariable write SetStringVariable;
    property WWWAuthenticate: string index 3 read GetStringVariable write SetStringVariable;
    property Realm: string index 4 read GetStringVariable write SetStringVariable;
    property Allow: string index 5 read GetStringVariable write SetStringVariable;
    property Location: string index 6 read GetStringVariable write SetStringVariable;
    property ContentEncoding: string index 7 read GetStringVariable write SetStringVariable;
    property ContentType: string index 8 read GetStringVariable write SetStringVariable;
    property ContentVersion: string index 9 read GetStringVariable write SetStringVariable;
    property DerivedFrom: string index 10 read GetStringVariable write SetStringVariable;
    property Title: string index 11 read GetStringVariable write SetStringVariable;

    property StatusCode: Integer read GetStatusCode write SetStatusCode;
    property ContentLength: Integer index 0 read GetIntegerVariable write SetIntegerVariable;



    property LogMessage: string read GetLogMessage write SetLogMessage;

   }
  end;



implementation

{ TMARSResponse }

procedure TMARSResponse.SetCustomHeader(const Name, Value: string);
begin
  CustomHeaders.Values[Name] := Value;
end;

end.
