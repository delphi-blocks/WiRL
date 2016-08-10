(*
  Copyright 2015-2016, MARS - REST Library

  Home: https://github.com/MARS-library

*)
unit MARS.Core.Request;

interface

uses
  System.Classes, System.SysUtils;

type
  TMARSMethod = class
  const
    GET = 'GET';
    PUT = 'PUT';
    POST = 'POST';
    HEAD = 'HEAD';
    DELETE = 'DELETE';
    PATCH = 'PATCH';
    OPTIONS = 'OPTIONS';
    TRACE = 'TRACE';
    CONNECT = 'CONNECT';
  end;

  TMARSRequest = class
  protected
    function GetPathInfo: string; virtual; abstract;
    function GetQuery: string; virtual; abstract;
    function GetHost: string; virtual; abstract;
    function GetServerPort: Integer; virtual; abstract;
    function GetMethod: string; virtual; abstract;
    function GetQueryFields: TStrings; virtual; abstract;
    function GetContentFields: TStrings; virtual; abstract;
    function GetCookieFields: TStrings; virtual; abstract;
    function GetContent: string; virtual; abstract;
    function GetAuthorization: string; virtual; abstract;
    function GetAccept: string; virtual; abstract;
    function GetContentType: string; virtual; abstract;
    function GetContentLength: Integer; virtual; abstract;
    function GetContentVersion: string; virtual; abstract;
    function DoGetFieldByName(const Name: string): string; virtual; abstract;
  public
    property PathInfo: string read GetPathInfo;
    property Query: string read GetQuery;
    property Method: string read GetMethod;
    property Host: string read GetHost;
    property ServerPort: Integer read GetServerPort;
    property QueryFields: TStrings read GetQueryFields;
    property ContentFields: TStrings read GetContentFields;
    property CookieFields: TStrings read GetCookieFields;
    property Content: string read GetContent;
    property ContentType: string read GetContentType;
    property ContentLength: Integer read GetContentLength;
    property ContentVersion: string read GetContentVersion;
    property Authorization: string read GetAuthorization;
    property Accept: string read GetAccept;

    function GetFieldByName(const Name: string): string;
  end;

implementation

{ TMARSRequest }

function TMARSRequest.GetFieldByName(const Name: string): string;
begin
  Result := DoGetFieldByName(Name);
end;

end.
