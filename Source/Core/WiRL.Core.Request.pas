(*
  Copyright 2015-2016, WiRL - REST Library

  Home: https://github.com/WiRL-library

*)
unit WiRL.Core.Request;

interface

uses
  System.Classes, System.SysUtils;

type
  TWiRLMethod = class
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

  TWiRLRequest = class
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
    function GetRawPathInfo: string; virtual; abstract;
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
    property RawPathInfo: string read GetRawPathInfo;

    function GetFieldByName(const Name: string): string;
  end;

implementation

{ TWiRLRequest }

function TWiRLRequest.GetFieldByName(const Name: string): string;
begin
  Result := DoGetFieldByName(Name);
end;

end.
