unit MARS.http.Filters;

interface

uses
  System.SysUtils, System.Generics.Collections, System.Classes,
  MARS.Core.Request, MARS.Core.Response;

type
  IMARSContainerRequestFilter = interface
  ['{58406938-14A1-438F-946A-F0723920B511}']
    procedure Filter(Request: TMARSRequest);
  end;

  IMARSContainerResponseFilter = interface
  ['{F952495E-00DB-44C6-ACED-33F1F2F25527}']
    procedure Filter(Request :TMARSRequest; Response: TMARSResponse);
  end;

implementation

end.
