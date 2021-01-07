unit Server.Resources.Swagger;

interface

uses
  System.Classes, System.SysUtils,

  WiRL.Core.OpenAPI.Resource,
  WiRL.Core.Registry,
  WiRL.Core.Attributes;

type
  // 1. You must inherit a resource from the base class TOpenAPIResourceCustom
  // 2. You must decide the path of this new resource
  // 3. You must register the resource (look at the initialization section)
  [Path('swagger')]
  TDocumentationResource = class(TOpenAPIResourceCustom);

implementation

initialization

  // Remember to register the OpenAPI resource
  TWiRLResourceRegistry.Instance.RegisterResource<TDocumentationResource>;

end.
