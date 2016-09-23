unit Server.Filters.Attributes;

interface

uses
  System.SysUtils, System.Classes,
  MARS.Core.Attributes;

type
  [NameBinding]
  PoweredByMARSAttribute = class(TCustomAttribute);

//  PoweredByMARSAttribute = class(NameBindingAttribute);

implementation

end.
