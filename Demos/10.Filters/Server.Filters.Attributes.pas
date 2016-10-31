unit Server.Filters.Attributes;

interface

uses
  System.SysUtils, System.Classes,
  WiRL.Core.Attributes;

type
  [NameBinding]
  PoweredByWiRLAttribute = class(TCustomAttribute);

//  PoweredByWiRLAttribute = class(NameBindingAttribute);

implementation

end.
