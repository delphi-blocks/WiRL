{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Persistence.Attributes;

interface

uses
  System.Classes, System.SysUtils,
  WiRL.Persistence.Types;

type
  NeonAttribute = class(TCustomAttribute)
  end;

  NeonNamedAttribute = class(NeonAttribute)
  private
    FValue: string;
  public
    constructor Create(const AValue: string);
    property Value: string read FValue write FValue;
  end;

  /// <summary>
  ///   The attribute [NeonProperty]  is used to indicate the property name in JSON.
  /// </summary>
  /// <remarks>
  ///   Read + Write Attribute
  /// </remarks>
  NeonPropertyAttribute = class(NeonNamedAttribute);

  /// <summary>
  ///   The Neon attribute [NeonIgnore] is used to tell Neon to ignore a certain property (field)
  ///   of a Delphi object. The property is ignored both when reading JSON into Delphi objects, and
  ///   when writing Delphi objects into JSON.
  /// </summary>
  /// <remarks>
  ///   Read + Write Attribute
  /// </remarks>
  NeonIgnoreAttribute = class(NeonAttribute);

  /// <summary>
  ///   The Neon attribute [NeonIncludeIf] is used to compute the inclusion of the
  ///   field/property at run time. The member is serialized if the method passed as
  ///   parameter returns True
  /// </summary>
  /// <remarks>
  ///   Read + Write Attribute
  /// </remarks>
  NeonIncludeIfAttribute = class(NeonNamedAttribute);

  /// <summary>
  ///   The NeonIgnoreProperties Neon annotation is used to specify a list of properties
  ///   of a class to ignore. The NeonIgnoreProperties annotation is placed above the
  ///   class declaration instead of above the individual properties (fields) to ignore.
  /// </summary>
  /// <remarks>
  ///   Read + Write Attribute
  /// </remarks>
  NeonIgnorePropertiesAttribute = class(NeonAttribute);

  /// <summary>
  ///   The NeonIgnoreType Neon annotation is used to mark a whole type (class) to be
  ///   ignored everywhere that type is used.
  /// </summary>
  /// <remarks>
  ///   Read + Write Attribute
  /// </remarks>
  NeonIgnoreTypeAttribute = class(NeonAttribute);

  /// <summary>
  ///   The Neon attribute NeonMembers is used to tell Neon to change the Members
  ///   when reading/writing a specific record/object
  /// </summary>
  /// <remarks>
  ///   Read + Write Attribute
  /// </remarks>
  NeonMembersAttribute = class(NeonAttribute)
  private
    FValue: TNeonMembers;
  public
    constructor Create(const AValue: TNeonMembers);
    property Value: TNeonMembers read FValue write FValue;
  end;

  /// <summary>
  ///   The Neon attribute NeonMembersType is used to tell Neon to change the Visibility
  ///   when reading/writing a specific record/object
  /// </summary>
  /// <remarks>
  ///   Read + Write Attribute
  /// </remarks>
  NeonVisibilityAttribute = class(NeonAttribute)
  private
    FValue: TNeonVisibility;
  public
    constructor Create(const AValue: TNeonVisibility);
    property Value: TNeonVisibility read FValue write FValue;
  end;

  /// <summary>
  ///   The Neon annotation NeonInclude tells Neon to include the property (or field)
  /// </summary>
  /// <remarks>
  ///   Write Attribute
  /// </remarks>
  NeonIncludeAttribute = class(NeonAttribute);

  /// <summary>
  ///   The NeonSerialize Neon annotation is used to specify a custom serializer for a
  ///   field in a Delphi object.
  /// </summary>
  NeonSerializeAttribute = class(NeonAttribute);

  /// <summary>
  ///   The Neon annotation NeonDeserialize is used to specify a custom de-serializer
  ///   class for a given field in a Delphi object.
  /// </summary>
  NeonDeserializeAttribute = class(NeonAttribute);

  /// <summary>
  ///   The NeonPropertyOrder Neon annotation can be used to specify in what order the
  ///   fields of your Delphi object should be serialized into JSON.
  /// </summary>
  NeonPropertyOrderAttribute = class(NeonAttribute);

  /// <summary>
  ///   The Neon annotation NeonValue tells Neon that Neon should not attempt to
  ///   serialize the object itself, but rather call a method on the object which
  ///   serializes the object to a JSON string. Neon will escape any quotation marks
  ///   inside the String returned by the custom serialization, so you cannot return e.g.
  ///   a full JSON object. For that you should use NeonRawValue instead.
  /// </summary>
  NeonValueAttribute = class(NeonAttribute);

  /// <summary>
  ///   The NeonRawValue annotation tells Neon that this property value should written
  ///   directly as it is to the JSON output. If the property is a String Neon would
  ///   normally have enclosed the value in quotation marks, but if annotated with the
  ///   NeonRawValue property Neon won't do that.
  /// </summary>
  NeonRawValueAttribute = class(NeonAttribute);

  {
  //Read Annotations
  NeonSetterAttribute = class(NeonAttribute);
  NeonAnySetterAttribute = class(NeonAttribute);
  NeonCreatorAttribute = class(NeonAttribute);
  NeonInjectAttribute = class(NeonAttribute);
  //Write Annotations
  NeonGetterAttribute = class(NeonAttribute);
  NeonAnyGetterAttribute = class(NeonAttribute);
  }

implementation

uses
  System.Rtti, System.StrUtils, System.DateUtils,

  WiRL.Rtti.Utils,
  WiRL.Core.Utils;

{ NeonNamedAttribute }

constructor NeonNamedAttribute.Create(const AValue: string);
begin
  FValue := AValue;
end;

{ NeonMembersTypeAttribute }

constructor NeonMembersAttribute.Create(const AValue: TNeonMembers);
begin
  FValue := AValue;
end;

{ NeonVisibilityAttribute }

constructor NeonVisibilityAttribute.Create(const AValue: TNeonVisibility);
begin
  FValue := AValue;
end;

end.
