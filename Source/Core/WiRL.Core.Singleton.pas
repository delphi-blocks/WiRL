{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit WiRL.Core.Singleton;

interface

// Current limitation: The class must have a default non-parameters constructor

uses
  System.SysUtils, System.SyncObjs;

type
  EWiRLSingletonException = class(Exception);

  /// <summary>
  ///   Singleton Class (Singleton Pattern)
  /// </summary>
  TWiRLSingleton<T: class, constructor> = class(TObject)
  private
    class var
      FInstance: T;
      FInstanceLock: TCriticalSection;
  protected
    class function GetInstance: T; static;
    class function GetSafeInstance: T; static;
    class procedure CheckInstance(AObject: T); static;
  public
    /// <summary>
    ///   Class constructor (called before initialization)
    /// </summary>
    class constructor Create;
    /// <summary>
    ///   Class destructor (called after finalization)
    /// </summary>
    class destructor Destroy;
    /// <summary>
    ///   Hide base class constructor Create (without parameters)
    /// </summary>
    class function Create: T;

    /// <summary>
    ///   Instance class property
    /// </summary>
    class property Instance: T read GetInstance;
    /// <summary>
    ///   Instance class property (thread safe)
    /// </summary>
    class property SafeInstance: T read GetSafeInstance;
  end;

 
implementation
 
{ TWiRLSingleton<T> }

class constructor TWiRLSingleton<T>.Create;
begin
  FInstanceLock := TCriticalSection.Create;
end;

class function TWiRLSingleton<T>.Create(): T;
begin
  Result := Instance;
end;
 
class destructor TWiRLSingleton<T>.Destroy;
begin
  FreeAndNil(FInstance);
  FreeAndNil(FInstanceLock);
end;

class procedure TWiRLSingleton<T>.CheckInstance(AObject: T);
begin
  if Assigned(FInstance) and Assigned(AObject) and (AObject <> FInstance) then
    raise Exception.CreateFmt('Please use property "Instance" of "%s" and not named constructor "Create"', [AObject.ClassName]);
end;

class function TWiRLSingleton<T>.GetInstance: T;
begin
  if not Assigned(FInstance) then
    FInstance := T.Create;

  Result := FInstance;
end;
 
class function TWiRLSingleton<T>.GetSafeInstance: T;
begin
  if not Assigned(FInstance) then
  begin
    // Lazy Initialization protection
    FInstanceLock.Acquire;
    try
      // Need to recheck FInstance because other calls may have created the instance
      if not Assigned(FInstance) then
        FInstance := GetInstance;
    finally
      FInstanceLock.Release;
    end;
  end;
  Result := FInstance;
end;

end.
