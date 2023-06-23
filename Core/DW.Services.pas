unit DW.Services;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2023 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // RTL
  System.Generics.Collections;

type
  TServiceList = TDictionary<TGUID, IInterface>;

  /// <summary>
  ///   Registry of services used by an application. Similar to FMX.Platform.TPlatformServices
  /// </summary>
  TServices = class(TObject)
  private
    class var FServices: TServices;
    class constructor CreateClass;
    class destructor DestroyClass;
  private
    FList: TServiceList;
    class function GetService(const AIndex: Integer): IInterface; static;
  protected
    property List: TServiceList read FList;
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>
    ///   Adds a service to the list. First checks if the service has already been added
    /// </summary>
    class function Add(const AServiceGUID: TGUID; const AService: IInterface): Boolean;
    /// <summary>
    ///   Retrieves a service from the list
    /// </summary>
    class function Get(const AServiceGUID: TGUID; out AService): Boolean;
    /// <summary>
    ///   Removes a service from the list
    /// </summary>
    class procedure Remove(const AServiceGUID: TGUID);
    /// <summary>
    ///   Count of the services in the list
    /// </summary>
    class function ServiceCount: Integer;
    /// <summary>
    ///   Access to individual services
    /// </summary>
    class property Services[const AIndex: Integer]: IInterface read GetService;
  end;

implementation

uses
  // RTL
  System.SysUtils;

{ TServices }

constructor TServices.Create;
begin
  inherited;
  FList := TDictionary<TGUID, IInterface>.Create;
end;

destructor TServices.Destroy;
begin
  FList.Free;
  inherited;
end;

class constructor TServices.CreateClass;
begin
  FServices := TServices.Create;
end;

class destructor TServices.DestroyClass;
begin
  FServices.Free;
end;

class function TServices.Add(const AServiceGUID: TGUID; const AService: IInterface): Boolean;
begin
  Result := False;
  if not FServices.List.ContainsKey(AServiceGUID) then
  begin
    FServices.List.Add(AServiceGUID, AService);
    Result := True;
  end;
end;

class function TServices.Get(const AServiceGUID: TGUID; out AService): Boolean;
begin
  if not FServices.List.ContainsKey(AServiceGUID) then
  begin
    Pointer(AService) := nil;
    Result := False;
  end
  else
    Result := Supports(FServices.List.Items[AServiceGUID], AServiceGUID, AService);
end;

class function TServices.GetService(const AIndex: Integer): IInterface;
begin
  Result := FServices.List.ToArray[AIndex].Value;
end;

class procedure TServices.Remove(const AServiceGUID: TGUID);
begin
  FServices.List.Remove(AServiceGUID);
end;

class function TServices.ServiceCount: Integer;
begin
  Result := FServices.List.Count;
end;

end.
