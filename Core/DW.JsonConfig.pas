unit DW.JsonConfig;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2021 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  System.Classes;

type
  TJsonConfig = class(TObject)
  private
    FOnChanged: TNotifyEvent;
  protected
    procedure DoLoaded; virtual;
    procedure DoSave; virtual;
    procedure DoSaved; virtual;
  public
    /// <summary>
    ///   Creates the object from a file if it exists, or creates a new one
    /// </summary>
    class procedure CreateFromFile<T: TJsonConfig, constructor>(out AObject: T; const ACreate: Boolean = True);
    /// <summary>
    ///   Returns the name/path of the config file
    /// </summary>
    class function GetConfigFileName: string; virtual;
  public
    procedure Save;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

implementation

uses
  // RTL
  System.IOUtils, System.SysUtils,
  // REST
  REST.Json,
  // DW
  DW.IOUtils.Helpers, DW.REST.Json.Helpers;

{ TJsonConfig }

class procedure TJsonConfig.CreateFromFile<T>(out AObject: T; const ACreate: Boolean = True);
begin
  if not TJson.FileToObject(AObject, T.GetConfigFilename) and ACreate then
    AObject := T.Create;
  if AObject <> nil then
    TJsonConfig(AObject).DoLoaded;
end;

procedure TJsonConfig.DoLoaded;
begin
  //
end;

procedure TJsonConfig.DoSave;
begin
  //
end;

procedure TJsonConfig.DoSaved;
begin
  //
end;

class function TJsonConfig.GetConfigFilename: string;
begin
  Result := TPathHelper.GetAppSupportFile(TPathHelper.GetAppName + '.json');
end;

procedure TJsonConfig.Save;
begin
  DoSave;
  ForceDirectories(TPath.GetDirectoryName(GetConfigFilename));
  TJson.SaveToFile(Self, GetConfigFilename);
  if Assigned(FOnChanged) then
    FOnChanged(Self);
  DoSaved;
end;

end.
