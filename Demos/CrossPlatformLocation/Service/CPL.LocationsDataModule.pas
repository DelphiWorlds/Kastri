unit CPL.LocationsDataModule;

interface

uses
  System.SysUtils, System.Classes, System.Sensors,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.ConsoleUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TLocationsDataModule = class(TDataModule)
    FDConnection: TFDConnection;
    LocationsTable: TFDTable;
    LocationsTableID: TFDAutoIncField;
    LocationsTableLatitude: TFloatField;
    LocationsTableLongitude: TFloatField;
    LocationsTableDeviceState: TIntegerField;
  private
    { Private declarations }
  public
    procedure Connect;
    procedure AddLocation(const ALocation: TLocationCoord2D; const ADeviceState: Integer);
  end;

var
  LocationsDataModule: TLocationsDataModule;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

uses
  System.IOUtils,
  DW.OSLog;

{ TLocationsDataModule }

procedure TLocationsDataModule.Connect;
begin
  FDConnection.Params.Database := TPath.Combine(TPath.GetDocumentsPath, 'CPL.sqlite');
  try
    FDConnection.Connected := True;
  except
    on E: Exception do
      TOSLog.e('Unable to connect to %s - %s: %s', [FDConnection.Params.Database, E.ClassName, E.Message]);
  end;
  if FDConnection.Connected then
  try
    LocationsTable.Open;
  except
    on E: Exception do
      TOSLog.e('Unable to open Locations table - %s: %s', [E.ClassName, E.Message]);
  end;
end;

procedure TLocationsDataModule.AddLocation(const ALocation: TLocationCoord2D; const ADeviceState: Integer);
begin
  if LocationsTable.Active then
  try
    LocationsTable.Append;
    LocationsTableLatitude.Value := ALocation.Latitude;
    LocationsTableLongitude.Value := ALocation.Longitude;
    LocationsTableDeviceState.Value := ADeviceState;
    LocationsTable.Post;
  except
    on E: Exception do
      TOSLog.e('Unable to add record to Locations table - %s: %s', [E.ClassName, E.Message]);
  end;
end;

end.
