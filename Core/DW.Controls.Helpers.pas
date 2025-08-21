unit DW.Controls.Helpers;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // RTL
  System.Generics.Defaults, System.Types,
  // FMX
  FMX.Controls, FMX.Graphics;

type
  TControlHelper = class helper for TControl
    /// <summary>
    ///   Finds the first child control matching the class: T
    /// </summary>
    function FindControl<T: TControl>(var AFindControl: T): Boolean; overload;
    /// <summary>
    ///   Finds the first child control matching the class: T, and Name matching AName
    /// </summary>
    function FindControl<T: TControl>(const AName: string; var AFindControl: T): Boolean; overload;
    /// <summary>
    ///   Finds the first parent control matching the class: T
    /// </summary>
    function FindParent<T: TControl>(var AFindControl: T): Boolean;
    function OuterRect: TRectF;
  end;

implementation

uses
  // DW
  DW.OSLog,
  // RTL
  System.SysUtils, System.Generics.Collections;

{ TControlHelper }

function TControlHelper.FindControl<T>(var AFindControl: T): Boolean;
begin
  Result := FindControl<T>('', AFindControl);
end;

function TControlHelper.FindControl<T>(const AName: string; var AFindControl: T): Boolean;
var
  LControl: T;
begin
  // TOSLog.d('Searching %s for %s with name: %s', [Name, T.ClassName, AName]);
  LControl := nil;
  EnumControls(
    function(AControl: TControl): TEnumControlsResult
    begin
      // TOSLog.d('Found %s (%s)', [AControl.Name, AControl.ClassName]);
      if (AControl is T) and (AName.IsEmpty or AnsiSameText(AControl.Name, AName)) then
      begin
        LControl := T(AControl);
        Result := TEnumControlsResult.Stop;
      end
      else
        Result := TEnumControlsResult.Continue;
    end
  );
  AFindControl := LControl;
  Result := AFindControl <> nil;
end;

function TControlHelper.FindParent<T>(var AFindControl: T): Boolean;
var
  LControl: TControl;
begin
  AFindControl := nil;
  LControl := ParentControl;
  while (LControl <> nil) and not (LControl is T) do
    LControl := LControl.ParentControl;
  Result := (LControl <> nil) and (LControl is T);
  if Result then
    AFindControl := T(LControl);
end;

function TControlHelper.OuterRect: TRectF;
begin
  Result := BoundsRect;
  Result.Inflate(Margins.Left, Margins.Top, Margins.Right, Margins.Bottom);
end;

end.
