unit DW.OrientationMonitor;

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
  // FMX
  FMX.Types;

type
  TOrientationChangedEvent = procedure(Sender: TObject; const Orientation: TScreenOrientation) of object;

  TOrientationMonitor = class(TObject)
  private
    FHasOrientationChanged: Boolean;
    FOrientation: TScreenOrientation;
    FRotation: Integer;
    FTimer: TTimer;
    FOnOrientationChanged: TOrientationChangedEvent;
    procedure DoOrientationChange;
    procedure SetIsActive(const Value: Boolean);
    procedure TimerHandler(Sender: TObject);
    function GetIsActive: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property IsActive: Boolean read GetIsActive write SetIsActive;
    property Orientation: TScreenOrientation read FOrientation;
    property OnOrientationChanged: TOrientationChangedEvent read FOnOrientationChanged write FOnOrientationChanged;
  end;

implementation

uses
  // DW
  DW.UIHelper;

{ TOrientationMonitor }

constructor TOrientationMonitor.Create;
begin
  inherited;
  FOrientation := TUIHelper.GetScreenOrientation;
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 100;
  FTimer.OnTimer := TimerHandler;
end;

destructor TOrientationMonitor.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TOrientationMonitor.DoOrientationChange;
begin
  if Assigned(FOnOrientationChanged) then
    FOnOrientationChanged(Self, FOrientation);
end;

function TOrientationMonitor.GetIsActive: Boolean;
begin
  Result := FTimer.Enabled;
end;

procedure TOrientationMonitor.SetIsActive(const Value: Boolean);
begin
  FTimer.Enabled := Value;
end;

procedure TOrientationMonitor.TimerHandler(Sender: TObject);
var
  LOrientation: TScreenOrientation;
begin
  LOrientation := TUIHelper.GetScreenOrientation;
  if LOrientation <> FOrientation then
  begin
    FOrientation := LOrientation;
    DoOrientationChange;
  end;
end;

end.
