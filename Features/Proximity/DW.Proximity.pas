unit DW.Proximity;

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

type
  TProximity = class;

  TCustomPlatformProximity = class(TObject)
  private
    FBlankScreenWhenNear: Boolean;
    FProximity: TProximity;
  protected
    procedure DoProximityChange(const AIsNear: Boolean); virtual;
    procedure SetBlankScreenWhenNear(const Value: Boolean); virtual;
    property BlankScreenWhenNear: Boolean read FBlankScreenWhenNear write SetBlankScreenWhenNear;
    property Proximity: TProximity read FProximity;
  public
    constructor Create(const AProximity: TProximity); virtual;
    destructor Destroy; override;
  end;

  TProximityEvent = procedure(Sender: TObject; const IsNear: Boolean) of object;

  TProximity = class(TObject)
  private
    FPlatformProximity: TCustomPlatformProximity;
    FOnProximity: TProximityEvent;
    function GetBlankScreenWhenNear: Boolean;
    procedure SetBlankScreenWhenNear(const Value: Boolean);
  protected
    procedure DoProximityChange(const AIsNear: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    property BlankScreenWhenNear: Boolean read GetBlankScreenWhenNear write SetBlankScreenWhenNear;
    property OnProximity: TProximityEvent read FOnProximity write FOnProximity;
  end;

implementation

uses
  {$IF Defined(IOS)}
  DW.Proximity.iOS;
  {$ELSEIF Defined(ANDROID)}
  DW.Proximity.Android;
  {$ENDIF}

{ TCustomPlatformProximity }

constructor TCustomPlatformProximity.Create(const AProximity: TProximity);
begin
  inherited Create;
  FProximity := AProximity;
end;

destructor TCustomPlatformProximity.Destroy;
begin
  //
  inherited;
end;

procedure TCustomPlatformProximity.DoProximityChange(const AIsNear: Boolean);
begin
  FProximity.DoProximityChange(AIsNear);
end;

procedure TCustomPlatformProximity.SetBlankScreenWhenNear(const Value: Boolean);
begin
  FBlankScreenWhenNear := Value;
end;

{ TProximity }

constructor TProximity.Create;
begin
  inherited;
  FPlatformProximity := TPlatformProximity.Create(Self);
end;

destructor TProximity.Destroy;
begin
  FPlatformProximity.Free;
  inherited;
end;

procedure TProximity.DoProximityChange(const AIsNear: Boolean);
begin
  //
end;

function TProximity.GetBlankScreenWhenNear: Boolean;
begin
  Result := FPlatformProximity.BlankScreenWhenNear;
end;

procedure TProximity.SetBlankScreenWhenNear(const Value: Boolean);
begin
  FPlatformProximity.BlankScreenWhenNear := Value;
end;

end.
