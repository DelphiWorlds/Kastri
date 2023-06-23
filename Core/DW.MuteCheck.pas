unit DW.MuteCheck;

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
  TMuteCheck = class;

  TMuteCheckResultProc = reference to procedure(const IsMuted: Boolean);

  TCustomPlatformMuteCheck = class(TObject)
  private
    FMuteCheck: TMuteCheck;
  protected
    procedure Check; virtual; abstract;
    procedure HandleResult(const AIsMuted: Boolean);
    procedure SetSoundFileName(const Value: string); virtual;
    property MuteCheck: TMuteCheck read FMuteCheck;
  public
    constructor Create(const AMuteCheck: TMuteCheck); virtual;
    destructor Destroy; override;
  end;

  TMuteCheck = class(TObject)
  private
    FPlatformMuteCheck: TCustomPlatformMuteCheck;
    FCheckResultHandler: TMuteCheckResultProc;
    FSoundFileName: string;
    procedure SetSoundFileName(const Value: string);
  protected
    procedure HandleResult(const AIsMuted: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Check(const AResultHandler: TMuteCheckResultProc);
    property SoundFileName: string read FSoundFileName write SetSoundFileName;
  end;

implementation

uses
  // DW
  {$IF Defined(IOS)}
  DW.MuteCheck.iOS;
  {$ELSEIF Defined(ANDROID)}
  DW.MuteCheck.Android;
  {$ENDIF}

{ TCustomPlatformMuteCheck }

constructor TCustomPlatformMuteCheck.Create(const AMuteCheck: TMuteCheck);
begin
  inherited Create;
  FMuteCheck := AMuteCheck;
end;

destructor TCustomPlatformMuteCheck.Destroy;
begin
  //
  inherited;
end;

procedure TCustomPlatformMuteCheck.HandleResult(const AIsMuted: Boolean);
begin
  FMuteCheck.HandleResult(AIsMuted);
end;

procedure TCustomPlatformMuteCheck.SetSoundFileName(const Value: string);
begin
  //
end;

{ TMuteCheck }

procedure TMuteCheck.Check(const AResultHandler: TMuteCheckResultProc);
begin
  FCheckResultHandler := AResultHandler;
  FPlatformMuteCheck.Check;
end;

constructor TMuteCheck.Create;
begin
  inherited;
  FPlatformMuteCheck := TPlatformMuteCheck.Create(Self);
end;

destructor TMuteCheck.Destroy;
begin
  FPlatformMuteCheck.Free;
  inherited;
end;

procedure TMuteCheck.HandleResult(const AIsMuted: Boolean);
begin
  if Assigned(FCheckResultHandler) then
    FCheckResultHandler(AIsMuted);
  FCheckResultHandler := nil;
end;

procedure TMuteCheck.SetSoundFileName(const Value: string);
begin
  if FSoundFileName <> Value then
  begin
    FSoundFileName := Value;
    FPlatformMuteCheck.SetSoundFileName(FSoundFileName);
  end;
end;

end.
