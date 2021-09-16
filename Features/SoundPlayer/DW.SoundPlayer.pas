unit DW.SoundPlayer;

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

type
  TSoundPlayer = class;

  TSoundItem = record
    FileName: string;
    ID: Integer;
    Name: string;
    constructor Create(const AFileName, AName: string; const AID: Integer);
  end;

  TSoundItems = TArray<TSoundItem>;

  TAudioContentType = (Music, Sound, Movie, Speech);

  TAudioUsage = (Media, Game, Alarm, Ringtone);
  
  TCustomPlatformSoundPlayer = class(TObject)
  private
    FAudioContentType: TAudioContentType;
    FAudioUsage: TAudioUsage;
    FSoundPlayer: TSoundPlayer;
    function GetSoundItems: TSoundItems;
    procedure SetMaxStreams(const Value: Integer);
    procedure SetAudioContentType(const Value: TAudioContentType);
    procedure SetAudioUsage(const Value: TAudioUsage);
  protected
    FMaxStreams: Integer;
    function AddSound(const AFileName, ASoundName: string): Integer; virtual; abstract;
    procedure ClearSoundItems;
    function DoAddSound(const AFileName, ASoundName: string; const AID: Integer): Integer;
    procedure NeedsUpdate; virtual;
    procedure PlaySound(const AItem: TSoundItem; const ARate: Single); virtual; abstract;
    property AudioContentType: TAudioContentType read FAudioContentType write SetAudioContentType;
    property AudioUsage: TAudioUsage read FAudioUsage write SetAudioUsage;
    property MaxStreams: Integer read FMaxStreams write SetMaxStreams;
    property SoundItems: TSoundItems read GetSoundItems;
    property SoundPlayer: TSoundPlayer read FSoundPlayer;
  public
    constructor Create(const ASoundPlayer: TSoundPlayer); virtual;
    destructor Destroy; override;
  end;

  TSoundPlayer = class(TObject)
  private
    FSoundItems: TSoundItems;
    FPlatformSoundPlayer: TCustomPlatformSoundPlayer;
    function GetAudioContentType: TAudioContentType;
    function GetAudioUsage: TAudioUsage;
    function GetMaxStreams: Integer;
    procedure SetAudioContentType(const Value: TAudioContentType);
    procedure SetAudioUsage(const Value: TAudioUsage);
    procedure SetMaxStreams(const Value: Integer);
  protected
    procedure ClearSoundItems;
    function DoAddSound(const AFileName, ASoundName: string; const AID: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function AddSound(const AFileName: string; const ASoundName: string = ''): Integer;
    procedure PlaySound(const AIndex: Integer; const ARate: Single = 1.0); overload;
    procedure PlaySound(const ASoundName: string; const ARate: Single = 1.0); overload;
    property AudioContentType: TAudioContentType read GetAudioContentType write SetAudioContentType;
    property AudioUsage: TAudioUsage read GetAudioUsage write SetAudioUsage;
    property MaxStreams: Integer read GetMaxStreams write SetMaxStreams;
    property SoundItems: TSoundItems read FSoundItems;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.IOUtils,
  // DW
  {$IF Defined(MACOS)}
  DW.SoundPlayer.Mac;
  {$ELSEIF Defined(ANDROID)}
  DW.SoundPlayer.Android;
  {$ENDIF}

{ TSoundItem }

constructor TSoundItem.Create(const AFileName, AName: string; const AID: Integer);
begin
  FileName := AFileName;
  Name := AName;
  ID := AID;
end;

{ TCustomPlatformSoundPlayer }

constructor TCustomPlatformSoundPlayer.Create(const ASoundPlayer: TSoundPlayer);
begin
  inherited Create;
  FSoundPlayer := ASoundPlayer;
end;

destructor TCustomPlatformSoundPlayer.Destroy;
begin
  //
  inherited;
end;

procedure TCustomPlatformSoundPlayer.ClearSoundItems;
begin
  FSoundPlayer.ClearSoundItems;
end;

function TCustomPlatformSoundPlayer.DoAddSound(const AFileName, ASoundName: string; const AID: Integer): Integer;
begin
  Result := FSoundPlayer.DoAddSound(AFileName, ASoundName, AID);
end;

function TCustomPlatformSoundPlayer.GetSoundItems: TSoundItems;
begin
  Result := FSoundPlayer.SoundItems;
end;

procedure TCustomPlatformSoundPlayer.NeedsUpdate;
begin
  //
end;

procedure TCustomPlatformSoundPlayer.SetAudioContentType(const Value: TAudioContentType);
begin
  if Value <> FAudioContentType then
  begin
    FAudioContentType := Value;
    NeedsUpdate;
  end;
end;

procedure TCustomPlatformSoundPlayer.SetAudioUsage(const Value: TAudioUsage);
begin
  if Value <> FAudioUsage then
  begin
    FAudioUsage := Value;
    NeedsUpdate;
  end;
end;

procedure TCustomPlatformSoundPlayer.SetMaxStreams(const Value: Integer);
begin
  if Value <> FMaxStreams then
  begin
    FMaxStreams := Value;
    NeedsUpdate;
  end;
end;

{ TSoundPlayer }

constructor TSoundPlayer.Create;
begin
  inherited;
  FPlatformSoundPlayer := TPlatformSoundPlayer.Create(Self);
end;

destructor TSoundPlayer.Destroy;
begin
  FPlatformSoundPlayer.Free;
  inherited;
end;

procedure TSoundPlayer.ClearSoundItems;
begin
  FSoundItems := [];
end;

function TSoundPlayer.DoAddSound(const AFileName, ASoundName: string; const AID: Integer): Integer;
begin
  Result := Length(FSoundItems);
  FSoundItems := FSoundItems + [TSoundItem.Create(AFileName, ASoundName, AID)];
end;

function TSoundPlayer.GetAudioContentType: TAudioContentType;
begin
  Result := FPlatformSoundPlayer.AudioContentType;
end;

function TSoundPlayer.GetAudioUsage: TAudioUsage;
begin
  Result := FPlatformSoundPlayer.AudioUsage;
end;

function TSoundPlayer.GetMaxStreams: Integer;
begin
  Result := FPlatformSoundPlayer.MaxStreams;
end;

procedure TSoundPlayer.PlaySound(const AIndex: Integer; const ARate: Single = 1.0);
begin
  if (AIndex >= 0) and (AIndex < Length(FSoundItems)) then
    FPlatformSoundPlayer.PlaySound(FSoundItems[AIndex], ARate);
end;

procedure TSoundPlayer.PlaySound(const ASoundName: string; const ARate: Single = 1.0);
var
  LItem: TSoundItem;
begin
  for LItem in FSoundItems do
  begin
    if LItem.Name.Equals(ASoundName) then
    begin
      FPlatformSoundPlayer.PlaySound(LItem, ARate);
      Break;
    end;
  end;
end;

procedure TSoundPlayer.SetAudioContentType(const Value: TAudioContentType);
begin
  FPlatformSoundPlayer.AudioContentType := Value;
end;

procedure TSoundPlayer.SetAudioUsage(const Value: TAudioUsage);
begin
  FPlatformSoundPlayer.AudioUsage := Value;
end;

procedure TSoundPlayer.SetMaxStreams(const Value: Integer);
begin
  FPlatformSoundPlayer.MaxStreams := Value;
end;

function TSoundPlayer.AddSound(const AFileName: string; const ASoundName: string = ''): Integer;
begin
  if ASoundName.IsEmpty then
    FPlatformSoundPlayer.AddSound(AFileName, TPath.GetFileNameWithoutExtension(TPath.GetFileName(AFileName)))
  else
    FPlatformSoundPlayer.AddSound(AFileName, ASoundName);
end;

end.
