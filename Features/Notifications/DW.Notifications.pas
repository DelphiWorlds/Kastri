unit DW.Notifications;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

{$SCOPEDENUMS ON}

type
  TRepeatInterval = (None, Second, Minute, Hour, Day, Week, Weekday, Month, Quarter, Year, Era);

  TNotification = record
    AlertAction: string;
    AlertBody: string;
    EnableSound: Boolean;
    FireDate: TDateTime;
    HasAction: Boolean;
    Image: string;
    IsInsistent: Boolean;
    Name: string;
    Number: Integer;
    RepeatInterval: TRepeatInterval;
    SoundName: string;
    Subtitle: string;
    Title: string;
  end;

  TCustomPlatformNotificationChannel = class(TObject)
  protected
    procedure DoRegisterChannel; virtual; abstract;
    function GetDescription: string; virtual; abstract;
    function GetGroup: string; virtual; abstract;
    function GetId: string; virtual; abstract;
    function GetImportance: Integer; virtual; abstract;
    function GetLightColor: Integer; virtual; abstract;
    function GetLockScreenVisibility: Integer; virtual; abstract;
    function GetName: string; virtual; abstract;
    function GetSound: string; virtual; abstract;
    procedure SetDescription(const Value: string); virtual; abstract;
    procedure SetGroup(const Value: string); virtual; abstract;
    procedure SetImportance(const Value: Integer); virtual; abstract;
    procedure SetLightColor(const Value: Integer); virtual; abstract;
    procedure SetLockScreenVisibility(const Value: Integer); virtual; abstract;
    procedure SetName(const Value: string); virtual; abstract;
    procedure SetSound(const Value: string); virtual; abstract;
  public
    constructor Create(const AId, AName: string; const AImportance: Integer); virtual;
    procedure RegisterChannel;
    property Description: string read GetDescription write SetDescription;
    property Group: string read GetGroup write SetGroup;
    property Id: string read GetId;
    property Importance: Integer read GetImportance write SetImportance;
    property LightColor: Integer read GetLightColor write SetLightColor;
    property LockScreenVisibility: Integer read GetLockScreenVisibility write SetLockScreenVisibility;
    property Name: string read GetName write SetName;
    property Sound: string read GetSound write SetSound;
  end;

  TNotificationChannels = class(TObject);

  TNotifications = class;

  TCustomPlatformNotifications = class(TObject)
  private
    FChannels: TNotificationChannels;
    FNotifications: TNotifications;
  protected
    procedure CancelAll; virtual;
    procedure CancelNotification(const AName: string); virtual;
    procedure PresentNotification(const ANotification: TNotification); virtual;
    procedure ScheduleNotification(const ANotification: TNotification); virtual;
    property Channels: TNotificationChannels read FChannels;
    property Notifications: TNotifications read FNotifications;
  public
    constructor Create(const ANotifications: TNotifications); virtual;
    destructor Destroy; override;
  end;

  TNotificationReceivedEvent = procedure(Sender: TObject; const Notification: TNotification) of object;

  TNotifications = class(TObject)
  private
    FPlatformNotifications: TCustomPlatformNotifications;
    FOnNotificationReceived: TNotificationReceivedEvent;
    function GetChannels: TNotificationChannels;
  protected
    procedure DoNotificationReceived(const ANotification: TNotification);
  public
    constructor Create;
    destructor Destroy; override;
    procedure CancelAll;
    procedure CancelNotification(const AName: string);
    procedure PresentNotification(const ANotification: TNotification);
    procedure ScheduleNotification(const ANotification: TNotification);
    property Channels: TNotificationChannels read GetChannels;
    property OnNotificationReceived: TNotificationReceivedEvent read FOnNotificationReceived write FOnNotificationReceived;
  end;

implementation

uses
  {$IF Defined(IOS)}
  DW.Notifications.iOS;
  {$ELSEIF Defined(ANDROID)}
  DW.Notifications.Android;
  {$ELSE}
  DW.Notifications.Default;
  {$ENDIF}

{ TCustomPlatformNotificationChannel }

constructor TCustomPlatformNotificationChannel.Create(const AId, AName: string; const AImportance: Integer);
begin
  // does nothing
end;

procedure TCustomPlatformNotificationChannel.RegisterChannel;
begin
  DoRegisterChannel;
end;

{ TCustomPlatformNotifications }

constructor TCustomPlatformNotifications.Create(const ANotifications: TNotifications);
begin
  inherited Create;
  FNotifications := ANotifications;
end;

destructor TCustomPlatformNotifications.Destroy;
begin
  //
  inherited;
end;

procedure TCustomPlatformNotifications.CancelAll;
begin
  //
end;

procedure TCustomPlatformNotifications.CancelNotification(const AName: string);
begin
  //
end;

procedure TCustomPlatformNotifications.PresentNotification(const ANotification: TNotification);
begin
  //
end;

procedure TCustomPlatformNotifications.ScheduleNotification(const ANotification: TNotification);
begin
  //
end;

{ TNotifications }

constructor TNotifications.Create;
begin
  inherited;
  FPlatformNotifications := TPlatformNotifications.Create(Self);
end;

destructor TNotifications.Destroy;
begin
  FPlatformNotifications.Free;
  inherited;
end;

procedure TNotifications.DoNotificationReceived(const ANotification: TNotification);
begin
  if Assigned(FOnNotificationReceived) then
    FOnNotificationReceived(Self, ANotification);
end;

function TNotifications.GetChannels: TNotificationChannels;
begin
  Result := FPlatformNotifications.Channels;
end;

procedure TNotifications.CancelAll;
begin
  FPlatformNotifications.CancelAll;
end;

procedure TNotifications.CancelNotification(const AName: string);
begin
  FPlatformNotifications.CancelNotification(AName);
end;

procedure TNotifications.PresentNotification(const ANotification: TNotification);
begin
  FPlatformNotifications.PresentNotification(ANotification);
end;

procedure TNotifications.ScheduleNotification(const ANotification: TNotification);
begin
  FPlatformNotifications.ScheduleNotification(ANotification);
end;

end.
