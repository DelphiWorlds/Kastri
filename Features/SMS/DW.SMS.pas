unit DW.SMS;

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
  // DW
  DW.Types;

type
  TSMS = class;

  TMessageResult = (Cancelled, Failed, Sent);

  TCustomPlatformSMS = class(TObject)
  private
    FSMS: TSMS;
  protected
    function GetAuthorizationStatus: TAuthorizationStatus; virtual;
    procedure DoMessageResult(const ADestinations: TArray<string>; const AMessageResult: TMessageResult);
    procedure DoPermissionRequestResult(const AIsGranted: Boolean);
    procedure RequestPermission; virtual;
    procedure SendTextMessage(const AText: string; const ADestinations: TArray<string>); virtual;
    property SMS: TSMS read FSMS;
  public
    constructor Create(const ASMS: TSMS); virtual;
    destructor Destroy; override;
  end;

  TPermissionRequestResultEvent = procedure(Sender: TObject; const IsGranted: Boolean) of object;
  TMessageResultEvent = procedure(Sender: TObject; const Destinations: TArray<string>; const MessageResult: TMessageResult) of object;

  TSMS = class(TObject)
  private
    FPlatformSMS: TCustomPlatformSMS;
    FOnMessageResult: TMessageResultEvent;
    FOnPermissionRequestResult: TPermissionRequestResultEvent;
  protected
    procedure DoMessageResult(const ADestinations: TArray<string>; const AMessageResult: TMessageResult);
    procedure DoPermissionRequestResult(const AIsGranted: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    function GetAuthorizationStatus: TAuthorizationStatus;
    procedure RequestPermission;
    procedure SendTextMessage(const AText: string; const ADestinations: TArray<string>);
    property OnMessageResult: TMessageResultEvent read FOnMessageResult write FOnMessageResult;
    property OnPermissionRequestResult: TPermissionRequestResultEvent read FOnPermissionRequestResult write FOnPermissionRequestResult;
  end;

implementation

uses
  // DW
  {$IF Defined(IOS)}
  DW.SMS.iOS,
  {$ELSEIF Defined(ANDROID)}
  DW.SMS.Android,
  {$ENDIF}
  // RTL
  System.SysUtils;

{$IF not (Defined(IOS) or Defined(ANDROID))}
type
  TPlatformSMS = class(TCustomPlatformSMS);
{$ENDIF}

{ TCustomPlatformSMS }

constructor TCustomPlatformSMS.Create(const ASMS: TSMS);
begin
  inherited Create;
  FSMS := ASMS;
end;

destructor TCustomPlatformSMS.Destroy;
begin
  //
  inherited;
end;

function TCustomPlatformSMS.GetAuthorizationStatus: TAuthorizationStatus;
begin
  Result := TAuthorizationStatus.NotDetermined;
end;

procedure TCustomPlatformSMS.DoMessageResult(const ADestinations: TArray<string>; const AMessageResult: TMessageResult);
begin
  FSMS.DoMessageResult(ADestinations, AMessageResult)
end;

procedure TCustomPlatformSMS.DoPermissionRequestResult(const AIsGranted: Boolean);
begin
  FSMS.DoPermissionRequestResult(AIsGranted);
end;

procedure TCustomPlatformSMS.RequestPermission;
begin
  //
end;

procedure TCustomPlatformSMS.SendTextMessage(const AText: string; const ADestinations: TArray<string>);
begin
  //
end;

{ TSMS }

constructor TSMS.Create;
begin
  inherited;
  FPlatformSMS := TPlatformSMS.Create(Self);
end;

destructor TSMS.Destroy;
begin
  FPlatformSMS.Free;
  inherited;
end;

function TSMS.GetAuthorizationStatus: TAuthorizationStatus;
begin
  Result := FPlatformSMS.GetAuthorizationStatus;
end;

procedure TSMS.DoMessageResult(const ADestinations: TArray<string>; const AMessageResult: TMessageResult);
begin
  if Assigned(FOnMessageResult) then
    FOnMessageResult(Self, ADestinations, AMessageResult);
end;

procedure TSMS.DoPermissionRequestResult(const AIsGranted: Boolean);
begin
  if Assigned(FOnPermissionRequestResult) then
    FOnPermissionRequestResult(Self, AIsGranted);
end;

procedure TSMS.RequestPermission;
begin
  FPlatformSMS.RequestPermission;
end;

procedure TSMS.SendTextMessage(const AText: string; const ADestinations: TArray<string>);
begin
  FPlatformSMS.SendTextMessage(AText, ADestinations);
end;

end.
