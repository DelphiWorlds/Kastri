unit DW.Firebase.Crashlytics;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{  Contributed 2025 by Denis Hrastnik                   }
{*******************************************************}

interface

{$SCOPEDENUMS ON}

uses
  {$IF Defined(ANDROID)}
  Androidapi.JNI.JavaTypes, DW.Androidapi.JNI.FirebaseCrashlytics,
  {$ENDIF}
  System.Generics.Collections;

type
  TFirebaseCrashlytics = class;
  TCustomPlatformFirebaseCrashlytics = class(TObject)
  private
    FFirebaseCrashlytics: TFirebaseCrashlytics;
  protected
    procedure deleteUnsentReports;  virtual; abstract;
    function  didCrashOnPreviousExecution: Boolean;  virtual; abstract;
    procedure log(const Amsg: string);  virtual; abstract;
    {$IF Defined(ANDROID)}
    //procedure recordException(const Athrowable: JThrowable); virtual; abstract;
    procedure recordException(const Athrowable: string); virtual; abstract;
    {$ENDIF}
    procedure sendUnsentReports;  virtual; abstract;
    {$IF Defined(ANDROID)}
    procedure setCrashlyticsCollectionEnabled(const AEnabled: JBoolean); overload; virtual; abstract;
    {$ENDIF}
    procedure setCrashlyticsCollectionEnabled(const AEnabled: Boolean); overload; virtual; abstract;
    procedure setCustomKey(const Akey, Avalue: string); overload; virtual; abstract;
    procedure setCustomKey(const Akey: string; const Avalue: Single); overload; virtual; abstract;
    procedure setCustomKey(const Akey: string; const Avalue: Double); overload; virtual; abstract;
    procedure setCustomKey(const Akey: string; const Avalue: Boolean); overload; virtual; abstract;
    procedure setCustomKey(const Akey: string; const Avalue: Int64);  overload; virtual; abstract;
    procedure setCustomKey(const Akey: string; const Avalue: Integer); overload; virtual; abstract;
    {$IF Defined(ANDROID)}
    procedure setCustomKeys(const keysAndValues: JCustomKeysAndValues); overload; virtual; abstract;
    {$ENDIF}
    procedure setUserId(const Aidentifier: string); virtual; abstract;
  public
    constructor Create(const AFirebaseCrashlytics: TFirebaseCrashlytics); virtual;
  end;

  TFirebaseCrashlytics = class(TObject)
  private
    FPlatformFirebaseCrashlytics: TCustomPlatformFirebaseCrashlytics;
  public
    constructor Create;
    destructor Destroy; override;
    procedure deleteUnsentReports;
    function  didCrashOnPreviousExecution: Boolean;
    procedure log(const Amsg: string);
    {$IF Defined(ANDROID)}
    //procedure recordException(const Athrowable: JThrowable); overload;
    procedure recordException(const Arecordexception: string);
    {$ENDIF}
    procedure sendUnsentReports;
    {$IF Defined(ANDROID)}
    procedure setCrashlyticsCollectionEnabled(const AEnabled: JBoolean); overload;
    {$ENDIF}
    procedure setCrashlyticsCollectionEnabled(const AEnabled: Boolean); overload;
    procedure setCustomKey(const Akey, Avalue: string); overload;
    procedure setCustomKey(const Akey: string; const Avalue: Single); overload;
    procedure setCustomKey(const Akey: string; const Avalue: Double); overload;
    procedure setCustomKey(const Akey: string; const Avalue: Boolean); overload;
    procedure setCustomKey(const Akey: string; const Avalue: Int64);  overload;
    procedure setCustomKey(const Akey: string; const Avalue: Integer); overload;
    {$IF Defined(ANDROID)}
    procedure setCustomKeys(const keysAndValues: JCustomKeysAndValues); overload;
    {$ENDIF}
    procedure setUserId(const Aidentifier: string);
  end;

implementation

uses
  {$IF Defined(IOS)}
  DW.Firebase.Crashlytics.iOS;
  {$ELSEIF Defined(ANDROID)}
  DW.Firebase.Crashlytics.Android;
  {$ENDIF}

{ TCustomPlatformFirebaseCrashlytics }

constructor TCustomPlatformFirebaseCrashlytics.Create(const AFirebaseCrashlytics: TFirebaseCrashlytics);
begin
  inherited Create;
  FFirebaseCrashlytics := AFirebaseCrashlytics;
end;

{ TFirebaseCrashlytics }

constructor TFirebaseCrashlytics.Create;
begin
  inherited Create;
  FPlatformFirebaseCrashlytics := TPlatformFirebaseCrashlytics.Create(Self);
end;

destructor TFirebaseCrashlytics.Destroy;
begin
  FPlatformFirebaseCrashlytics.Free;
  inherited;
end;

procedure TFirebaseCrashlytics.deleteUnsentReports;
begin
  FPlatformFirebaseCrashlytics.deleteUnsentReports;
end;

procedure TFirebaseCrashlytics.sendUnsentReports;
begin
  FPlatformFirebaseCrashlytics.sendUnsentReports;
end;

function  TFirebaseCrashlytics.didCrashOnPreviousExecution: Boolean;
begin
  Result := FPlatformFirebaseCrashlytics.didCrashOnPreviousExecution;
end;

procedure TFirebaseCrashlytics.log(const Amsg: string);
begin
  FPlatformFirebaseCrashlytics.log(Amsg);
end;

procedure TFirebaseCrashlytics.recordException(const Arecordexception: string);
begin
  FPlatformFirebaseCrashlytics.recordException(Arecordexception);
end;

procedure TFirebaseCrashlytics.setCrashlyticsCollectionEnabled(const AEnabled: Boolean);
begin
  FPlatformFirebaseCrashlytics.setCrashlyticsCollectionEnabled(AEnabled);
end;

{$IF Defined(ANDROID)}
procedure TFirebaseCrashlytics.setCrashlyticsCollectionEnabled(const AEnabled: JBoolean);
begin
  FPlatformFirebaseCrashlytics.setCrashlyticsCollectionEnabled(AEnabled);
end;
{$ENDIF}

procedure TFirebaseCrashlytics.setCustomKey(const Akey, Avalue: string);
begin
  FPlatformFirebaseCrashlytics.setCustomKey(Akey, Avalue);
end;

procedure TFirebaseCrashlytics.setCustomKey(const Akey: string; const Avalue: Single);
begin
  FPlatformFirebaseCrashlytics.setCustomKey(Akey, Avalue);
end;

procedure TFirebaseCrashlytics.setCustomKey(const Akey: string; const Avalue: Double);
begin
  FPlatformFirebaseCrashlytics.setCustomKey(Akey, Avalue);
end;

procedure TFirebaseCrashlytics.setCustomKey(const Akey: string; const Avalue: Boolean);
begin
  FPlatformFirebaseCrashlytics.setCustomKey(Akey, Avalue);
end;

procedure TFirebaseCrashlytics.setCustomKey(const Akey: string; const Avalue: Int64);
begin
  FPlatformFirebaseCrashlytics.setCustomKey(Akey, Avalue);
end;

procedure TFirebaseCrashlytics.setCustomKey(const Akey: string; const Avalue: Integer);
begin
  FPlatformFirebaseCrashlytics.setCustomKey(Akey, Avalue);
end;

{$IF Defined(ANDROID)}
procedure TFirebaseCrashlytics.setCustomKeys(const keysAndValues: JCustomKeysAndValues);
begin
  FPlatformFirebaseCrashlytics.setCustomKeys(keysAndValues);
end;
{$ENDIF}

procedure TFirebaseCrashlytics.setUserId(const Aidentifier: string);
begin
  FPlatformFirebaseCrashlytics.setUserId(Aidentifier);
end;

end.

