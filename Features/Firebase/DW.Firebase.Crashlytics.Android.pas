unit DW.Firebase.Crashlytics.Android;

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

uses
  // Android
  Androidapi.JNI.Os, Androidapi.JNI.JavaTypes,
  // DW
  DW.Firebase.Crashlytics, DW.Androidapi.JNI.FirebaseCrashlytics;

type
  TPlatformFirebaseCrashlytics = class(TCustomPlatformFirebaseCrashlytics)
  private
    FCrashlytics: JFirebaseCrashlytics;
  protected
    procedure deleteUnsentReports; override;
    function  didCrashOnPreviousExecution: Boolean; override;
    procedure log(const Amsg: string); override;
    //procedure recordException(const Athrowable: JThrowable); override;
    procedure recordException(const Aexception:string); override;
    procedure sendUnsentReports; override;
    procedure setCrashlyticsCollectionEnabled(const AEnabled: Boolean); overload; override;
    procedure setCrashlyticsCollectionEnabled(const AEnabled: JBoolean); overload; override;
    procedure setCustomKey(const Akey, Avalue: string); overload; override;
    procedure setCustomKey(const Akey: string; const Avalue: Single); overload; override;
    procedure setCustomKey(const Akey: string; const Avalue: Double); overload; override;
    procedure setCustomKey(const Akey: string; const Avalue: Boolean); overload; override;
    procedure setCustomKey(const Akey: string; const Avalue: Int64);  overload; override;
    procedure setCustomKey(const Akey: string; const Avalue: Integer); overload; override;
    procedure setCustomKeys(const keysAndValues: JCustomKeysAndValues); override;
    procedure setUserId(const Aidentifier: string); override;
  public
    constructor Create(const AFirebaseCrashlytics: TFirebaseCrashlytics); override;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.Rtti, System.Generics.Collections,
  // Android
  Androidapi.Helpers;

{ TPlatformFirebaseCrashlytics }

constructor TPlatformFirebaseCrashlytics.Create(const AFirebaseCrashlytics: TFirebaseCrashlytics);
begin
  inherited;
  FCrashlytics := TJFirebaseCrashlytics.JavaClass.getInstance();
end;

procedure TPlatformFirebaseCrashlytics.deleteUnsentReports();
begin
  FCrashlytics.deleteUnsentReports();
end;

function TPlatformFirebaseCrashlytics.didCrashOnPreviousExecution(): Boolean;
begin
  Result := FCrashlytics.didCrashOnPreviousExecution;
end;

procedure TPlatformFirebaseCrashlytics.log(const Amsg: string);
begin
  FCrashlytics.log(StringToJString(Amsg));
end;

procedure TPlatformFirebaseCrashlytics.recordException(const Aexception: string);
begin
  FCrashlytics.recordException(TJException.JavaClass.init(StringToJString(Aexception)));
end;

procedure TPlatformFirebaseCrashlytics.sendUnsentReports();
begin
  FCrashlytics.sendUnsentReports();
end;

procedure TPlatformFirebaseCrashlytics.setCrashlyticsCollectionEnabled(const AEnabled: Boolean);
begin
  FCrashlytics.setCrashlyticsCollectionEnabled(AEnabled);
end;

procedure TPlatformFirebaseCrashlytics.setCrashlyticsCollectionEnabled(const AEnabled: JBoolean);
begin
  FCrashlytics.setCrashlyticsCollectionEnabled(AEnabled);
end;

procedure TPlatformFirebaseCrashlytics.setCustomKey(const Akey: string; const Avalue: string);
begin
  FCrashlytics.setCustomKey(StringToJString(Akey), StringToJString(Avalue));
end;

procedure TPlatformFirebaseCrashlytics.setCustomKey(const Akey: string; const Avalue: Single);
begin
  FCrashlytics.setCustomKey(StringToJString(Akey), Avalue);
end;

procedure TPlatformFirebaseCrashlytics.setCustomKey(const Akey: string; const Avalue: Double);
begin
  FCrashlytics.setCustomKey(StringToJString(Akey), Avalue);
end;

procedure TPlatformFirebaseCrashlytics.setCustomKey(const Akey: string; const Avalue: Boolean);
begin
  FCrashlytics.setCustomKey(StringToJString(Akey), Avalue);
end;

procedure TPlatformFirebaseCrashlytics.setCustomKey(const Akey: string; const Avalue: Int64);
begin
  FCrashlytics.setCustomKey(StringToJString(Akey), Avalue);
end;

procedure TPlatformFirebaseCrashlytics.setCustomKey(const Akey: string; const Avalue: Integer);
begin
  FCrashlytics.setCustomKey(StringToJString(Akey), Avalue);
end;

procedure TPlatformFirebaseCrashlytics.setCustomKeys(const keysAndValues: JCustomKeysAndValues);
begin
  FCrashlytics.setCustomKeys(keysAndValues);
end;

procedure TPlatformFirebaseCrashlytics.setUserId(const Aidentifier: string);
begin
  FCrashlytics.setUserId(StringToJString(Aidentifier));
end;

end.
