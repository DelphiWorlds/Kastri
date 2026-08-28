unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls,
  DW.Firebase.Analytics, DW.Firebase.Crashlytics;

type
  TForm1 = class(TForm)
    ANRButton: TButton;
    CrashDelphiButton: TButton;
    CrashIntentButton: TButton;
    CrashReportButton: TButton;
    RuntimeCrashButton: TButton;
    ThreadButton: TButton;
    procedure ANRButtonClick(Sender: TObject);
    procedure CrashDelphiButtonClick(Sender: TObject);
    procedure CrashIntentButtonClick(Sender: TObject);
    procedure CrashReportButtonClick(Sender: TObject);
    procedure RuntimeCrashButtonClick(Sender: TObject);
    procedure ThreadButtonClick(Sender: TObject);
  private
    FAnalytics: TFirebaseAnalytics;
    FCrashlytics: TFirebaseCrashlytics;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  {$IF Defined(ANDROID)}
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes,
  Androidapi.JNIBridge, Androidapi.JNI, Androidapi.Helpers, Androidapi.JNI.Os,
  DW.Androidapi.JNI.FirebaseCrashlytics;
  {$ENDIF}

{ TForm1 }

// Delphi catches (wraps) Android native exceptions so crashlytics see them as non-fatal exceptions
// If you want to see exceptions in Crashlytics console, enclose critical code with try/except block
// and use FCrashlytics.recordException(E.Message)
//
// For testing functionality only:
// procedure TForm1.RuntimeCrashClick generate native Android exception bypassing Delphi wrapper.

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FAnalytics := TFirebaseAnalytics.Create;
  FCrashlytics := TFirebaseCrashlytics.Create;
  FCrashlytics.setCrashlyticsCollectionEnabled(True);
end;

destructor TForm1.Destroy;
begin
  FCrashlytics.Free;
  FAnalytics.Free;
  inherited;
end;

procedure TForm1.ANRButtonClick(Sender: TObject);
var
  I: Integer;
begin
  // infinite loop in Main Activity
  while True do I := 1;
end;

procedure TForm1.CrashDelphiButtonClick(Sender: TObject);
begin
  try
    raise Exception.Create('Delphi native exception');
  except
    on E: Exception do
      begin
        FCrashlytics.recordException(E.Message);
      end;
  end;
end;

procedure TForm1.CrashIntentButtonClick(Sender: TObject);
var
  LIntent: JIntent;
begin
  FCrashlytics.log('Throw NullPointer error !!!');
  LIntent := nil;
  try
    LIntent := TJIntent.JavaClass.init(LIntent);
  except
    on E: Exception do
      begin
        FCrashlytics.log('LIntent got nil value !!!');
        FCrashlytics.recordException(E.Message);
      end;
  end;
end;

procedure TForm1.CrashReportButtonClick(Sender: TObject);
var
  {$IF Defined(ANDROID)}
  KeysAndValues :JCustomKeysAndValues;
  keyBuilder: JCustomKeysAndValues_Builder;
  {$ENDIF}
begin
  FCrashlytics.log('Record exception');
  // Set custom demo_key to "Crashlytics demo key".
  FCrashlytics.setCustomKey('Crashlytics demo key', 'demo_key');
  // If you really want to use bulk of custom keys and values
  {$IF Defined(ANDROID)}
  keyBuilder := TJCustomKeysAndValues_Builder.JavaClass.init;
  KeysAndValues := TJCustomKeysAndValues.JavaClass.init(keyBuilder);
  // Add custom keys and values
  keyBuilder.putBoolean(StringToJString('Working?'),True);
  keyBuilder.putFloat(StringToJString('Float number'),1.23456);
  keyBuilder.putDouble(StringToJString('Double prec. number'),1.23456789123456);
  keyBuilder.putInt(StringToJString('Integer number'),12);
  keyBuilder.putLong(StringToJString('Int64 number'),123456789);
  keyBuilder.putString(StringToJString('String'),StringToJString('Some string'));
  keyBuilder.putString(StringToJString('Summary string'),StringToJString('CustomKeysValues are OK'));
  KeysAndValues := keyBuilder.build;
  FCrashlytics.setCustomKeys(KeysAndValues);
  {$ENDIF}
  // Record Non-Fatal error
  FCrashlytics.recordException('Crashlytic recordException');
end;

procedure TForm1.RuntimeCrashButtonClick(Sender: TObject);
begin
  {$IF Defined(ANDROID)}
  TThread.CreateAnonymousThread(procedure ()
    var
      Env: PJNIEnv;
      RuntimeExceptionClass: JNIClass;
    begin
      Env := TJNIResolver.GetJNIEnv;
      RuntimeExceptionClass := Env^.FindClass(Env, 'java/lang/RuntimeException');
      Env^.ThrowNew(Env, RuntimeExceptionClass, PAnsiChar(AnsiString('Runtime Android real crash from Delphi(async)')));
    end).Start;
   {$ENDIF}
end;

procedure TForm1.ThreadButtonClick(Sender: TObject);
begin
  {$IF Defined(ANDROID)}
  TThread.CreateAnonymousThread(procedure ()
    begin
      try
        raise Exception.Create('Delphi thread exception');
      except
        on E: Exception do
        begin
          FCrashlytics.log('Exception in thread');
          FCrashlytics.recordException(E.Message);
        end;
      end;
    end).Start;
   {$ENDIF}
end;

end.

