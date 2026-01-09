unit DW.FirebaseApp.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2026 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

type
  TPlatformFirebaseApp = class(TObject)
  private
    class var FStarted: Boolean;
  public
    class procedure Start;
  end;

implementation

uses
  // Android
  Androidapi.Helpers,
  // DW
  DW.Androidapi.JNI.Firebase;

{ TPlatformFirebaseApp }

class procedure TPlatformFirebaseApp.Start;
begin
  if not FStarted then
  begin
    TJFirebaseApp.JavaClass.initializeApp(TAndroidHelper.Context);
    FStarted := True;
  end;
end;

end.
