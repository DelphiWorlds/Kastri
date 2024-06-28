unit DW.Firebase.Common.iOS;

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

type
  TFirebaseCommon = record
  private
    class var IsConfigured: Boolean;
  public
    class procedure Configure; static;
  end;

implementation

uses
  // DW
  DW.iOSapi.FirebaseCore;

{ TFirebaseCommon }

class procedure TFirebaseCommon.Configure;
begin
  if not IsConfigured then
  begin
    TFIRApp.OCClass.configure;
    IsConfigured := True;
  end;
end;

end.
