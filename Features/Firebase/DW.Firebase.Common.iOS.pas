unit DW.Firebase.Common.iOS;

interface

type
  TFirebaseCommon = record
  private
    class var FInitialized: Boolean;
  public
    class procedure Initialize; static;
  end;

implementation

uses
  DW.iOSapi.FirebaseCore;

{ TFirebaseCommon }

class procedure TFirebaseCommon.Initialize;
begin
  if not FInitialized then
    TFIRApp.OCClass.configure;
  FInitialized := True;
end;

end.
