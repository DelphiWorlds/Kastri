unit DW.Types;

interface

type
  TAuthorizationStatus = (NotDetermined, Restricted, Denied, Authorized);
  TAuthorizationStatusProc = reference to procedure(const Status: TAuthorizationStatus);
  TAuthorizationStatusEvent = procedure(Sender: TObject; const Status: TAuthorizationStatus) of object;

implementation

end.
