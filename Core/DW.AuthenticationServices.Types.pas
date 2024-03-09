unit DW.AuthenticationServices.Types;

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

uses
  // RTL
  System.Messaging;

type
  TAppleIDAuthorizationFailReason = (None, Unknown, Canceled, Failed, InvalidResponse, NotHandled);
  TAppleIDAuthorizationCredential = (Invalid, AppleID, Password);
  // TAppleIDUserDetectionRealUserStatus = (Unknown, LikelyReal, Unsupported);

  TAppleIDCredentials = record
    AuthorizationCode: string;
    Credential: TAppleIDAuthorizationCredential;
    Email: string;
    Password: string;
    User: string;
    GivenName: string;
    FamilyName: string;
  end;

  TAppleIDAuthorizationResponse = record
    Credentials: TAppleIDCredentials;
    FailReason: TAppleIDAuthorizationFailReason;
    function IsAuthorized: Boolean;
  end;

  TAppleIDAuthorizationResponseMessage = TMessage<TAppleIDAuthorizationResponse>;

implementation

{ TAppleIDAuthorizationResponse }

function TAppleIDAuthorizationResponse.IsAuthorized: Boolean;
begin
  Result := FailReason = TAppleIDAuthorizationFailReason.None;
end;

end.
