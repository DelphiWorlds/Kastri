unit DW.Firebase.Default;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{    Copyright 2020 Dave Nottage under MIT license      }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  DW.Firebase.Messaging;

type
  TPlatformFirebaseMessaging = class(TCustomPlatformFirebaseMessaging)
  protected
    procedure Connect; override;
    procedure Disconnect; override;
    procedure RequestAuthorization; override;
    procedure SubscribeToTopic(const ATopicName: string); override;
    procedure UnsubscribeFromTopic(const ATopicName: string); override;
  end;

implementation

{ TPlatformFirebaseMessaging }

procedure TPlatformFirebaseMessaging.Connect;
begin
  //
end;

procedure TPlatformFirebaseMessaging.Disconnect;
begin
  //
end;

procedure TPlatformFirebaseMessaging.RequestAuthorization;
begin
  //
end;

procedure TPlatformFirebaseMessaging.SubscribeToTopic(const ATopicName: string);
begin
  //
end;

procedure TPlatformFirebaseMessaging.UnsubscribeFromTopic(const ATopicName: string);
begin
  //
end;

end.
