# DW.FCMSender unit

## Description

This unit is the "engine room" of the push notification sending code. Those familiar with the [PushIt application](https://github.com/DelphiWorlds/PushIt) will notice that it contains some of the same code, and soon PushIt will be updated to use this unit.

As per the comments at the head of the unit, it makes use of [OpenSSL support provided by Grijjy](https://github.com/grijjy/DelphiOpenSsl), and will **require the relevant unit (OpenSSL.Api_11.pas) and binaries** provided by them.

## Setup of your project in Firebase Console

If you already have FCM support in your client applications, then you will have a project in Firebase Console. The code here is based around the [HTTP v1 API](https://firebase.google.com/docs/cloud-messaging/http-server-ref) (as opposed to the [legacy HTTP API](https://firebase.google.com/docs/cloud-messaging/migrate-v1)), and in order to use the (newer) API you will need to create a service account. You do this by:

1. Logging in to [Firebase Console](https://console.firebase.google.com)
2. Select the project you have FCM configured for
3. Click the Settings icon (a "cog" in the top left) and click Project Settings
4. Click the Service Accounts tab heading
5. Click the Generate New Private Key button
6. Click Generate Key
7. Save the json file to somewhere secure, as it has information private to your project

## Usage

Create an instance of `TFCMSender`, and call `LoadServiceAccount` to load the json file mentioned in the steps above. This method returns `True` if it successfully parsed the JSON.

Create an instance of `TFCMMessage`, and populate the relevant properties. Most commonly this will be `Title` and `Body`. To send the message, call the `Post` method of the `TFCMSender` instance, passing as the parameter the result of either the `GetTopicPayload` or `GetTokenPayload` method of the `TFCMMessage` instance.

## Payload methods

`GetTopicPayload` returns a message payload that contains a topic as the destination. Client apps that have called [`SubscribeToTopic`](https://github.com/DelphiWorlds/Kastri/blob/3c4ac0a9c2bc622f6bd5a4a78dd07b4076a81249/Features/Firebase/DW.Firebase.Messaging.pas#L80) with a matching topic name will receive the message, if the client app has the same package identifier as the server is using.

`GetTokenPayload` returns a message payload that contains a token as the destination. Use this method if you are targeting a specific device. Usually, your server side code will have logic that associates a token with an identified user, ideally sent by the client app at the appropriate time (such as a login).

## Events

### OnNewAccessToken

This event is fired when the OAuth2 call (`DoGetAccessToken`) returns and there is a new token. The intention is for the token to be persisted (e.g. save the JSON to a file) so that it can be re-read when the next message is posted. The token information is in the `BearerToken` property. The `SaveToFile` method of `BearerToken` can be used to save the token information to a file, and the `LoadFromFile` method will read that information back in to the `BearerToken` property.

TFCMSender handles requesting a new token if one is needed

### OnError

Use this event to handle either networking or HTTP response errors

## Example code

```
  LFCMSender := TFCMSender.Create;
  try
    LFCMSender.LoadServiceAccount('C:\FCMConfig\delphi-worlds-test-firebase-adminsdk-jfxoy-999d333222.json');
    LMessage := TFCMMessage.Create;
    try
      LMessage.Title := 'Test';
      LMessage.Body := 'This is a test message';
      LFCMSender.Post(LMessage.GetTopicPayload('Kastri'));
    finally
      LMessage.Free;
    end;
  finally
    LFCMSender.Free;
  end;
```