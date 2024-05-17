# SMS Demo

## Description

Demonstrates the use of the SMS feature in Kastri

## Supported Delphi versions

The demos should compile and work for at least versions 10.4.x, 11 and 12 - please note that you need to use SMSDemoD12 for Delphi 12, and SMSDemoD11 for Delphi 11.

## Supported platforms

Supported platforms are iOS and Android

## **Important notes about Android**

On Android, an application that targets Play Store can use the Android SMSManager class only if the app can be used as the primary SMS app on the device. [This is a query](https://github.com/DelphiWorlds/HowTo/blob/main/ChatGPTConversations/AndroidDefaultSMSApp.md) I made with ChatGPT about the conditions imposed by Play Store.

If you intend to target Play Store and your app is not intended to be a primary SMS app, you will need to set to `True` the value of the `UseIntents` property of your instance of the `TSMS` class, as per the demo. Unfortunately, when using intents to send SMS messages, the user experience can be greatly diminished.

## Test numbers

The test numbers in the demo form are Australian, and messages sent to them can be accessed [here](https://receive-sms.cc/Australia-Phone-Number/)

This service has numbers available for other countries as well

