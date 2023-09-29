# ShareItems Demo

## Description

ShareItems support is intended as an alternative for ShareSheet actions in Delphi

It allows you to share text, images, and files, and the number being shared is restricted only by the OS.

The implementation for the supported platforms is located in the [Features\ShareItems](https://github.com/DelphiWorlds/Kastri/tree/master/Features/ShareItems) folder of Kastri.

## Adding items

Create an instance of TShareItems and call one of AddFile, AddImage or AddText for each item to be shared.

## Executing sharing of items

Call the Share method to share the items that have been added

On iOS, you can restrict which activities can be shared to by using the AExcludedActivities parameter. At present these are:

* Facebook
* Twitter
* Message
* Mail
* Printer
* Pasteboard
* Contacts
* Camera Roll
* Reading List
* Flickr
* Vimeo
* Weibo
* Tencent Weibo
* AirDrop
* IBooks
* PDF

## Handling the result

If you wish to know the result of sharing items, assign a handler for the OnShareCompleted property. Note that this is applicable to **iOS only**.




