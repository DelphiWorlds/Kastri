unit DW.Messaging;

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
  System.Messaging, System.Types;

type
  TOrientationDidChangeMessage = class(TMessage);
  TOrientationWillChangeMessage = class(TMessage);
  TVirtualKeyboardRectChangeMessage = class(TMessage<TRect>);

implementation

end.
