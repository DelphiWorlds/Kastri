unit DW.Messaging;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2023 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  System.Messaging, System.Types;

type
  TComboBoxCancelMessage = class(TMessage);
  TOrientationDidChangeMessage = class(TMessage);
  TOrientationWillChangeMessage = class(TMessage);
  TVirtualKeyboardRectChangeMessage = class(TMessage<TRect>);
  TWindowFocusChangedMessage = TMessage<Boolean>;

implementation

end.
