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

implementation

uses
  // DW
  DW.iOSapi.FirebaseCore;

initialization
  TFIRApp.OCClass.configure;

end.
