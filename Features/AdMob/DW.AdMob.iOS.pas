unit DW.AdMob.iOS;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2021 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

implementation

uses
  // DW
  DW.iOSapi.GoogleMobileAds;

initialization
  TGADMobileAds.OCClass.sharedInstance.startWithCompletionHandler(nil);

end.
