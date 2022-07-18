unit DW.AdMob;

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

const
  cTestAdUnitIdAppOpen = 'ca-app-pub-3940256099942544/3419835294';
  cTestAdUnitIdBanner =	'ca-app-pub-3940256099942544/6300978111';
  cTestAdUnitIdInterstitial	= 'ca-app-pub-3940256099942544/1033173712';
  cTestAdUnitIdInterstitialVideo = 'ca-app-pub-3940256099942544/8691691433';
  cTestAdUnitIdRewarded	= 'ca-app-pub-3940256099942544/5224354917';
  cTestAdUnitIdRewardedInterstitial	= 'ca-app-pub-3940256099942544/5354046379';
  cTestAdUnitIdNativeAdvanced	= 'ca-app-pub-3940256099942544/2247696110';
  cTestAdUnitIdNativeAdvancedVideo = 'ca-app-pub-3940256099942544/1044960115';

type
  TAdError = record
    ErrorCode: Integer;
    Message: string;
  end;

  TAdErrorEvent = procedure(Sender: TObject; const Error: TAdError) of object;

implementation

// May be required when using Mediation?
//{$IF Defined(IOS)}
//uses
//  DW.AdMob.iOS;
//{$ENDIF}
//{$IF Defined(Android)}
//uses
//  DW.AdMob.Android;
//{$ENDIF}

end.
