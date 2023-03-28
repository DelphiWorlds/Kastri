unit DW.iOSapi.MediaPlayerExtra;

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
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.MediaPlayer, iOSapi.CoreGraphics, iOSapi.UIKit;

const
  MPNowPlayingInfoMediaTypeNone = 0;
  MPNowPlayingInfoMediaTypeAudio = 1;
  MPNowPlayingInfoMediaTypeVideo = 2;
  MPNowPlayingPlaybackStateUnknown = 0;
  MPNowPlayingPlaybackStatePlaying = 1;
  MPNowPlayingPlaybackStatePaused = 2;
  MPNowPlayingPlaybackStateStopped = 3;
  MPNowPlayingPlaybackStateInterrupted = 4;
  MPNowPlayingInfoLanguageOptionTypeAudible = 0;
  MPNowPlayingInfoLanguageOptionTypeLegible = 1;
  MPShuffleTypeOff = 0;
  MPShuffleTypeItems = 1;
  MPShuffleTypeCollections = 2;
  MPRepeatTypeOff = 0;
  MPRepeatTypeOne = 1;
  MPRepeatTypeAll = 2;
  MPRemoteCommandHandlerStatusSuccess = 0;
  MPRemoteCommandHandlerStatusNoSuchContent = 100;
  MPRemoteCommandHandlerStatusNoActionableNowPlayingItem = 110;
  MPRemoteCommandHandlerStatusDeviceNotFound = 120;
  MPRemoteCommandHandlerStatusCommandFailed = 200;

type
  MPRemoteCommandEvent = interface;
  MPRemoteCommand = interface;
  MPFeedbackCommand = interface;
  MPChangePlaybackPositionCommand = interface;
  MPChangePlaybackRateCommand = interface;
  MPChangeShuffleModeCommand = interface;
  MPChangeRepeatModeCommand = interface;
  MPRatingCommand = interface;
  MPSkipIntervalCommandClass = interface;
  MPRemoteCommandCenter = interface;
  MPNowPlayingInfoCenter = interface;

  MPShuffleType = NSInteger;
  MPRepeatType = NSInteger;
  MPNowPlayingInfoMediaType = NSInteger;
  MPNowPlayingPlaybackState = NSInteger;
  MPNowPlayingInfoLanguageOptionType = NSInteger;
  MPRemoteCommandHandlerStatus = NSInteger;

  TMPRemoteCommandBlockMethod1 = procedure(event: MPRemoteCommandEvent) of object;

  MPRemoteCommandEventClass = interface(NSObjectClass)
    ['{9719A2B5-3507-4DB8-9174-166FEFE3DBE6}']
  end;

  MPRemoteCommandEvent = interface(NSObject)
    ['{4BD45FF8-5BE9-4221-B615-71540212A5D1}']
    function command: MPRemoteCommand; cdecl;
    function timestamp: NSTimeInterval; cdecl;
  end;
  TMPRemoteCommandEvent = class(TOCGenericImport<MPRemoteCommandEventClass, MPRemoteCommandEvent>) end;

  MPRemoteCommandClass = interface(NSObjectClass)
    ['{BDF6AC37-F66E-40FC-ACE2-318A46F905BB}']
  end;

  MPRemoteCommand = interface(NSObject)
    ['{EB26F4E7-D62C-4554-9C9E-EE75E75AF34C}']
    [MethodName('addTarget:action:')]
    procedure addTarget(target: Pointer; action: Pointer); cdecl;
    function addTargetWithHandler(handler: TMPRemoteCommandBlockMethod1): Pointer; cdecl;
    function isEnabled: Boolean; cdecl;
    [MethodName('removeTarget:action:')]
    procedure removeTarget(target: Pointer; action: Pointer); overload; cdecl;
    procedure removeTarget(target: Pointer); overload; cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
  end;
  TMPRemoteCommand = class(TOCGenericImport<MPRemoteCommandClass, MPRemoteCommand>) end;

  MPFeedbackCommandClass = interface(MPRemoteCommandClass)
    ['{1BD981CD-996D-4694-8987-895F1DC05D75}']
  end;

  MPFeedbackCommand = interface(MPRemoteCommand)
    ['{23B7506D-6EF2-49AB-8EEF-20B3B989300D}']
    function isActive: Boolean; cdecl;
    function localizedShortTitle: NSString; cdecl;
    function localizedTitle: NSString; cdecl;
    procedure setActive(active: Boolean); cdecl;
    procedure setLocalizedShortTitle(localizedShortTitle: NSString); cdecl;
    procedure setLocalizedTitle(localizedTitle: NSString); cdecl;
  end;
  TMPFeedbackCommand = class(TOCGenericImport<MPFeedbackCommandClass, MPFeedbackCommand>) end;

  MPChangePlaybackPositionCommandClass = interface(MPRemoteCommandClass)
    ['{597B2FAB-49D8-4461-8295-B1C1D458DEBB}']
  end;

  MPChangePlaybackPositionCommand = interface(MPRemoteCommand)
    ['{B12B2689-B61E-458E-9083-523D659E77EC}']
  end;
  TMPChangePlaybackPositionCommand = class(TOCGenericImport<MPChangePlaybackPositionCommandClass, MPChangePlaybackPositionCommand>) end;

  MPChangePlaybackRateCommandClass = interface(MPRemoteCommandClass)
    ['{B99EEF13-16E5-4544-A4D0-C21F7F075FD1}']
  end;

  MPChangePlaybackRateCommand = interface(MPRemoteCommand)
    ['{BBE66BDE-A6DE-4F3A-BE89-0518BADCAF53}']
    procedure setSupportedPlaybackRates(supportedPlaybackRates: NSArray); cdecl;
    function supportedPlaybackRates: NSArray; cdecl;
  end;
  TMPChangePlaybackRateCommand = class(TOCGenericImport<MPChangePlaybackRateCommandClass, MPChangePlaybackRateCommand>) end;

  MPChangeShuffleModeCommandClass = interface(MPRemoteCommandClass)
    ['{0EDF0538-CD7C-4A6C-9480-485AE25F0A34}']
  end;

  MPChangeShuffleModeCommand = interface(MPRemoteCommand)
    ['{CA7008BD-0CB9-48E2-8EB9-2A4B5B5C296D}']
    function currentShuffleType: MPShuffleType; cdecl;
    procedure setCurrentShuffleType(currentShuffleType: MPShuffleType); cdecl;
  end;
  TMPChangeShuffleModeCommand = class(TOCGenericImport<MPChangeShuffleModeCommandClass, MPChangeShuffleModeCommand>) end;

  MPChangeRepeatModeCommandClass = interface(MPRemoteCommandClass)
    ['{C0E1E6AE-4A19-463B-9591-38D83E05D0F4}']
  end;

  MPChangeRepeatModeCommand = interface(MPRemoteCommand)
    ['{E7089E3B-DD33-473A-9D2D-BD717B13A7B3}']
    function currentRepeatType: MPRepeatType; cdecl;
    procedure setCurrentRepeatType(currentRepeatType: MPRepeatType); cdecl;
  end;
  TMPChangeRepeatModeCommand = class(TOCGenericImport<MPChangeRepeatModeCommandClass, MPChangeRepeatModeCommand>) end;

  MPRatingCommandClass = interface(MPRemoteCommandClass)
    ['{BC1BF7AF-9626-4247-930F-EB5024395C9A}']
  end;

  MPRatingCommand = interface(MPRemoteCommand)
    ['{EA88C59E-D107-4A43-B4A2-4A59698A82D6}']
    function maximumRating: Single; cdecl;
    function minimumRating: Single; cdecl;
    procedure setMaximumRating(maximumRating: Single); cdecl;
    procedure setMinimumRating(minimumRating: Single); cdecl;
  end;
  TMPRatingCommand = class(TOCGenericImport<MPRatingCommandClass, MPRatingCommand>) end;

  MPSkipIntervalCommandClass = interface(MPRemoteCommandClass)
    ['{F6A2006F-202E-4E4C-9212-B84F1A077E8C}']
  end;

  MPSkipIntervalCommand = interface(MPRemoteCommand)
    ['{D793E042-5A08-4585-85DE-A719BE753B24}']
    function preferredIntervals: NSArray; cdecl;
    procedure setPreferredIntervals(preferredIntervals: NSArray); cdecl;
  end;
  TMPSkipIntervalCommand = class(TOCGenericImport<MPSkipIntervalCommandClass, MPSkipIntervalCommand>) end;

  MPRemoteCommandCenterClass = interface(NSObjectClass)
    ['{7658F7C0-1CE0-433C-9D99-0ECA92E30EED}']
    {class} function sharedCommandCenter: MPRemoteCommandCenter; cdecl;
  end;

  MPRemoteCommandCenter = interface(NSObject)
    ['{6AA14E0C-3E0D-4F47-B10B-6BA7AFA8E16C}']
    function bookmarkCommand: MPFeedbackCommand; cdecl;
    function changePlaybackPositionCommand: MPChangePlaybackPositionCommand; cdecl;
    function changePlaybackRateCommand: MPChangePlaybackRateCommand; cdecl;
    function changeRepeatModeCommand: MPChangeRepeatModeCommand; cdecl;
    function changeShuffleModeCommand: MPChangeShuffleModeCommand; cdecl;
    function disableLanguageOptionCommand: MPRemoteCommand; cdecl;
    function dislikeCommand: MPFeedbackCommand; cdecl;
    function enableLanguageOptionCommand: MPRemoteCommand; cdecl;
    function likeCommand: MPFeedbackCommand; cdecl;
    function nextTrackCommand: MPRemoteCommand; cdecl;
    function pauseCommand: MPRemoteCommand; cdecl;
    function playCommand: MPRemoteCommand; cdecl;
    function previousTrackCommand: MPRemoteCommand; cdecl;
    function ratingCommand: MPRatingCommand; cdecl;
    function seekBackwardCommand: MPRemoteCommand; cdecl;
    function seekForwardCommand: MPRemoteCommand; cdecl;
    function skipBackwardCommand: MPSkipIntervalCommand; cdecl;
    function skipForwardCommand: MPSkipIntervalCommand; cdecl;
    function stopCommand: MPRemoteCommand; cdecl;
    function togglePlayPauseCommand: MPRemoteCommand; cdecl;
  end;
  TMPRemoteCommandCenter = class(TOCGenericImport<MPRemoteCommandCenterClass, MPRemoteCommandCenter>) end;

  MPNowPlayingInfoCenterClass = interface(NSObjectClass)
    ['{63CCCE61-F24D-43A5-90EE-530DABFC859E}']
    {class} function defaultCenter: MPNowPlayingInfoCenter; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  MPNowPlayingInfoCenter = interface(NSObject)
    ['{42A7A6EF-5A9A-4703-A519-360A8392C7D4}']
    function nowPlayingInfo: NSDictionary; cdecl;
    function playbackState: MPNowPlayingPlaybackState; cdecl;
    procedure setNowPlayingInfo(nowPlayingInfo: NSDictionary); cdecl;
    procedure setPlaybackState(playbackState: MPNowPlayingPlaybackState); cdecl;
  end;
  TMPNowPlayingInfoCenter = class(TOCGenericImport<MPNowPlayingInfoCenterClass, MPNowPlayingInfoCenter>) end;

  TMPMediaItemArtworkBlockMethod1 = function(size: CGSize): UIImage of object;

  MPMediaItemArtworkClass = interface(NSObjectClass)
    ['{7513F1AF-D5CF-43F9-9B04-627E1B9EAA4A}']
  end;

  MPMediaItemArtwork = interface(NSObject)
    ['{FAE1DECB-5F06-4FA8-AA7B-48B4C22AB4B0}']
    function bounds: CGRect; cdecl;
    function imageCropRect: CGRect; cdecl;
    function imageWithSize(size: CGSize): UIImage; cdecl;
    [MethodName('initWithBoundsSize:requestHandler:')]
    function initWithBoundsSize(boundsSize: CGSize; requestHandler: TMPMediaItemArtworkBlockMethod1): Pointer; cdecl;
    function initWithImage(image: UIImage): Pointer; cdecl;
  end;
  TMPMediaItemArtwork = class(TOCGenericImport<MPMediaItemArtworkClass, MPMediaItemArtwork>) end;

function MPMediaItemPropertyArtist: NSString;
function MPMediaItemPropertyArtwork: NSString;
function MPMediaItemPropertyTitle: NSString;

implementation

function MPMediaItemPropertyArtist: NSString;
begin
  Result := CocoaNSStringConst(libMediaPlayer, 'MPMediaItemPropertyArtist');
end;

function MPMediaItemPropertyArtwork: NSString;
begin
  Result := CocoaNSStringConst(libMediaPlayer, 'MPMediaItemPropertyArtwork');
end;

function MPMediaItemPropertyTitle: NSString;
begin
  Result := CocoaNSStringConst(libMediaPlayer, 'MPMediaItemPropertyTitle');
end;

end.
