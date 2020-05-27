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

// This include file contains defines that are application-wide for Delphi Worlds code

// All Delphi Worlds code makes use of scoped enumerations
{$SCOPEDENUMS ON}

// Special define to indicate the code is being compiled for an actual Mac device
{$IF Defined(MACOS) and not Defined(IOS)}
  {$DEFINE MACDEV}
{$ENDIF}

// Special defines to indicate the code is being compiled for an actual iOS device, or for simulator
{$IF Defined(IOS)}
  {$IF Defined(CPUARM)}
    {$DEFINE IOSDEV}
  {$ELSE}
    {$DEFINE IOSSIM}
  {$ENDIF}
{$ENDIF}

// Indicates whether the app is being compiled for a mobile platform
{$IF Defined(IOS) or Defined(ANDROID) or Defined(ANDROID64)}
  {$DEFINE MOBILE}
{$ENDIF}



