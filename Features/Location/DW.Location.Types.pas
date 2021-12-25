unit DW.Location.Types;

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

uses
  // RTL
  System.Sensors;

const
  cInvalidLatitude = 91;
  cInvalidLongitude = 181;

type
  TLocationDataFlag = (Accuracy, Altitude, Bearing, Speed);

  TLocationDataFlags = set of TLocationDataFlag;

  TLocationData = record
    Accuracy: Double;
    Altitude: Double;
    Bearing: Double;
    DateTime: TDateTime;
    Flags: TLocationDataFlags;
    // BearingAccuracy: Double;
    Location: TLocationCoord2D;
    Speed: Double;
  end;

  TLocationChangedEvent = procedure(Sender: TObject; const Data: TLocationData) of object;

implementation

end.
