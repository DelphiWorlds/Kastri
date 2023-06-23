unit DW.Sensors;

interface

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

uses
  // RTL
  System.Sensors;

type
  TLocationUsage = (Unauthorized, WhenInUse, Always);
  TLocationActivity = (Other, Automotive, Fitness, Navigation);

implementation

end.
