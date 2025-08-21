unit DW.Sensors;

interface

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$SCOPEDENUMS ON}

uses
  // RTL
  System.Sensors;

type
  TLocationUsage = (Unauthorized, WhenInUse, Always);
  TLocationActivity = (Other, Automotive, Fitness, Navigation);

implementation

end.
