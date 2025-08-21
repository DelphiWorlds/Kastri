unit DW.HideFromDock.Mac;

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


// Including this unit in your macOS application turns it into an "accessory", i.e. it will be hidden from the Dock, as per:
//   https://stackoverflow.com/a/9220857/3164070

interface

implementation

uses
  // macOS
  Macapi.AppKit;

function SharedApplication: NSApplication;
begin
  Result := TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication);
end;

initialization
  SharedApplication.setActivationPolicy(NSApplicationActivationPolicyAccessory);

end.
