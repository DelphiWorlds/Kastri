unit DW.OSMetadata.Mac;

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
  {$IF Defined(IOS)}
  // iOS
  iOSapi.Foundation;
  {$ELSEIF Defined(MACOS)}
  // macOS
  Macapi.Foundation;
  {$ENDIF}

type
  TPlatformOSMetadata = record
  private
    class var FMetadata: NSBundle;
    class function GetMetadata: NSBundle; static;
    class function InternalGetValue(const AKey: string): Pointer; static;
  public
    class function ContainsKey(const AKey: string): Boolean; static;
    class function GetValue(const AKey: string; var AValue: string): Boolean; static;
  end;

implementation

uses
  // macOS
  Macapi.Helpers;

{ TPlatformOSMetadata }

class function TPlatformOSMetadata.GetMetadata: NSBundle;
begin
  if FMetadata = nil then
    FMetadata := TNSBundle.Wrap(TNSBundle.OCClass.mainBundle);
  Result := FMetadata;
end;

class function TPlatformOSMetadata.InternalGetValue(const AKey: string): Pointer;
begin
  Result := GetMetadata.infoDictionary.valueForKey(StrToNSStr(AKey));
end;

class function TPlatformOSMetadata.ContainsKey(const AKey: string): Boolean;
begin
  Result := InternalGetValue(AKey) <> nil;
end;

class function TPlatformOSMetadata.GetValue(const AKey: string; var AValue: string): Boolean;
var
  LValue: Pointer;
begin
  LValue := InternalGetValue(AKey);
  Result := LValue <> nil;
  if Result then
    AValue := NSStrToStr(TNSString.Wrap(InternalGetValue(AKey)));
end;

end.