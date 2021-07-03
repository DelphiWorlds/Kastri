unit DW.UniversalLinks;

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

type
  TOpenApplicationContext = class(TObject)
  private
    FURL: string;
  public
    constructor Create(const AURL: string);
    property URL: string read FURL;
  end;

implementation

{$IF Defined(IOS)}
uses
  DW.UniversalLinks.iOS;
{$ENDIF}
{$IF Defined(ANDROID)}
uses
  DW.UniversalLinks.Android;
{$ENDIF}

{ TOpenApplicationContext }

constructor TOpenApplicationContext.Create(const AURL: string);
begin
  inherited Create;
  FURL := AURL;
end;

end.
