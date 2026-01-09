unit DW.AppReview;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2026 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

type
  IAppReview = interface(IInterface)
    ['{BEAB3513-1687-4868-969B-940FB31470E2}']
    procedure RequestReview;
  end;

var
  AppReview: IAppReview;

implementation

// DW
{$IF Defined(IOS)}
uses
  DW.AppReview.iOS;
{$ELSEIF Defined(ANDROID)}
uses
  DW.AppReview.Android;
{$ELSE}

type
  TPlatformAppReview = class(TInterfacedObject, IAppReview)
  public
    { IAppReview }
    procedure RequestReview;
  end;

procedure TPlatformAppReview.RequestReview;
begin
  //
end;

initialization
  AppReview := TPlatformAppReview.Create;
{$ENDIF}

end.
