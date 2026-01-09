unit DW.AppReview.iOS;

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

implementation

uses
  // DW
  DW.iOSapi.StoreKit, DW.AppReview;

type
  TPlatformAppReview = class(TInterfacedObject, IAppReview)
  public
    { IAppReview }
    procedure RequestReview;
  end;

{ TPlatformAppReview }

procedure TPlatformAppReview.RequestReview;
begin
  TSKStoreReviewController.OCClass.requestReview;
end;

initialization
  AppReview := TPlatformAppReview.Create;

end.
