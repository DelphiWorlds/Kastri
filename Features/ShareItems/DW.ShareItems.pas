unit DW.ShareItems;

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
  System.Generics.Collections,
  // FMX
  FMX.Graphics, FMX.Controls;

type
  TSharingItem = class(TObject);

  TSharingItemTextBase = class(TSharingItem)
  private
    FText: string;
  public
    constructor Create(const AText: string);
    property Text: string read FText;
  end;

  TSharingItemText = class(TSharingItemTextBase);

  TSharingItemFile = class(TSharingItemTextBase);

  TSharingItemImage = class(TSharingItem)
  private
    FImage: TBitmap;
  public
    constructor Create(const AImage: TBitmap);
    destructor Destroy; override;
    property Image: TBitmap read FImage;
  end;

  TSharingItems = class(TObjectList<TSharingItem>);

  TShareItems = class;

  TShareActivity = (Unknown, PostToFacebook, PostToTwitter, PostToWeibo, Message, Mail, Print, CopyToPasteboard, AssignToContact, SaveToCameraRoll,
    AddToReadingList, PostToFlickr, PostToVimeo, PostToTencentWeibo, AirDrop, OpenInIBooks, MarkupAsPDF, None);

  TShareActivities = set of TShareActivity;

  TCustomPlatformShareItems = class(TObject)
  private
    FShareItems: TShareItems;
    function GetItems: TSharingItems;
  protected
    procedure DoShareCompleted(const AActivity: TShareActivity; const AError: string);
    procedure Share(const AControl: TControl; const AExcludedActivities: TShareActivities); virtual;
    property Items: TSharingItems read GetItems;
    property ShareItems: TShareItems read FShareItems;
  public
    constructor Create(const AShareItems: TShareItems); virtual;
    destructor Destroy; override;
  end;

  TShareCompletedEvent = procedure(Sender: TObject; const Activity: TShareActivity; const Error: string) of object;

  TShareItems = class(TObject)
  private
    FItems: TSharingItems;
    FPlatformShareItems: TCustomPlatformShareItems;
    FOnShareCompleted: TShareCompletedEvent;
  protected
    procedure DoShareCompleted(const AActivity: TShareActivity; const AError: string);
    property Items: TSharingItems read FItems;
  public
    class function AllShareActivities: TShareActivities;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddFile(const AFileName: string);
    procedure AddImage(const AImage: TBitmap);
    procedure AddText(const AText: string);
    procedure Clear;
    procedure Share(const AControl: TControl); overload;
    procedure Share(const AControl: TControl; const AExcludedActivities: TShareActivities); overload;
    property OnShareCompleted: TShareCompletedEvent read FOnShareCompleted write FOnShareCompleted;
  end;

implementation

{$IF Defined(IOS)}
uses
  DW.ShareItems.iOS;
{$ELSEIF Defined(ANDROID)}
uses
  DW.ShareItems.Android;
{$ENDIF}

{$IF not Defined(IOS) or Defined(ANDROID)}
type
  TPlatformShareItems = class(TCustomPlatformShareItems);
{$ENDIF}

{ TSharingItemTextBase }

constructor TSharingItemTextBase.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
end;

{ TSharingItemImage }

constructor TSharingItemImage.Create(const AImage: TBitmap);
begin
  inherited Create;
  FImage := TBitmap.Create;
  FImage.Assign(AImage);
end;

destructor TSharingItemImage.Destroy;
begin
  FImage.Free;
  inherited;
end;

{ TCustomPlatformShareItems }

constructor TCustomPlatformShareItems.Create(const AShareItems: TShareItems);
begin
  inherited Create;
  FShareItems := AShareItems;
end;

destructor TCustomPlatformShareItems.Destroy;
begin
  //
  inherited;
end;

procedure TCustomPlatformShareItems.DoShareCompleted(const AActivity: TShareActivity; const AError: string);
begin
  FShareItems.DoShareCompleted(AActivity, AError);
end;

function TCustomPlatformShareItems.GetItems: TSharingItems;
begin
  Result := FShareItems.Items;
end;

procedure TCustomPlatformShareItems.Share(const AControl: TControl; const AExcludedActivities: TShareActivities);
begin
  //
end;

{ TShareItems }

constructor TShareItems.Create;
begin
  inherited;
  FPlatformShareItems := TPlatformShareItems.Create(Self);
  FItems := TSharingItems.Create;
end;

destructor TShareItems.Destroy;
begin
  FPlatformShareItems.Free;
  FItems.Free;
  inherited;
end;

class function TShareItems.AllShareActivities: TShareActivities;
var
  LActivity: TShareActivity;
begin
  Result := [];
  for LActivity := Succ(Low(TShareActivity)) to Pred(High(TShareActivity)) do
    Include(Result, LActivity);
end;

procedure TShareItems.DoShareCompleted(const AActivity: TShareActivity; const AError: string);
begin
  if Assigned(FOnShareCompleted) then
    FOnShareCompleted(Self, AActivity, AError);
end;

procedure TShareItems.Share(const AControl: TControl; const AExcludedActivities: TShareActivities);
begin
  FPlatformShareItems.Share(AControl, AExcludedActivities);
end;

procedure TShareItems.Share(const AControl: TControl);
begin
  FPlatformShareItems.Share(AControl, []);
end;

procedure TShareItems.AddFile(const AFileName: string);
begin
  FItems.Add(TSharingItemFile.Create(AFileName));
end;

procedure TShareItems.AddImage(const AImage: TBitmap);
begin
  FItems.Add(TSharingItemImage.Create(AImage));
end;

procedure TShareItems.AddText(const AText: string);
begin
  FItems.Add(TSharingItemText.Create(AText));
end;

procedure TShareItems.Clear;
begin
  FItems.Clear;
end;

end.
