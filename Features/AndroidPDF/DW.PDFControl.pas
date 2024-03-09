unit DW.PDFControl;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // RTL
  System.Classes, System.Types,
  // DW
  {$IF Defined(ANDROID)} DW.PDFRenderer.Android, {$ENDIF}
  // FMX
  FMX.Controls, FMX.Layouts, FMX.StdCtrls, FMX.Objects;

type
  TPDFPage = class(TControl)
    FImage: TImage;
    FPageNumberLabel: TLabel;
  private
    FPageIndex: Integer;
    procedure SetPageIndex(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    property Image: TImage read FImage;
    property PageIndex: Integer read FPageIndex write SetPageIndex;
  end;

  TPDFControl = class(TControl)
  private
    FIsUpdating: Boolean;
    {$IF Defined(ANDROID)}
    FRenderer: TPDFRenderer;
    {$ENDIF}
    FPageIndex: Integer;
    FPageNumberLabel: TLabel;
    FVisibleIndex: Integer;
    FVertScrollBox: TVertScrollBox;
    procedure ClearPages;
    function HasMorePages: Boolean;
    procedure LoadPage;
    procedure SetVisibleIndex(const AValue: Integer);
    procedure VertScrollBoxViewportPositionChangeHandler(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF;
      const ContentSizeChanged: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadPDF(const AFileName: string);
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // FMX
  FMX.Types;

{ TPDFPage }

constructor TPDFPage.Create(AOwner: TComponent);
begin
  inherited;
  Padding.Rect := RectF(4, 0, 4, 4);
  Margins.Top := 4;
  FPageNumberLabel := TLabel.Create(Self);
  FPageNumberLabel.TextSettings.HorzAlign := TTextAlign.Center;
  FPageNumberLabel.Align := TAlignLayout.Bottom;
  FPageNumberLabel.Parent := Self;
  FImage := TImage.Create(Self);
  FImage.Align := TAlignLayout.Client;
  FImage.Parent := Self;
end;

procedure TPDFPage.SetPageIndex(const Value: Integer);
begin
  FPageIndex := Value;
  FPageNumberLabel.Text := (FPageIndex + 1).ToString;
end;

{ TPDFControl }

constructor TPDFControl.Create(AOwner: TComponent);
begin
  inherited;
  {$IF Defined(ANDROID)}
  FRenderer := TPDFRenderer.Create;
  {$ENDIF}
  FPageNumberLabel := TLabel.Create(Self);
  FPageNumberLabel.TextSettings.HorzAlign := TTextAlign.Center;
  FPageNumberLabel.Margins.Bottom := 2;
  FPageNumberLabel.Align := TAlignLayout.Top;
  FPageNumberLabel.Parent := Self;
  FVertScrollBox := TVertScrollBox.Create(Self);
  FVertScrollBox.OnViewportPositionChange := VertScrollBoxViewportPositionChangeHandler;
  FVertScrollBox.Align := TAlignLayout.Client;
  FVertScrollBox.Parent := Self;
end;

procedure TPDFControl.ClearPages;
var
  I: Integer;
begin
  FIsUpdating := True;
  try
    for I := ComponentCount - 1 downto 0 do
    begin
      if Components[I] is TPDFPage then
        Components[I].Free;
    end;
  finally
    FIsUpdating := False;
  end;
end;

function TPDFControl.HasMorePages: Boolean;
begin
  {$IF Defined(ANDROID)}
  Result := FPageIndex < FRenderer.GetPageCount - 1;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

procedure TPDFControl.LoadPage;
var
  LPage: TPDFPage;
begin
  Inc(FPageIndex);
  LPage := TPDFPage.Create(Self);
  LPage.PageIndex := FPageIndex;
  LPage.Height := FVertScrollBox.ClientHeight;
  LPage.Position.Y := FVertScrollBox.ContentBounds.Height;
  LPage.Align := TAlignLayout.Top;
  LPage.Parent := FVertScrollBox;
  {$IF Defined(ANDROID)}
  LPage.Image.Bitmap.SetSize(LPage.Image.Size.Size.Round);
  FRenderer.RenderPage(FPageIndex, LPage.Image.Bitmap);
  {$ENDIF}
end;

procedure TPDFControl.LoadPDF(const AFileName: string);
begin
  ClearPages;
  FPageIndex := -1;
  FVisibleIndex := -1;
  FPageNumberLabel.Text := '';
  {$IF Defined(ANDROID)}
  FRenderer.LoadFromFile(AFileName);
  if FRenderer.GetPageCount > 0 then
  begin
    FIsUpdating := True;
    try
      LoadPage;
      SetVisibleIndex(FPageIndex);
      if HasMorePages then
        LoadPage; // Load first 2 pages so that it's scrollable
    finally
      FIsUpdating := False;
    end;
  end;
  {$ENDIF}
end;

procedure TPDFControl.SetVisibleIndex(const AValue: Integer);
begin
  if AValue <> FVisibleIndex then
  begin
    FVisibleIndex := AValue;
    {$IF Defined(ANDROID)}
    FPageNumberLabel.Text := Format('Page %d of %d', [FVisibleIndex + 1, FRenderer.GetPageCount]);
    {$ENDIF}
  end;
end;

procedure TPDFControl.VertScrollBoxViewportPositionChangeHandler(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF;
  const ContentSizeChanged: Boolean);
var
  I, LVisibleIndex: Integer;
  LHalfWay, LY: Single;
  LPage: TPDFPage;
begin
  if not FIsUpdating then
  begin
    LVisibleIndex := 0;
    LHalfWay := NewViewportPosition.Y + (FVertScrollBox.ClientHeight / 2);
    LY := 0;
    for I := 0 to ComponentCount - 1 do
    begin
      if Components[I] is TPDFPage then
      begin
        LPage := TPDFPage(Components[I]);
        // If the position of this page is less than half of the viewport window...
        if LPage.Position.Y < LHalfWay then
        begin
          // ..and is the "highest"..
          if LPage.Position.Y > LY then
          begin
            // ..it's the "most" visible
            LY := LPage.Position.Y;
            LVisibleIndex := LPage.PageIndex;
          end;
        end;
      end;
    end;
    SetVisibleIndex(LVisibleIndex);
    // Load the next page, if there are any left to load
    if (FVisibleIndex = FPageIndex) and HasMorePages then
      LoadPage;
  end;
end;

end.
