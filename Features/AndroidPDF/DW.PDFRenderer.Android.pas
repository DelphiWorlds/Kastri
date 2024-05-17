unit DW.PDFRenderer.Android;

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
  // Android
  Androidapi.JNI.GraphicsContentViewText,
  // FMX
  FMX.Graphics;

type
  TPDFRenderer = class(TObject)
  private
    FRenderer: JPdfRenderer;
    procedure DestroyRenderer;
  public
    destructor Destroy; override;
    function GetPageCount: Integer;
    procedure LoadFromFile(const AFileName: string);
    procedure RenderPage(const APageIndex: Integer; const ABitmap: TBitmap);
  end;

implementation

uses
  // Android
  Androidapi.JNI.Os, Androidapi.Helpers, Androidapi.JNI.JavaTypes,
  // FMX
  FMX.Surfaces, FMX.Helpers.Android;

{ TPDFRenderer }

destructor TPDFRenderer.Destroy;
begin
  DestroyRenderer;
  inherited;
end;

procedure TPDFRenderer.DestroyRenderer;
begin
  if FRenderer <> nil then
    FRenderer.close;
  FRenderer := nil;
end;

function TPDFRenderer.GetPageCount: Integer;
begin
  if FRenderer <> nil then
    Result := FRenderer.getPageCount
  else
    Result := 0;
end;

procedure TPDFRenderer.LoadFromFile(const AFileName: string);
var
  LDescriptor: JParcelFileDescriptor;
  LFile: JFile;
begin
  LFile := TJFile.JavaClass.init(StringToJString(AFileName));
  LDescriptor := TJParcelFileDescriptor.JavaClass.open(LFile, TJParcelFileDescriptor.JavaClass.MODE_READ_ONLY);
  DestroyRenderer;
  FRenderer := TJPdfRenderer.JavaClass.init(LDescriptor);
end;

procedure TPDFRenderer.RenderPage(const APageIndex: Integer; const ABitmap: TBitmap);
var
  LNativeBitmap: JBitmap;
  LPage: JPdfRenderer_Page;
  LSurface: TBitmapSurface;
begin
  LNativeBitmap := TJBitmap.JavaClass.createBitmap(ABitmap.Width, ABitmap.Height, TJBitmap_Config.JavaClass.ARGB_4444);
  try
    LPage := FRenderer.openPage(APageIndex);
    LPage.render(LNativeBitmap, nil, nil, TJPdfRenderer_Page.JavaClass.RENDER_MODE_FOR_DISPLAY);
    LPage.close;
    LSurface := TBitmapSurface.Create;
    try
      if JBitmapToSurface(LNativeBitmap, LSurface) then
        ABitmap.Assign(LSurface);
    finally
      LSurface.Free;
    end;
  finally
    LNativeBitmap.recycle;
  end;
end;

end.
