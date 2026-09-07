unit DW.Graphics.Helpers.Android;

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

uses
  // Android
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Net, Androidapi.JNI.JavaTypes,
  // FMX
  FMX.Graphics;

type
  TBitmapHelper = class helper for TBitmap
  public
    /// <summary>
    ///   Imports from a JBitmap into this bitmap
    /// </summary>
    function FromJBitmap(const AJBitmap: JBitmap): Boolean;
    /// <summary>
    ///   Converts into a JBitmap
    /// </summary>
    function ToJBitmap: JBitmap;
    /// <summary>
    ///   Rescales to the destination, natively
    /// </summary>
    function RescaleBitmap(const ABitmap: TBitmap): Boolean;
    /// <summary>
    ///   Loads bitmap content from an Android URI (e.g. content://, file://)
    /// </summary>
    function LoadFromURI(const AURI: Jnet_Uri): Boolean;
    /// <summary>
    ///   Loads bitmap content from an Android InputStream
    /// </summary>
    function LoadFromInputStream(const AInputStream: JInputStream): Boolean;
  end;

implementation

uses
  // Android
  Androidapi.Helpers,
  // FMX
  FMX.Surfaces, FMX.Helpers.Android;

{ TBitmapHelper }

function TBitmapHelper.FromJBitmap(const AJBitmap: JBitmap): Boolean;
var
  LSurface: TBitmapSurface;
begin
  LSurface := TBitmapSurface.Create;
  try
    Result := JBitmapToSurface(AJBitmap, LSurface);
    if Result  then
      Assign(LSurface);
  finally
    LSurface.Free;
  end;
end;

function TBitmapHelper.ToJBitmap: JBitmap;
var
  LSurface: TBitmapSurface;
begin
  Result := TJBitmap.JavaClass.createBitmap(Width, Height, TJBitmap_Config.JavaClass.ARGB_8888);
  LSurface := TBitmapSurface.Create;
  try
    LSurface.Assign(Self);
    SurfaceToJBitmap(LSurface, Result);
  finally
    LSurface.Free;
  end;
end;

// https://stackoverflow.com/a/53193158/3164070
function TBitmapHelper.RescaleBitmap(const ABitmap: TBitmap): Boolean;
var
  LSrcJBitmap, LDstJBitmap: JBitmap;
  LSurface: TBitmapSurface;
begin
  Result := False;
  LSurface := TBitmapSurface.Create;
  try
    LSurface.Assign(Self);
    LSrcJBitmap := TJBitmap.JavaClass.createBitmap(LSurface.Width, LSurface.Height, TJBitmap_Config.JavaClass.ARGB_8888);
    if SurfaceToJBitmap(LSurface, LSrcJBitmap) then
    begin
      LDstJBitmap := TJBitmap.JavaClass.createScaledBitmap(LSrcJBitmap, ABitmap.Width, ABitmap.Height, True);
      if JBitmapToSurface(LDstJBitmap, LSurface) then
      begin
        ABitmap.Assign(LSurface);
        Result := True;
      end;
    end;
  finally
    LSurface.Free;
  end;
end;

function TBitmapHelper.LoadFromInputStream(const AInputStream: JInputStream): Boolean;
var
  LJBitmap: JBitmap;
begin
  LJBitmap := TJBitmapFactory.JavaClass.decodeStream(AInputStream);
  if LJBitmap <> nil then
    Result := FromJBitmap(LJBitmap);
end;

function TBitmapHelper.LoadFromURI(const AURI: Jnet_Uri): Boolean;
var
  LInputStream: JInputStream;
  LJBitmap: JBitmap;
begin
  Result := False;
  LInputStream := TAndroidHelper.Context.getContentResolver.openInputStream(AURI);
  if LInputStream <> nil then
  try
    Result := LoadFromInputStream(LInputStream);
  finally
    LInputStream.close;
  end;
end;

end.
