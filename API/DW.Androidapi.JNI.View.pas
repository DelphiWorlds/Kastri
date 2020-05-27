unit DW.Androidapi.JNI.View;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{    Copyright 2020 Dave Nottage under MIT license      }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.JNI.Util;

type
  JGravity = interface;
  JTextureView = interface;
  JTextureView_SurfaceTextureListener = interface;

  JGravityClass = interface(JObjectClass)
    ['{9B523AD3-BAF8-4524-BA51-EA1F183059C8}']
    {class} function _GetAXIS_CLIP: Integer; cdecl;
    {class} function _GetAXIS_PULL_AFTER: Integer; cdecl;
    {class} function _GetAXIS_PULL_BEFORE: Integer; cdecl;
    {class} function _GetAXIS_SPECIFIED: Integer; cdecl;
    {class} function _GetAXIS_X_SHIFT: Integer; cdecl;
    {class} function _GetAXIS_Y_SHIFT: Integer; cdecl;
    {class} function _GetBOTTOM: Integer; cdecl;
    {class} function _GetCENTER: Integer; cdecl;
    {class} function _GetCENTER_HORIZONTAL: Integer; cdecl;
    {class} function _GetCENTER_VERTICAL: Integer; cdecl;
    {class} function _GetCLIP_HORIZONTAL: Integer; cdecl;
    {class} function _GetCLIP_VERTICAL: Integer; cdecl;
    {class} function _GetDISPLAY_CLIP_HORIZONTAL: Integer; cdecl;
    {class} function _GetDISPLAY_CLIP_VERTICAL: Integer; cdecl;
    {class} function _GetEND: Integer; cdecl;
    {class} function _GetFILL: Integer; cdecl;
    {class} function _GetFILL_HORIZONTAL: Integer; cdecl;
    {class} function _GetFILL_VERTICAL: Integer; cdecl;
    {class} function _GetHORIZONTAL_GRAVITY_MASK: Integer; cdecl;
    {class} function _GetLEFT: Integer; cdecl;
    {class} function _GetNO_GRAVITY: Integer; cdecl;
    {class} function _GetRELATIVE_HORIZONTAL_GRAVITY_MASK: Integer; cdecl;
    {class} function _GetRELATIVE_LAYOUT_DIRECTION: Integer; cdecl;
    {class} function _GetRIGHT: Integer; cdecl;
    {class} function _GetSTART: Integer; cdecl;
    {class} function _GetTOP: Integer; cdecl;
    {class} function _GetVERTICAL_GRAVITY_MASK: Integer; cdecl;
    {class} function init: JGravity; cdecl;//Deprecated
    {class} procedure apply(gravity: Integer; w: Integer; h: Integer; container: JRect; outRect: JRect); cdecl; overload;//Deprecated
    {class} procedure apply(gravity: Integer; w: Integer; h: Integer; container: JRect; outRect: JRect; layoutDirection: Integer); cdecl; overload;//Deprecated
    {class} procedure apply(gravity: Integer; w: Integer; h: Integer; container: JRect; xAdj: Integer; yAdj: Integer; outRect: JRect); cdecl; overload;//Deprecated
    {class} procedure apply(gravity: Integer; w: Integer; h: Integer; container: JRect; xAdj: Integer; yAdj: Integer; outRect: JRect; layoutDirection: Integer); cdecl; overload;//Deprecated
    {class} procedure applyDisplay(gravity: Integer; display: JRect; inoutObj: JRect); cdecl; overload;//Deprecated
    {class} procedure applyDisplay(gravity: Integer; display: JRect; inoutObj: JRect; layoutDirection: Integer); cdecl; overload;//Deprecated
    {class} function getAbsoluteGravity(gravity: Integer; layoutDirection: Integer): Integer; cdecl;//Deprecated
    {class} function isHorizontal(gravity: Integer): Boolean; cdecl;//Deprecated
    {class} function isVertical(gravity: Integer): Boolean; cdecl;//Deprecated
    {class} property AXIS_CLIP: Integer read _GetAXIS_CLIP;
    {class} property AXIS_PULL_AFTER: Integer read _GetAXIS_PULL_AFTER;
    {class} property AXIS_PULL_BEFORE: Integer read _GetAXIS_PULL_BEFORE;
    {class} property AXIS_SPECIFIED: Integer read _GetAXIS_SPECIFIED;
    {class} property AXIS_X_SHIFT: Integer read _GetAXIS_X_SHIFT;
    {class} property AXIS_Y_SHIFT: Integer read _GetAXIS_Y_SHIFT;
    {class} property BOTTOM: Integer read _GetBOTTOM;
    {class} property CENTER: Integer read _GetCENTER;
    {class} property CENTER_HORIZONTAL: Integer read _GetCENTER_HORIZONTAL;
    {class} property CENTER_VERTICAL: Integer read _GetCENTER_VERTICAL;
    {class} property CLIP_HORIZONTAL: Integer read _GetCLIP_HORIZONTAL;
    {class} property CLIP_VERTICAL: Integer read _GetCLIP_VERTICAL;
    {class} property DISPLAY_CLIP_HORIZONTAL: Integer read _GetDISPLAY_CLIP_HORIZONTAL;
    {class} property DISPLAY_CLIP_VERTICAL: Integer read _GetDISPLAY_CLIP_VERTICAL;
    // {class} property END: Integer read _GetEND; // use &??
    {class} property FILL: Integer read _GetFILL;
    {class} property FILL_HORIZONTAL: Integer read _GetFILL_HORIZONTAL;
    {class} property FILL_VERTICAL: Integer read _GetFILL_VERTICAL;
    {class} property HORIZONTAL_GRAVITY_MASK: Integer read _GetHORIZONTAL_GRAVITY_MASK;
    {class} property LEFT: Integer read _GetLEFT;
    {class} property NO_GRAVITY: Integer read _GetNO_GRAVITY;
    {class} property RELATIVE_HORIZONTAL_GRAVITY_MASK: Integer read _GetRELATIVE_HORIZONTAL_GRAVITY_MASK;
    {class} property RELATIVE_LAYOUT_DIRECTION: Integer read _GetRELATIVE_LAYOUT_DIRECTION;
    {class} property RIGHT: Integer read _GetRIGHT;
    {class} property START: Integer read _GetSTART;
    {class} property TOP: Integer read _GetTOP;
    {class} property VERTICAL_GRAVITY_MASK: Integer read _GetVERTICAL_GRAVITY_MASK;
  end;

  [JavaSignature('android/view/Gravity')]
  JGravity = interface(JObject)
    ['{641A23F0-9ABD-46DE-97D5-B7077E7EEF4C}']
  end;
  TJGravity = class(TJavaGenericImport<JGravityClass, JGravity>) end;

  JTextureViewClass = interface(JViewClass)
    ['{B61657F8-975F-44DD-98BD-627EDAEAA15D}']
    {class} function init(context: JContext): JTextureView; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet): JTextureView; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet; defStyleAttr: Integer): JTextureView; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet; defStyleAttr: Integer; defStyleRes: Integer): JTextureView; cdecl; overload;
  end;

  [JavaSignature('android/view/TextureView')]
  JTextureView = interface(JView)
    ['{F0FC4FB8-64C8-4BA6-B768-A4E67C9397CC}']
    procedure buildLayer; cdecl;
    procedure draw(canvas: JCanvas); cdecl;
    function getBitmap: JBitmap; cdecl; overload;
    function getBitmap(width: Integer; height: Integer): JBitmap; cdecl; overload;
    function getLayerType: Integer; cdecl;
    function getSurfaceTexture: JSurfaceTexture; cdecl;
    function getSurfaceTextureListener: JTextureView_SurfaceTextureListener; cdecl;
    function getTransform(transform: JMatrix): JMatrix; cdecl;
    function isAvailable: Boolean; cdecl;
    function isOpaque: Boolean; cdecl;
    function lockCanvas: JCanvas; cdecl; overload;
    function lockCanvas(dirty: JRect): JCanvas; cdecl; overload;
    procedure setLayerPaint(paint: JPaint); cdecl;
    procedure setLayerType(layerType: Integer; paint: JPaint); cdecl;
    procedure setOpaque(opaque: Boolean); cdecl;
    procedure setSurfaceTexture(surfaceTexture: JSurfaceTexture); cdecl;
    procedure setSurfaceTextureListener(listener: JTextureView_SurfaceTextureListener); cdecl;
    procedure setTransform(transform: JMatrix); cdecl;
    procedure unlockCanvasAndPost(canvas: JCanvas); cdecl;
  end;
  TJTextureView = class(TJavaGenericImport<JTextureViewClass, JTextureView>) end;

  JTextureView_SurfaceTextureListenerClass = interface(IJavaClass)
    ['{079709DF-C144-4083-888E-2318271F676F}']
  end;

  [JavaSignature('android/view/TextureView$SurfaceTextureListener')]
  JTextureView_SurfaceTextureListener = interface(IJavaInstance)
    ['{1E496A42-F10C-4473-BDE1-43960E671F09}']
    procedure onSurfaceTextureAvailable(surface: JSurfaceTexture; width: Integer; height: Integer); cdecl;
    function onSurfaceTextureDestroyed(surface: JSurfaceTexture): Boolean; cdecl;
    procedure onSurfaceTextureUpdated(surface: JSurfaceTexture); cdecl;
    procedure onSurfaceTextureSizeChanged(surface: JSurfaceTexture; width: Integer; height: Integer); cdecl;
  end;
  TJTextureView_SurfaceTextureListener = class(TJavaGenericImport<JTextureView_SurfaceTextureListenerClass, JTextureView_SurfaceTextureListener>) end;

implementation

end.

