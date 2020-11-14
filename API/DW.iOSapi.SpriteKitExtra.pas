unit DW.iOSapi.SpriteKitExtra;

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

// This unit is required so as to avoid circular references to DW.iOSapi.SceneKit

interface

uses
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit, iOSapi.CoreGraphics,
  // DW
  DW.Macapi.Simd, DW.iOSapi.SpriteKit, DW.iOSapi.SceneKit;

type
  SK3DNodeClass = interface(SKNodeClass)
    ['{48EE0436-32C0-4864-9F65-33554ACC33C2}']
  end;

  SK3DNode = interface(SKNode)
    ['{F0018A06-7CA2-4080-B8BE-B6DA4DC323CB}']
    function autoenablesDefaultLighting: Boolean; cdecl;
    function hitTest(point: CGPoint; options: NSDictionary): NSArray; cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function initWithViewportSize(viewportSize: CGSize): Pointer; cdecl;
    function isPlaying: Boolean; cdecl;
    function loops: Boolean; cdecl;
    function pointOfView: SCNNode; cdecl;
    function projectPoint(point: vector_float3): vector_float3; cdecl;
    function sceneTime: NSTimeInterval; cdecl;
    function scnScene: SCNScene; cdecl;
    procedure setAutoenablesDefaultLighting(autoenablesDefaultLighting: Boolean); cdecl;
    procedure setLoops(loops: Boolean); cdecl;
    procedure setPlaying(playing: Boolean); cdecl;
    procedure setPointOfView(pointOfView: SCNNode); cdecl;
    procedure setSceneTime(sceneTime: NSTimeInterval); cdecl;
    procedure setScnScene(scnScene: SCNScene); cdecl;
    procedure setViewportSize(viewportSize: CGSize); cdecl;
    function unprojectPoint(point: vector_float3): vector_float3; cdecl;
    function viewportSize: CGSize; cdecl;
  end;
  TSK3DNode = class(TOCGenericImport<SK3DNodeClass, SK3DNode>) end;

implementation

end.
