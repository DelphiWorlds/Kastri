unit DW.iOSapi.Symbols;

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
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation;

type
  NSSymbolEffectOptions = interface;
  NSSymbolEffect = interface;
  NSSymbolPulseEffect = interface;
  NSSymbolBounceEffect = interface;
  NSSymbolVariableColorEffect = interface;
  NSSymbolScaleEffect = interface;
  NSSymbolAppearEffect = interface;
  NSSymbolDisappearEffect = interface;
  NSSymbolContentTransition = interface;
  NSSymbolReplaceContentTransition = interface;
  NSSymbolAutomaticContentTransition = interface;

  NSSymbolEffectOptionsClass = interface(NSObjectClass)
    ['{BD695740-FE75-48F1-A7BF-23C615DDACC6}']
    {class} function new: Pointer; cdecl;
    {class} function options: Pointer; cdecl;
    {class} function optionsWithNonRepeating: Pointer; cdecl;
    {class} function optionsWithRepeatCount(count: NSInteger): Pointer; cdecl;
    {class} function optionsWithRepeating: Pointer; cdecl;
    {class} function optionsWithSpeed(speed: Double): Pointer; cdecl;
  end;

  NSSymbolEffectOptions = interface(NSObject)
    ['{DA12E898-1A01-466A-9E89-7B4E4BA3F8B9}']
    function optionsWithNonRepeating: Pointer; cdecl;
    function optionsWithRepeatCount(count: NSInteger): Pointer; cdecl;
    function optionsWithRepeating: Pointer; cdecl;
    function optionsWithSpeed(speed: Double): Pointer; cdecl;
  end;
  TNSSymbolEffectOptions = class(TOCGenericImport<NSSymbolEffectOptionsClass, NSSymbolEffectOptions>) end;

  NSSymbolEffectClass = interface(NSObjectClass)
    ['{F834852B-9DF4-4372-9259-15F2A81FB1CA}']
    {class} function new: Pointer; cdecl;
  end;

  NSSymbolEffect = interface(NSObject)
    ['{52C6F935-98A3-481D-AE5D-182AC4E15066}']
  end;
  TNSSymbolEffect = class(TOCGenericImport<NSSymbolEffectClass, NSSymbolEffect>) end;

  NSSymbolPulseEffectClass = interface(NSSymbolEffectClass)
    ['{7D99A333-F199-46D5-8F0A-8ADE852639E3}']
    {class} function effect: Pointer; cdecl;
  end;

  NSSymbolPulseEffect = interface(NSSymbolEffect)
    ['{3C8C9DCF-707A-4E91-866F-C1C453BC2A12}']
    function effectWithByLayer: Pointer; cdecl;
    function effectWithWholeSymbol: Pointer; cdecl;
  end;
  TNSSymbolPulseEffect = class(TOCGenericImport<NSSymbolPulseEffectClass, NSSymbolPulseEffect>) end;

  NSSymbolBounceEffectClass = interface(NSSymbolEffectClass)
    ['{A20D23EB-198A-4303-A9FE-DB87A85AAFCB}']
    {class} function bounceDownEffect: Pointer; cdecl;
    {class} function bounceUpEffect: Pointer; cdecl;
    {class} function effect: Pointer; cdecl;
  end;

  NSSymbolBounceEffect = interface(NSSymbolEffect)
    ['{78F42292-D4DF-4BDE-AC58-0F61B85DAD61}']
    function effectWithByLayer: Pointer; cdecl;
    function effectWithWholeSymbol: Pointer; cdecl;
  end;
  TNSSymbolBounceEffect = class(TOCGenericImport<NSSymbolBounceEffectClass, NSSymbolBounceEffect>) end;

  NSSymbolVariableColorEffectClass = interface(NSSymbolEffectClass)
    ['{F3ACB710-0EF0-4E85-A1BA-44CD1C8EF1FF}']
    {class} function effect: Pointer; cdecl;
  end;

  NSSymbolVariableColorEffect = interface(NSSymbolEffect)
    ['{D1B14C5C-EEDD-4A7F-AEC4-5AA8DAF7C12B}']
    function effectWithCumulative: Pointer; cdecl;
    function effectWithDimInactiveLayers: Pointer; cdecl;
    function effectWithHideInactiveLayers: Pointer; cdecl;
    function effectWithIterative: Pointer; cdecl;
    function effectWithNonReversing: Pointer; cdecl;
    function effectWithReversing: Pointer; cdecl;
  end;
  TNSSymbolVariableColorEffect = class(TOCGenericImport<NSSymbolVariableColorEffectClass, NSSymbolVariableColorEffect>) end;

  NSSymbolScaleEffectClass = interface(NSSymbolEffectClass)
    ['{372BBA31-3E78-4CA5-9B50-9B216AC3B61D}']
    {class} function effect: Pointer; cdecl;
    {class} function scaleDownEffect: Pointer; cdecl;
    {class} function scaleUpEffect: Pointer; cdecl;
  end;

  NSSymbolScaleEffect = interface(NSSymbolEffect)
    ['{21FD0BA0-EC60-4F1C-8462-E1A8554CA042}']
    function effectWithByLayer: Pointer; cdecl;
    function effectWithWholeSymbol: Pointer; cdecl;
  end;
  TNSSymbolScaleEffect = class(TOCGenericImport<NSSymbolScaleEffectClass, NSSymbolScaleEffect>) end;

  NSSymbolAppearEffectClass = interface(NSSymbolEffectClass)
    ['{24343708-EC13-4A2B-A059-3AB752734D58}']
    {class} function appearDownEffect: Pointer; cdecl;
    {class} function appearUpEffect: Pointer; cdecl;
    {class} function effect: Pointer; cdecl;
  end;

  NSSymbolAppearEffect = interface(NSSymbolEffect)
    ['{E14128EB-F7B4-4CFC-B9D0-14E8CAD45E30}']
    function effectWithByLayer: Pointer; cdecl;
    function effectWithWholeSymbol: Pointer; cdecl;
  end;
  TNSSymbolAppearEffect = class(TOCGenericImport<NSSymbolAppearEffectClass, NSSymbolAppearEffect>) end;

  NSSymbolDisappearEffectClass = interface(NSSymbolEffectClass)
    ['{06F00A9C-DC1C-41C7-A429-5434EF3ADA6F}']
    {class} function disappearDownEffect: Pointer; cdecl;
    {class} function disappearUpEffect: Pointer; cdecl;
    {class} function effect: Pointer; cdecl;
  end;

  NSSymbolDisappearEffect = interface(NSSymbolEffect)
    ['{8F8D2E32-F2D6-4094-83E2-B0C96C7E0095}']
    function effectWithByLayer: Pointer; cdecl;
    function effectWithWholeSymbol: Pointer; cdecl;
  end;
  TNSSymbolDisappearEffect = class(TOCGenericImport<NSSymbolDisappearEffectClass, NSSymbolDisappearEffect>) end;

  NSSymbolContentTransitionClass = interface(NSObjectClass)
    ['{8B642DB5-D1C0-46D7-8331-0D4828EAF329}']
    {class} function new: Pointer; cdecl;
  end;

  NSSymbolContentTransition = interface(NSObject)
    ['{99E9B9DA-93DA-49D3-A414-32DFCF43DC66}']
  end;
  TNSSymbolContentTransition = class(TOCGenericImport<NSSymbolContentTransitionClass, NSSymbolContentTransition>) end;

  NSSymbolReplaceContentTransitionClass = interface(NSSymbolContentTransitionClass)
    ['{876767F3-7D48-40DE-91BB-BC0B1F77D2B6}']
    {class} function replaceDownUpTransition: Pointer; cdecl;
    {class} function replaceOffUpTransition: Pointer; cdecl;
    {class} function replaceUpUpTransition: Pointer; cdecl;
    {class} function transition: Pointer; cdecl;
  end;

  NSSymbolReplaceContentTransition = interface(NSSymbolContentTransition)
    ['{E467410B-0518-4CC8-A81B-333CF631DF0E}']
    function transitionWithByLayer: Pointer; cdecl;
    function transitionWithWholeSymbol: Pointer; cdecl;
  end;
  TNSSymbolReplaceContentTransition = class(TOCGenericImport<NSSymbolReplaceContentTransitionClass, NSSymbolReplaceContentTransition>) end;

  NSSymbolAutomaticContentTransitionClass = interface(NSSymbolContentTransitionClass)
    ['{9CE99266-793E-4989-86C1-2E782314E236}']
    {class} function transition: Pointer; cdecl;
  end;

  NSSymbolAutomaticContentTransition = interface(NSSymbolContentTransition)
    ['{65ED6556-01E9-4F2F-92AE-4DB882354354}']
  end;
  TNSSymbolAutomaticContentTransition = class(TOCGenericImport<NSSymbolAutomaticContentTransitionClass, NSSymbolAutomaticContentTransition>) end;

const
  libSymbols = '/System/Library/Frameworks/Symbols.framework/Symbols';

implementation

uses
  Posix.Dlfcn;

var
  SymbolsModule: THandle;

initialization
  SymbolsModule := dlopen(MarshaledAString(libSymbols), RTLD_LAZY);

finalization
  dlclose(SymbolsModule);

end.