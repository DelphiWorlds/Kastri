unit DW.Macapi.Symbols;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.CocoaTypes, Macapi.Foundation;

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
    ['{F569C3C1-B735-4709-A1ED-E9C9D10D6559}']
    {class} function new: Pointer; cdecl;
    {class} function options: Pointer; cdecl;
    {class} function optionsWithNonRepeating: Pointer; cdecl;
    {class} function optionsWithRepeatCount(count: NSInteger): Pointer; cdecl;
    {class} function optionsWithRepeating: Pointer; cdecl;
    {class} function optionsWithSpeed(speed: Double): Pointer; cdecl;
  end;

  NSSymbolEffectOptions = interface(NSObject)
    ['{D82DC8C4-2F4E-46E8-99F0-8BAD521EB4A1}']
    function optionsWithNonRepeating: Pointer; cdecl;
    function optionsWithRepeatCount(count: NSInteger): Pointer; cdecl;
    function optionsWithRepeating: Pointer; cdecl;
    function optionsWithSpeed(speed: Double): Pointer; cdecl;
  end;
  TNSSymbolEffectOptions = class(TOCGenericImport<NSSymbolEffectOptionsClass, NSSymbolEffectOptions>) end;

  NSSymbolEffectClass = interface(NSObjectClass)
    ['{C057B951-DDB4-4CE0-B70C-CE68D708C176}']
    {class} function new: Pointer; cdecl;
  end;

  NSSymbolEffect = interface(NSObject)
    ['{06A3CDBB-61E2-4A58-97C2-A53B01F6929E}']
  end;
  TNSSymbolEffect = class(TOCGenericImport<NSSymbolEffectClass, NSSymbolEffect>) end;

  NSSymbolPulseEffectClass = interface(NSSymbolEffectClass)
    ['{506FFAA4-B034-4ED6-B815-17BDCD95963F}']
    {class} function effect: Pointer; cdecl;
  end;

  NSSymbolPulseEffect = interface(NSSymbolEffect)
    ['{E433137F-ABAE-4C60-8F5C-6833A546CE68}']
    function effectWithByLayer: Pointer; cdecl;
    function effectWithWholeSymbol: Pointer; cdecl;
  end;
  TNSSymbolPulseEffect = class(TOCGenericImport<NSSymbolPulseEffectClass, NSSymbolPulseEffect>) end;

  NSSymbolBounceEffectClass = interface(NSSymbolEffectClass)
    ['{742AFF1B-7CAA-40F5-9461-7DEEE0DD569D}']
    {class} function bounceDownEffect: Pointer; cdecl;
    {class} function bounceUpEffect: Pointer; cdecl;
    {class} function effect: Pointer; cdecl;
  end;

  NSSymbolBounceEffect = interface(NSSymbolEffect)
    ['{0EDE40CF-4953-401F-9F9B-57FC54DE465C}']
    function effectWithByLayer: Pointer; cdecl;
    function effectWithWholeSymbol: Pointer; cdecl;
  end;
  TNSSymbolBounceEffect = class(TOCGenericImport<NSSymbolBounceEffectClass, NSSymbolBounceEffect>) end;

  NSSymbolVariableColorEffectClass = interface(NSSymbolEffectClass)
    ['{05147190-945F-4D42-933D-DA0BAF2D44EC}']
    {class} function effect: Pointer; cdecl;
  end;

  NSSymbolVariableColorEffect = interface(NSSymbolEffect)
    ['{77BCB4F1-6C93-47D9-8A43-71D831CCDB32}']
    function effectWithCumulative: Pointer; cdecl;
    function effectWithDimInactiveLayers: Pointer; cdecl;
    function effectWithHideInactiveLayers: Pointer; cdecl;
    function effectWithIterative: Pointer; cdecl;
    function effectWithNonReversing: Pointer; cdecl;
    function effectWithReversing: Pointer; cdecl;
  end;
  TNSSymbolVariableColorEffect = class(TOCGenericImport<NSSymbolVariableColorEffectClass, NSSymbolVariableColorEffect>) end;

  NSSymbolScaleEffectClass = interface(NSSymbolEffectClass)
    ['{DF968FF0-E4F6-4354-A63C-B2104BC467F2}']
    {class} function effect: Pointer; cdecl;
    {class} function scaleDownEffect: Pointer; cdecl;
    {class} function scaleUpEffect: Pointer; cdecl;
  end;

  NSSymbolScaleEffect = interface(NSSymbolEffect)
    ['{6447A3C1-1D48-4B09-9C78-F92F9044DED6}']
    function effectWithByLayer: Pointer; cdecl;
    function effectWithWholeSymbol: Pointer; cdecl;
  end;
  TNSSymbolScaleEffect = class(TOCGenericImport<NSSymbolScaleEffectClass, NSSymbolScaleEffect>) end;

  NSSymbolAppearEffectClass = interface(NSSymbolEffectClass)
    ['{63936D32-FC30-438E-8C2F-AFA7FA1CB8CF}']
    {class} function appearDownEffect: Pointer; cdecl;
    {class} function appearUpEffect: Pointer; cdecl;
    {class} function effect: Pointer; cdecl;
  end;

  NSSymbolAppearEffect = interface(NSSymbolEffect)
    ['{46659205-9786-4300-9EE1-E148D912AFBE}']
    function effectWithByLayer: Pointer; cdecl;
    function effectWithWholeSymbol: Pointer; cdecl;
  end;
  TNSSymbolAppearEffect = class(TOCGenericImport<NSSymbolAppearEffectClass, NSSymbolAppearEffect>) end;

  NSSymbolDisappearEffectClass = interface(NSSymbolEffectClass)
    ['{A1BF8E5E-A470-4BFB-B611-2309421DA720}']
    {class} function disappearDownEffect: Pointer; cdecl;
    {class} function disappearUpEffect: Pointer; cdecl;
    {class} function effect: Pointer; cdecl;
  end;

  NSSymbolDisappearEffect = interface(NSSymbolEffect)
    ['{88846A68-9D3E-49E4-9FD2-9AE493425985}']
    function effectWithByLayer: Pointer; cdecl;
    function effectWithWholeSymbol: Pointer; cdecl;
  end;
  TNSSymbolDisappearEffect = class(TOCGenericImport<NSSymbolDisappearEffectClass, NSSymbolDisappearEffect>) end;

  NSSymbolContentTransitionClass = interface(NSObjectClass)
    ['{F433C503-05E6-4D53-9A50-651A65CD884F}']
    {class} function new: Pointer; cdecl;
  end;

  NSSymbolContentTransition = interface(NSObject)
    ['{EFE35050-9403-4479-B2C9-1890D488192C}']
  end;
  TNSSymbolContentTransition = class(TOCGenericImport<NSSymbolContentTransitionClass, NSSymbolContentTransition>) end;

  NSSymbolReplaceContentTransitionClass = interface(NSSymbolContentTransitionClass)
    ['{DE9DFF55-BD97-4ACC-81D8-8D7C7AA7E167}']
    {class} function replaceDownUpTransition: Pointer; cdecl;
    {class} function replaceOffUpTransition: Pointer; cdecl;
    {class} function replaceUpUpTransition: Pointer; cdecl;
    {class} function transition: Pointer; cdecl;
  end;

  NSSymbolReplaceContentTransition = interface(NSSymbolContentTransition)
    ['{1113291E-58DE-47E5-B1F0-36390E4CA1AE}']
    function transitionWithByLayer: Pointer; cdecl;
    function transitionWithWholeSymbol: Pointer; cdecl;
  end;
  TNSSymbolReplaceContentTransition = class(TOCGenericImport<NSSymbolReplaceContentTransitionClass, NSSymbolReplaceContentTransition>) end;

  NSSymbolAutomaticContentTransitionClass = interface(NSSymbolContentTransitionClass)
    ['{40213559-5030-4A8A-B29A-B21015A8C6D4}']
    {class} function transition: Pointer; cdecl;
  end;

  NSSymbolAutomaticContentTransition = interface(NSSymbolContentTransition)
    ['{BF2264D7-F8A8-4788-A2B4-BBEBD6A99F4F}']
  end;
  TNSSymbolAutomaticContentTransition = class(TOCGenericImport<NSSymbolAutomaticContentTransitionClass, NSSymbolAutomaticContentTransition>) end;

const
  libSymbols = '/System/Library/Frameworks/Symbols.framework/Symbols';

implementation

uses
  System.SysUtils;

var
  SymbolsModule: THandle;

initialization
  SymbolsModule := LoadLibrary(libSymbols);

finalization
  if SymbolsModule <> 0 then
    FreeLibrary(SymbolsModule);

end.