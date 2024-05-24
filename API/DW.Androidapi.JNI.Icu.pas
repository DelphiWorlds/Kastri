unit DW.Androidapi.JNI.Icu;

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
  Androidapi.JNI.JavaTypes, Androidapi.JNIBridge;

type
  JFreezable = interface;

  JFreezableClass = interface(JObjectClass)
    ['{FAF2002E-096A-4654-974E-157BAE414567}']
  end;

  [JavaSignature('android/icu/util/Freezable')]
  JFreezable = interface(JObject)
    ['{23D673FB-D64E-4E70-8777-4FEDD79C8E0C}']
    function cloneAsThawed: JObject; cdecl;
    function freeze: JObject; cdecl;
    function isFrozen: boolean; cdecl;
  end;
  TJFreezable = class(TJavaGenericImport<JFreezableClass, JFreezable>)
  end;

implementation

end.
