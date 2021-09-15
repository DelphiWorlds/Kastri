unit DW.Androidapi.JNI.AndroidX.Content;

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
  // Android
  Androidapi.JNIBridge, Androidapi.Jni.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Os;

type
  JLocusIdCompat = interface;
  JShortcutInfoCompat = interface;

  JLocusIdCompatClass = interface(JObjectClass)
    ['{1DF4C941-18DE-4AE2-ACB5-A9347F426677}']
    {class} function init(string_: JString): JLocusIdCompat; cdecl;
  end;

  [JavaSignature('androidx/core/content/LocusIdCompat')]
  JLocusIdCompat = interface(JObject)
    ['{4728A252-E681-4F0B-96AF-960EDC50654F}']
    function equals(object_: JObject): Boolean; cdecl;
    function getId: JString; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
  end;
  TJLocusIdCompat = class(TJavaGenericImport<JLocusIdCompatClass, JLocusIdCompat>) end;

  JShortcutInfoCompatClass = interface(JObjectClass)
    ['{38D9871E-9287-4205-9473-D2A84A26C75D}']
  end;

  [JavaSignature('androidx/core/content/pm/ShortcutInfoCompat')]
  JShortcutInfoCompat = interface(JObject)
    ['{0F462C6A-3EEA-4561-AB06-25BB6EB30106}']
    function getActivity: JComponentName; cdecl;
    function getCategories: JSet; cdecl;
    function getDisabledMessage: JCharSequence; cdecl;
    function getDisabledReason: Integer; cdecl;
    function getExtras: JPersistableBundle; cdecl;
    // function getIcon: Jdrawable_IconCompat; cdecl;
    function getId: JString; cdecl;
    function getIntent: JIntent; cdecl;
    function getIntents: TJavaObjectArray<JIntent>; cdecl;
    function getLastChangedTimestamp: Int64; cdecl;
    function getLocusId: JLocusIdCompat; cdecl; overload;
    function getLongLabel: JCharSequence; cdecl;
    function getPackage: JString; cdecl;
    function getRank: Integer; cdecl;
    function getShortLabel: JCharSequence; cdecl;
    function getUserHandle: JUserHandle; cdecl;
    function hasKeyFieldsOnly: Boolean; cdecl;
    function isCached: Boolean; cdecl;
    function isDeclaredInManifest: Boolean; cdecl;
    function isDynamic: Boolean; cdecl;
    function isEnabled: Boolean; cdecl;
    function isImmutable: Boolean; cdecl;
    function isPinned: Boolean; cdecl;
    function toShortcutInfo: JShortcutInfo; cdecl;
  end;
  TShortcutInfoCompat = class(TJavaGenericImport<JShortcutInfoCompatClass, JShortcutInfoCompat>) end;

implementation

end.
