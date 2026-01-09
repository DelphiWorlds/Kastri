unit DW.Androidapi.JNI.AndroidX.FileProvider;

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
  Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.JNI.Net, Androidapi.JNI.Os;

type
  JFileProvider = interface;

  JFileProviderClass = interface(JContentProviderClass)
    ['{81FBD01F-F1CC-4960-AA63-9F424DC325B1}']
    {class} function getUriForFile(context: JContext; authority: JString; _file: JFile): Jnet_Uri; cdecl;
    {class} function init: JFileProvider; cdecl;
  end;

  [JavaSignature('androidx/core/content/FileProvider')]
  JFileProvider = interface(JContentProvider)
    ['{56A9B9A6-5625-4D46-96E1-E5935B72763A}']
    procedure attachInfo(context: JContext; info: JProviderInfo); cdecl;
    function delete(uri: Jnet_Uri; selection: JString; selectionArgs: TJavaObjectArray<JString>): Integer; cdecl;
    function getType(uri: Jnet_Uri): JString; cdecl;
    function insert(uri: Jnet_Uri; values: JContentValues): Jnet_Uri; cdecl;
    function onCreate: Boolean; cdecl;
    function openFile(uri: Jnet_Uri; mode: JString): JParcelFileDescriptor; cdecl;
    function query(uri: Jnet_Uri; projection: TJavaObjectArray<JString>; selection: JString; selectionArgs: TJavaObjectArray<JString>;
      sortOrder: JString): JCursor; cdecl;
    function update(uri: Jnet_Uri; values: JContentValues; selection: JString; selectionArgs: TJavaObjectArray<JString>): Integer; cdecl;
  end;
  TJFileProvider = class(TJavaGenericImport<JFileProviderClass, JFileProvider>) end;

implementation

end.

