unit DW.Androidapi.JNI.Text;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2023 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText;

type
  JHtml = interface;
  JHtml_ImageGetter = interface;
  JHtml_TagHandler = interface;

  JHtmlClass = interface(JObjectClass)
    ['{E7013A47-AB8B-44A5-AE40-F44391E22EF8}']
    function _GetFROM_HTML_MODE_COMPACT: Integer; cdecl;
    function _GetFROM_HTML_MODE_LEGACY: Integer; cdecl;
    function _GetFROM_HTML_OPTION_USE_CSS_COLORS: Integer; cdecl;
    function _GetFROM_HTML_SEPARATOR_LINE_BREAK_BLOCKQUOTE: Integer; cdecl;
    function _GetFROM_HTML_SEPARATOR_LINE_BREAK_DIV: Integer; cdecl;
    function _GetFROM_HTML_SEPARATOR_LINE_BREAK_HEADING: Integer; cdecl;
    function _GetFROM_HTML_SEPARATOR_LINE_BREAK_LIST: Integer; cdecl;
    function _GetFROM_HTML_SEPARATOR_LINE_BREAK_LIST_ITEM: Integer; cdecl;
    function _GetFROM_HTML_SEPARATOR_LINE_BREAK_PARAGRAPH: Integer; cdecl;
    function _GetTO_HTML_PARAGRAPH_LINES_CONSECUTIVE: Integer; cdecl;
    function _GetTO_HTML_PARAGRAPH_LINES_INDIVIDUAL: Integer; cdecl;
    function escapeHtml(text: JCharSequence): JString; cdecl;
    function fromHtml(source: JString): JSpanned; cdecl; overload;
    function fromHtml(source: JString; flags: Integer): JSpanned; cdecl; overload;
    function fromHtml(source: JString; flags: Integer; imageGetter: JHtml_ImageGetter; tagHandler: JHtml_TagHandler): JSpanned; cdecl; overload;
    function fromHtml(source: JString; imageGetter: JHtml_ImageGetter; tagHandler: JHtml_TagHandler): JSpanned; cdecl; overload;
    function toHtml(text: JSpanned): JString; cdecl; overload;
    function toHtml(text: JSpanned; option: Integer): JString; cdecl; overload;
    property FROM_HTML_MODE_COMPACT: Integer read _GetFROM_HTML_MODE_COMPACT;
    property FROM_HTML_MODE_LEGACY: Integer read _GetFROM_HTML_MODE_LEGACY;
    property FROM_HTML_OPTION_USE_CSS_COLORS: Integer read _GetFROM_HTML_OPTION_USE_CSS_COLORS;
    property FROM_HTML_SEPARATOR_LINE_BREAK_BLOCKQUOTE: Integer read _GetFROM_HTML_SEPARATOR_LINE_BREAK_BLOCKQUOTE;
    property FROM_HTML_SEPARATOR_LINE_BREAK_DIV: Integer read _GetFROM_HTML_SEPARATOR_LINE_BREAK_DIV;
    property FROM_HTML_SEPARATOR_LINE_BREAK_HEADING: Integer read _GetFROM_HTML_SEPARATOR_LINE_BREAK_HEADING;
    property FROM_HTML_SEPARATOR_LINE_BREAK_LIST: Integer read _GetFROM_HTML_SEPARATOR_LINE_BREAK_LIST;
    property FROM_HTML_SEPARATOR_LINE_BREAK_LIST_ITEM: Integer read _GetFROM_HTML_SEPARATOR_LINE_BREAK_LIST_ITEM;
    property FROM_HTML_SEPARATOR_LINE_BREAK_PARAGRAPH: Integer read _GetFROM_HTML_SEPARATOR_LINE_BREAK_PARAGRAPH;
    property TO_HTML_PARAGRAPH_LINES_CONSECUTIVE: Integer read _GetTO_HTML_PARAGRAPH_LINES_CONSECUTIVE;
    property TO_HTML_PARAGRAPH_LINES_INDIVIDUAL: Integer read _GetTO_HTML_PARAGRAPH_LINES_INDIVIDUAL;
  end;

  [JavaSignature('android/text/Html')]
  JHtml = interface(JObject)
    ['{937D7A71-86A7-478B-A43E-299BDD1333E9}']
  end;
  TJHtml = class(TJavaGenericImport<JHtmlClass, JHtml>)
  end;

  JHtml_ImageGetterClass = interface(JObjectClass)
    ['{02BF8F98-1B7C-4BA6-80C0-6BC4A3EDAD9C}']
  end;

  [JavaSignature('android/text/Html$ImageGetter')]
  JHtml_ImageGetter = interface(JObject)
    ['{9C6C6F1F-808A-4642-9991-63017C382C14}']
    function getDrawable(source: JString): JDrawable; cdecl;
  end;
  TJHtml_ImageGetter = class(TJavaGenericImport<JHtml_ImageGetterClass, JHtml_ImageGetter>)
  end;

  // Placeholder declarations
  JHtml_TagHandlerClass = interface(JObjectClass)
    ['{79B76907-D8D1-44D7-A9AF-E66A8A49FEDE}']
  end;

  [JavaSignature('android/text/Html$TagHandler')]
  JHtml_TagHandler = interface(JObject)
    ['{C972415C-A1B7-419C-9C98-71A4495FA361}']
  end;
  TJHtml_TagHandler = class(TJavaGenericImport<JHtml_TagHandlerClass, JHtml_TagHandler>)
  end;

implementation

end.
