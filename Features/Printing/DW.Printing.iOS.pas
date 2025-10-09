unit DW.Printing.iOS;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.Foundation, iOSapi.UIKit, iOSapi.CoreGraphics,
	// DW
  DW.Printing;

type
  UIPrintInteractionController = interface;

  UIPrintInteractionCompletionHandler = procedure(controller: UIPrintInteractionController; completed: Boolean; error: NSError) of object;

  UIPrintInteractionControllerClass = interface(NSObjectClass)
    ['{42B3B5B6-6C26-48E1-B9C1-9530256847ED}']
    {class} function canPrintData(data: NSData): Boolean; cdecl;
    {class} function canPrintURL(url: NSURL): Boolean; cdecl;
    {class} function isPrintingAvailable: Boolean; cdecl;
    {class} function printableUTIs: NSSet; cdecl;
    {class} function sharedPrintController: UIPrintInteractionController; cdecl;
  end;

  UIPrintInteractionController = interface(NSObject)
    ['{1F1B3A35-FCD7-439C-B384-4F070EF2A3D1}']
    function delegate: Pointer; cdecl;
    procedure dismissAnimated(animated: Boolean); cdecl;
    function presentAnimated(animated: Boolean; completionHandler: UIPrintInteractionCompletionHandler): Boolean; cdecl;
    function presentFromBarButtonItem(item: UIBarButtonItem; animated: Boolean; completionHandler: UIPrintInteractionCompletionHandler): Boolean; cdecl;
    function presentFromRect(rect: CGRect; inView: UIView; animated: Boolean; completionHandler: UIPrintInteractionCompletionHandler): Boolean; cdecl;
    function printFormatter: UIPrintFormatter; cdecl;
    function printInfo: UIPrintInfo; cdecl;
    function printingItem: Pointer; cdecl;
    function printingItems: NSArray; cdecl;
    function printPageRenderer: UIPrintPageRenderer; cdecl;
    function printPaper: UIPrintPaper; cdecl;
    function printToPrinter(printer: UIPrinter; completionHandler: UIPrintInteractionCompletionHandler): Boolean; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setPrintFormatter(printFormatter: UIPrintFormatter); cdecl;
    procedure setPrintInfo(printInfo: UIPrintInfo); cdecl;
    procedure setPrintingItem(printingItem: Pointer); cdecl;
    procedure setPrintingItems(printingItems: NSArray); cdecl;
    procedure setPrintPageRenderer(printPageRenderer: UIPrintPageRenderer); cdecl;
    procedure setShowsNumberOfCopies(showsNumberOfCopies: Boolean); cdecl;
    procedure setShowsPageRange(showsPageRange: Boolean); cdecl; // API_DEPRECATED("Pages can be removed from the print preview, so page range is always shown.", ios(4.2, 10.0))
    procedure setShowsPaperSelectionForLoadedPapers(showsPaperSelectionForLoadedPapers: Boolean); cdecl;
    function showsNumberOfCopies: Boolean; cdecl;
    function showsPageRange: Boolean; cdecl; // API_DEPRECATED("Pages can be removed from the print preview, so page range is always shown.", ios(4.2, 10.0))
    function showsPaperSelectionForLoadedPapers: Boolean; cdecl;
  end;
  TUIPrintInteractionController = class(TOCGenericImport<UIPrintInteractionControllerClass, UIPrintInteractionController>) end;

  TPlatformPrinting = class(TInterfacedObject, IPrinting)
  private
    function GetController(const AJobNumber: Integer): UIPrintInteractionController;
    procedure PrintCompletionHandler(controller: UIPrintInteractionController; completed: Boolean; error: NSError);
  public
    function GetPrintStatus(const AIndex: Integer): TPrintJobStatus;
    function Print(const AAdapter: IInterface): Integer; overload;
    function Print(const AFileName: string): Integer; overload;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // macOS
  Macapi.Helpers,
  // DW
  DW.OSLog,
  DW.OSDevice;

{ TPlatformPrinting }

function TPlatformPrinting.GetController(const AJobNumber: Integer): UIPrintInteractionController;
var
  LInfo: UIPrintInfo;
begin
  {$IF (CompilerVersion < 37)}
  LInfo := TUIPrintInfo.Wrap(TUIPrintInfo.OCClass.printInfo);
  {$ELSE}
  LInfo := TUIPrintInfo.OCClass.printInfo;
  {$ENDIF}
  LInfo.setJobName(StrToNSStr(Format('%s Print Job %d', [TOSDevice.GetDeviceName, AJobNumber])));
  LInfo.setOutputType(UIPrintInfoOutputGeneral);
  Result := TUIPrintInteractionController.OCClass.sharedPrintController;
  Result.setPrintInfo(LInfo);
end;

function TPlatformPrinting.GetPrintStatus(const AIndex: Integer): TPrintJobStatus;
begin
  Result := TPrintJobStatus.None;
end;

function TPlatformPrinting.Print(const AFileName: string): Integer;
var
  LController: UIPrintInteractionController;
begin
  Result := 1;
  LController := GetController(Result);
  LController.setPrintingItem(TNSData.OCClass.dataWithContentsOfFile(StrToNSStr(AFileName)));
  LController.presentAnimated(True, PrintCompletionHandler);
end;

function TPlatformPrinting.Print(const AAdapter: IInterface): Integer;
var
  LController: UIPrintInteractionController;
begin
  if AAdapter <> nil then
  begin
    Result := 1;
    LController := GetController(Result);
    LController.setPrintFormatter(UIPrintFormatter(AAdapter));
    LController.presentAnimated(True, PrintCompletionHandler);
  end
  else
    Result := -1;
end;

procedure TPlatformPrinting.PrintCompletionHandler(controller: UIPrintInteractionController; completed: Boolean; error: NSError);
begin
  TOSLog.d('TPlatformPrinting.PrintCompletionHandler');
end;

end.
