unit DW.Printing;

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

type
  TPrintJobStatus = (None, Blocked, Cancelled, Completed, Failed, Queued, Started);

  IPrinting = interface(IInterface)
    ['{C771CC67-4A94-462D-A834-5BACA49174A2}']
    function GetPrintStatus(const AIndex: Integer): TPrintJobStatus;
    /// <summary>
    ///   Prints a file via a supported adapter
    /// </summary>
    /// <remarks>
    ///   On Android, AAdapter must be a descendant of PrintDocumentAdapter
    ///   On iOS, AAdapter must be a descendant of UIPrintFormatter
    /// </remarks>
    function Print(const AAdapter: IInterface): Integer; overload;
    /// <summary>
    ///   Prints a file if the file type is supported
    /// </summary>
    /// <remarks>
    ///   On Android: Files with a .pdf extension are printed using a simple PDF renderer. All other files are printed via an internal WebView
    ///   On iOS: Printing of images that are compatible with the ImageIO framework, and PDF files is supported
    /// </remarks>
    function Print(const AFileName: string): Integer; overload;
  end;

var
  Printing: IPrinting;

implementation

{$IF Defined(ANDROID)}
uses
  DW.Printing.Android;
{$ELSEIF Defined(IOS)}
uses
  DW.Printing.iOS;
{$ELSE}

type
  TPlatformPrinting = class(TInterfacedObject, IPrinting)
  public
    { IPrinting }
    function GetPrintStatus(const AIndex: Integer): TPrintJobStatus;
    function Print(const AAdapter: IInterface): Integer; overload;
    function Print(const AFileName: string): Integer; overload;
  end;

function TPlatformPrinting.GetPrintStatus(const AIndex: Integer): TPrintJobStatus;
begin
  Result := TPrintJobStatus.None;
end;

function TPlatformPrinting.Print(const AAdapter: IInterface): Integer;
begin
  Result := -1;
end;

function TPlatformPrinting.Print(const AFileName: string): Integer;
begin
  Result := -1;
end;
{$ENDIF}

initialization
  Printing := TPlatformPrinting.Create;

end.
