unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Memo.Types, FMX.StdCtrls,
  DW.Toolbar.Mac;

type
  TForm1 = class(TForm)
    Memo: TMemo;
    OpenDialog: TOpenDialog;
  private
    FToolbar: TMacOSToolbar;
    procedure CreateToolbar;
    procedure ItemFileOpenClickHandler(Sender: TObject);
    procedure ItemEditCopyClickHandler(Sender: TObject);
    procedure ItemEditCopyValidateHandler(Sender: TObject; var AEnable: Boolean);
    procedure ItemInfoClickHandler(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Macapi.AppKit, Macapi.ObjectiveC, Macapi.CocoaTypes, Macapi.Helpers, Macapi.Foundation,
  FMX.Platform.Mac;

const
  NSWindowTitleVisible = 0;
  NSWindowTitleHidden = 1;

type
  NSWindowTitleVisibility = NSInteger;

  NSWindow = interface(Macapi.AppKit.NSWindow)
    ['{5782E82A-D658-4003-881E-14CEC0F2F886}']
    procedure setTitleVisibility(titleVisibility: NSWindowTitleVisibility); cdecl;
  end;
  TNSWindow = class(TOCGenericImport<NSWindowClass, NSWindow>) end;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  CreateToolbar;
end;

destructor TForm1.Destroy;
begin
  FToolbar.Free;
  inherited;
end;

procedure TForm1.CreateToolbar;
var
  LItem: TMacOSToolbarItem;
begin
  FToolbar := TMacOSToolbar.Create(ClassName);
  LItem := FToolbar.AddItem('FileOpen');
  LItem.Caption := 'Open';
  LItem.ImageName := 'Open.png';
  LItem.Hint := 'Open file';
  LItem.OnClick := ItemFileOpenClickHandler;
  LItem := FToolbar.AddItem('EditCopy');
  LItem.Caption := 'Copy';
  LItem.ImageName := 'Copy.png';
  LItem.Hint := 'Copy selected text';
  LItem.OnClick := ItemEditCopyClickHandler;
  LItem.OnValidate := ItemEditCopyValidateHandler;
  // Add a flexible spacer to right align the remaining button
  FToolbar.AddFlexibleSpaceItem;
  // Add a button onto the end of the toolbar
  LItem := FToolbar.AddItem('Info');
  LItem.Caption := 'Info';
  LItem.ImageName := 'Info.png';
  LItem.Hint := 'App information';
  LItem.OnClick := ItemInfoClickHandler;
  FToolbar.Form := Self;
  TNSWindow.Wrap(NSObjectToID(WindowHandleToPlatform(Handle).Wnd)).setTitleVisibility(NSWindowTitleHidden);
end;

procedure TForm1.ItemEditCopyClickHandler(Sender: TObject);
begin
  Memo.CopyToClipboard;
end;

procedure TForm1.ItemEditCopyValidateHandler(Sender: TObject; var AEnable: Boolean);
begin
  AEnable := Memo.SelLength > 0;
end;

procedure TForm1.ItemFileOpenClickHandler(Sender: TObject);
begin
  if OpenDialog.Execute then
    Memo.Lines.LoadFromFile(OpenDialog.FileName);
end;

procedure TForm1.ItemInfoClickHandler(Sender: TObject);
begin
  ShowMessage('Info');
end;

end.
