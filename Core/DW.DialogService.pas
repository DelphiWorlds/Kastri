unit DW.DialogService;

{*******************************************************}
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

interface

uses
  System.UITypes,
  FMX.Dialogs;

type
  TDialog = class(TObject)
  private
    class procedure ModalResultIgnore(const AResult: TModalResult);
  public
    class procedure Confirm(const AMsg: string; const ADefaultNo: Boolean; const ACloseProc: TInputCloseDialogProc);
    class procedure General(const AMsg: string; const AType: TMsgDlgType);
    class procedure Information(const AMsg: string);
    class procedure Warning(const AMsg: string);
  end;

implementation

uses
  FMX.DialogService;

{ TDialog }

class procedure TDialog.ModalResultIgnore(const AResult: TModalResult);
begin
  // Do nothing
end;

class procedure TDialog.Confirm(const AMsg: string; const ADefaultNo: Boolean; const ACloseProc: TInputCloseDialogProc);
const
  cDefaultButtons: array[Boolean] of TMsgDlgBtn = (TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo);
begin
  TDialogService.MessageDialog(AMsg, TMsgDlgType.mtConfirmation, mbYesNo, cDefaultButtons[ADefaultNo], 0, ACloseProc);
end;

class procedure TDialog.General(const AMsg: string; const AType: TMsgDlgType);
begin
  TDialogService.MessageDialog(AMsg, AType, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, ModalResultIgnore);
end;

class procedure TDialog.Information(const AMsg: string);
begin
  General(AMsg, TMsgDlgType.mtInformation);
end;

class procedure TDialog.Warning(const AMsg: string);
begin
  General(AMsg, TMsgDlgType.mtWarning);
end;

end.
