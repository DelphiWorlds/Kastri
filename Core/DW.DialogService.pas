unit DW.DialogService;

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
  // RTL
  System.UITypes,
  // FMX
  FMX.Dialogs;

type
  /// <summary>
  ///   Provides convenience methods for showing dialogs
  /// </summary>
  TDialog = class(TObject)
  private
    class procedure ModalResultIgnore(const AResult: TModalResult);
  public
    class procedure Confirm(const AMsg: string; const ADefaultNo: Boolean; const ACloseProc: TInputCloseDialogProc);
    class procedure General(const AMsg: string; const AType: TMsgDlgType; const ACloseProc: TInputCloseDialogProc = nil);
    class procedure Information(const AMsg: string; const ACloseProc: TInputCloseDialogProc = nil);
    class procedure Warning(const AMsg: string; const ACloseProc: TInputCloseDialogProc = nil);
  end;

implementation

uses
  // FMX
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

class procedure TDialog.General(const AMsg: string; const AType: TMsgDlgType; const ACloseProc: TInputCloseDialogProc = nil);
begin
  if Assigned(ACloseProc) then
    TDialogService.MessageDialog(AMsg, AType, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, ACloseProc)
  else
    TDialogService.MessageDialog(AMsg, AType, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, ModalResultIgnore);
end;

class procedure TDialog.Information(const AMsg: string; const ACloseProc: TInputCloseDialogProc = nil);
begin
  General(AMsg, TMsgDlgType.mtInformation, ACloseProc);
end;

class procedure TDialog.Warning(const AMsg: string; const ACloseProc: TInputCloseDialogProc = nil);
begin
  General(AMsg, TMsgDlgType.mtWarning, ACloseProc);
end;

end.
