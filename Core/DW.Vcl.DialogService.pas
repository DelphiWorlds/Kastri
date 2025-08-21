unit DW.Vcl.DialogService;

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

{$SCOPEDENUMS ON}

uses
  // RTL
  System.UITypes;

type
  TYesNoCancel = (Yes, No, Cancel);

  TDialog = class(TObject)
  public
    class function Confirm(const AMsg: string; const ADefaultNo: Boolean): Boolean;
    class procedure General(const AMsg: string; const AType: TMsgDlgType);
    class procedure Information(const AMsg: string);
    class procedure Warning(const AMsg: string);
    class function YesNoCancel(const AMsg: string; const ADefaultButton: TYesNoCancel): TYesNoCancel;
  end;

implementation

uses
  Vcl.Dialogs;

{ TDialog }

class function TDialog.Confirm(const AMsg: string; const ADefaultNo: Boolean): Boolean;
const
  cDefaultButtons: array[Boolean] of TMsgDlgBtn = (TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo);
begin
  Result := MessageDlg(AMsg, TMsgDlgType.mtConfirmation, mbYesNo, 0, cDefaultButtons[ADefaultNo]) = mrYes;
end;

class procedure TDialog.General(const AMsg: string; const AType: TMsgDlgType);
begin
  MessageDlg(AMsg, AType, [TMsgDlgBtn.mbOK], 0, TMsgDlgBtn.mbOK);
end;

class procedure TDialog.Information(const AMsg: string);
begin
  General(AMsg, TMsgDlgType.mtInformation);
end;

class procedure TDialog.Warning(const AMsg: string);
begin
  General(AMsg, TMsgDlgType.mtWarning);
end;

class function TDialog.YesNoCancel(const AMsg: string; const ADefaultButton: TYesNoCancel): TYesNoCancel;
const
  cDefaultButtons: array[TYesNoCancel] of TMsgDlgBtn = (TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel);
begin
  case MessageDlg(AMsg, TMsgDlgType.mtConfirmation, mbYesNoCancel, 0, cDefaultButtons[ADefaultButton]) of
    mrYes:
      Result := TYesNoCancel.Yes;
    mrNo:
      Result := TYesNoCancel.No;
    mrCancel:
      Result := TYesNoCancel.Cancel;
  else
    Result := TYesNoCancel.Cancel;
  end;
end;

end.
