unit DW.SpeechRecognition.Default;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{          DelphiWorlds Cross-Platform Library          }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  DW.SpeechRecognition;

type
  TPlatformSpeechRecognition = class(TCustomPlatformSpeechRecognition)
  protected
    class function IsSupported: Boolean; override;
  protected
    function IsRecording: Boolean; override;
    procedure StartRecording; override;
    procedure StopRecording; override;
  end;

implementation

{ TPlatformSpeechRecognition }

function TPlatformSpeechRecognition.IsRecording: Boolean;
begin
  Result := False;
end;

class function TPlatformSpeechRecognition.IsSupported: Boolean;
begin
  Result := False;
end;

procedure TPlatformSpeechRecognition.StartRecording;
begin
  //
end;

procedure TPlatformSpeechRecognition.StopRecording;
begin
  //
end;

end.
