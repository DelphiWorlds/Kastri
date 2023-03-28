unit DW.SpeechRecognition.Default;

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
  DW.SpeechRecognition;

type
  TPlatformSpeechRecognition = class(TCustomPlatformSpeechRecognition)
  protected
    function IsRecording: Boolean; override;
    procedure RequestPermission; override;
    procedure StartRecording; override;
    procedure StopRecording; override;
  public
    class function IsSupported: Boolean;
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

procedure TPlatformSpeechRecognition.RequestPermission;
begin
  //
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
