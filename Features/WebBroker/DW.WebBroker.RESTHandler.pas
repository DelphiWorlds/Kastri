unit DW.WebBroker.RESTHandler;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2022 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // RTL
  System.JSON,
  // Web
  Web.HTTPApp,
  // DW
  DW.WebBroker.Handler;

type
  /// <summary>
  ///    Specialised web request handler class that parses the content into a JSON object
  /// </summary>
  TWebBrokerRESTHandler = class(TWebBrokerHandler)
  private
    FContents: TJSONValue;
  protected
    procedure AfterHandle(const ARequest: TWebRequest; const AResponse: TWebResponse); override;
    procedure BeforeHandle(const ARequest: TWebRequest; const AResponse: TWebResponse); override;
    property Contents: TJSONValue read FContents;
  end;

implementation

{ TWebBrokerRESTHandler }

procedure TWebBrokerRESTHandler.BeforeHandle(const ARequest: TWebRequest; const AResponse: TWebResponse);
begin
  FContents := TJSONObject.ParseJSONValue(ARequest.Content);
  if FContents = nil then
    FContents := TJSONObject.Create;
end;

procedure TWebBrokerRESTHandler.AfterHandle(const ARequest: TWebRequest; const AResponse: TWebResponse);
begin
  FContents.Free;
end;

end.
