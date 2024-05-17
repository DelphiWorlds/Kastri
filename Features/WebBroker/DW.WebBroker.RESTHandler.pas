unit DW.WebBroker.RESTHandler;

// **NOTE**: The WebBroker feature in Kastri is being deprecated - no further changes will be made.

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
    procedure AfterHandle; override;
    procedure BeforeHandle; override;
    property Contents: TJSONValue read FContents;
  end;

implementation

{ TWebBrokerRESTHandler }

procedure TWebBrokerRESTHandler.BeforeHandle;
begin
  FContents := TJSONObject.ParseJSONValue(Request.Content);
  if FContents = nil then
    FContents := TJSONObject.Create;
end;

procedure TWebBrokerRESTHandler.AfterHandle;
begin
  FContents.Free;
end;

end.
