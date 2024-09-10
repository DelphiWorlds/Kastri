unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Messaging,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  DW.BackgroundTasks.iOS;

type
  TForm1 = class(TForm)
    Image1: TImage;
  private
    FAppRefreshTask: IBackgroundTask;
    FProcessingTask: IBackgroundTask;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
    procedure BackgroundRefreshStatusChangeHandler(Sender: TObject; const AStatus: TBackgroundRefreshStatus);
    procedure RegisterBackgroundTasks;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.Net.HttpClient, System.IOUtils,
  FMX.Platform,
  DW.OSLog;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
  BackgroundTaskManager.OnBackgroundRefreshStatusChange := BackgroundRefreshStatusChangeHandler;
  RegisterBackgroundTasks;
end;

destructor TForm1.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  inherited;
end;

procedure TForm1.BackgroundRefreshStatusChangeHandler(Sender: TObject; const AStatus: TBackgroundRefreshStatus);
begin
  // Take any action here that is required when the refresh status changes
end;

procedure TForm1.ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
var
  LFileName: string;
begin
  case TApplicationEventMessage(AMsg).Value.Event of
    TApplicationEvent.BecameActive:
    begin
      // When the app becomes active, re-load the image that may have been updated with the background task
      LFileName := TPath.Combine(TPath.GetDocumentsPath, 'Random.png');
      if TFile.Exists(LFileName) then
        Image1.Bitmap.LoadFromFile(LFileName);
      // Additionally, the BackgroundTaskManager.BackgroundRefreshStatus could be examined to determine whether background tasks will run,
      //   and the user could be informed of this
    end;
  end;
end;

procedure TForm1.RegisterBackgroundTasks;
begin
  FAppRefreshTask := BackgroundTaskManager.ScheduleTask('com.delphiworlds.bgdemo.apprefresh', TBackgroundTaskKind.AppRefresh, 2 * 60,
    function: Boolean
    var
      LHTTP: THTTPClient;
      LResponse: IHTTPResponse;
      LStream: TMemoryStream;
      LFileName: string;
    begin
      TOSLog.d('FAppRefreshTask Handler');
      // AppRefresh tasks should be SHORT (typically < 30s)
      LHTTP := THTTPClient.Create;
      try
        LResponse := LHTTP.Get('https://picsum.photos/300/200');
        if LResponse.StatusCode = 200 then
        begin
          LFileName := TPath.Combine(TPath.GetDocumentsPath, 'Random.png');
          LStream := TMemoryStream.Create;
          try
            LStream.CopyFrom(LResponse.ContentStream);
            LStream.SaveToFile(LFileName);
            TOSLog.d('> Updated: %s', [LFileName]);
          finally
            LStream.Free;
          end;
        end
        else
          TOSLog.d('> Status: %d - %s', [LResponse.StatusCode, LResponse.StatusText]);
      finally
        LHTTP.Free;
      end;
      Result := True;
    end
  );
  FProcessingTask := BackgroundTaskManager.ScheduleTask('com.delphiworlds.bgdemo.processing', TBackgroundTaskKind.Processing, 5 * 60,
    function: Boolean
    begin
      TOSLog.d('FProcessingTask Handler');
      // Processing tasks can be longer than AppRefresh, up to a few minutes, depending on system conditions
      // Execute some longer running task, here
      Result := True;
    end
  );
end;

end.
