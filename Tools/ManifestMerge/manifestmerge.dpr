program manifestmerge;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, System.IOUtils, System.Win.ComObj,
  Winapi.ActiveX,
  DW.ManifestMerger;

var
  LAppName, LMergePath, LManifestFileName: string;
  LIsInitialized: Boolean;

procedure ShowUsage;
begin
  Writeln(Format('%s merges a file or files containing merge info into an AndroidManifest.xml', [LAppName]));
  Writeln;
  Writeln(Format('Usage: %s <mergefile/folder> <androidmanifest>', [LAppName]));
  Writeln;
  Writeln('Where:');
  Writeln('  <mergefile/folder> is an xml file containing merge information **OR** a folder containing multiple merge files matching the pattern: ' +
    '*Manifest.merge.xml');
  Writeln('  <androidmanifest> is a file (usually AndroidManifest.xml) belonging to a project');
  // Readln;
  ExitCode := 99;
end;

begin
  LAppName := TPath.GetFileNameWithoutExtension(ParamStr(0));
  if ParamCount > 1 then
  begin
    LMergePath := ParamStr(1);
    LManifestFileName := ParamStr(2);
    try
      LIsInitialized := Succeeded(CoInitialize(nil));
      try
        ExitCode := TManifestMerger.MergeManifest(LMergePath, LManifestFileName);
      finally
        if LIsInitialized then
          CoUninitialize;
      end;
    except
      on E: Exception do
      begin
        Writeln(Format('%s caused an unhandled exception - %s: %s', [LAppName, E.ClassName, E.Message]));
        ExitCode := MaxInt;
      end;
    end;
  end
  else
    ShowUsage;
end.

