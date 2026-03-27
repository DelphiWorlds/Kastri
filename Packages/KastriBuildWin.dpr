program KastriBuildWin;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  DW.Precompile in '..\Core\DW.Precompile.pas';

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
