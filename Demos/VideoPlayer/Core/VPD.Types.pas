unit VPD.Types;

interface

type
  THLSStreamItem = record
  public
    class function CompareChannel(const ALeft, ARight: THLSStreamItem): Integer; static;
  public
    Channel: string;
    Location: string;
    URL: string;
    MaxResolution: string;
  end;

  THLSStreamItems = TArray<THLSStreamItem>;

  THLSStreams = record
    Items: THLSStreamItems;
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string);
    procedure Sort;
  end;

implementation

uses
  System.IOUtils, System.SysUtils, System.Rtti, System.Generics.Collections, System.Generics.Defaults,
  Neon.Core.Persistence.JSON;

{ THLSStreamItem }

class function THLSStreamItem.CompareChannel(const ALeft, ARight: THLSStreamItem): Integer;
begin
  Result := CompareStr(ALeft.Channel, ARight.Channel);
end;

{ THLSStreams }

procedure THLSStreams.LoadFromFile(const AFileName: string);
begin
  if TFile.Exists(AFileName) then
  begin
    Items := TNeon.JSONToValue<THLSStreamItems>(TFile.ReadAllText(AFileName));
    Sort;
  end;
end;

procedure THLSStreams.SaveToFile(const AFileName: string);
begin
  TFile.WriteAllText(AFileName, TNeon.ValueToJSONString(TValue.From(Items)));
end;

procedure THLSStreams.Sort;
begin
  TArray.Sort<THLSStreamItem>(Items, TComparer<THLSStreamItem>.Construct(THLSStreamItem.CompareChannel));
end;

end.
