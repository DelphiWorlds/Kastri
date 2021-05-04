unit DW.UniversalLinks;

interface

type
  TOpenApplicationContext = class(TObject)
  private
    FURL: string;
  public
    constructor Create(const AURL: string);
    property URL: string read FURL;
  end;

implementation

{$IF Defined(IOS)}
uses
  DW.UniversalLinks.iOS;
{$ENDIF}

{ TOpenApplicationContext }

constructor TOpenApplicationContext.Create(const AURL: string);
begin
  inherited Create;
  FURL := AURL;
end;

end.
