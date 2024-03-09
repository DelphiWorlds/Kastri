unit DW.Proj.Types;

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

type
  TProjConfig = record
    Name: string;
    Ident: string;
    constructor Create(const AName: string; const AIdent: string);
  end;

  TProjConfigs = TArray<TProjConfig>;

implementation

{ TProjConfig }

constructor TProjConfig.Create(const AName, AIdent: string);
begin
  Name := AName;
  Ident := AIdent;
end;

end.
