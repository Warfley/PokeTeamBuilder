unit listrecords;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  { TLanguage }

  TLanguage = record
    Name: String;
    ID: Integer;
    Identefier: String;
    class operator Equal(a, b: TLanguage): Boolean;
  end;

  { TMove }

  TMove = record
    AttackID, TypeID: Integer;
    AttackName: String;
    class operator Equal(a, b: TMove): Boolean;
  end;

  { TEdition }

  TEdition = record
    ID, GenerationID, GroupID: Integer;
    Name: String;
    class operator Equal(a, b: TEdition): Boolean;
  end;

  TObtainableKind = (okCatchable, okSwarm, okDualSlot, okRecieved, okEvolves, okBackEvolve, okEvent);

  TMoveAvailable = record
    MID: Integer;
    Available: Boolean;
  end;

  { TPokemon }

  TPokemon = record
    ID, Type1, Type2: Integer;
    Name: String;
    ObtainableKind: TObtainableKind;
    Moves: array of TMoveAvailable;
    class operator Equal(a, b: TPokemon): Boolean;
  end;     

  TTypeFactor = record
      TID, Factor: Integer;
      Name: String;
  end;

  { TTeam }

  TTeam = record
    Strength: array of TTypeFactor;
    Pokemon: array of TPokemon;
    class operator Equal(a,b: TTeam): Boolean;
  end;

  { TTypeStrength }

  TTypeStrength = record
    Name: String;
    ID: Integer;
    Factors: array of TTypeFactor;
    class operator Equal(a,b: TTypeStrength): Boolean;
  end;

implementation

{ TTypeStrength }

class operator TTypeStrength.Equal(a, b: TTypeStrength): Boolean;
begin
  Result:=a.ID=b.ID;
end;

{ TTeam }

class operator TTeam.Equal(a, b: TTeam): Boolean;
var
  i, j: Integer;
begin
  Result := Length(a.Pokemon) = Length(b.pokemon);
  if not Result then exit;
  for i:=0 to Length(a.Pokemon)-1 do
    for j:=0 to Length(b.Pokemon)-1 do
      if a.Pokemon[i].ID <> b.Pokemon[j].ID then
      begin
        Result := False;
        exit;
      end;
end;

{ TPokemon }

class operator TPokemon.Equal(a, b: TPokemon): Boolean;
begin
  result := a.ID = b.ID
end;

{ TEdition }

class operator TEdition.Equal(a, b: TEdition): Boolean;
begin
  Result := a.ID = b.ID;
end;

{ TMove }

class operator TMove.Equal(a, b: TMove): Boolean;
begin
  result := (a.AttackID = b.AttackID);
end;

{ TLanguage }

class operator TLanguage.Equal(a, b: TLanguage): Boolean;
begin
  Result := a.ID = b.ID;
end;

end.

