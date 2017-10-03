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
    AttackID: Integer;
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

  { TPokemon }

  TPokemon = record
    ID, Type1, Type2: Integer;
    Name: String;
    ObtainableKind: TObtainableKind;
    class operator Equal(a, b: TPokemon): Boolean;
  end;

implementation

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

