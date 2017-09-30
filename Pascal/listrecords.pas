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

  { TVM }

  TVM = record
    ItemID,
    AttackID: Integer;
    AttackName,
    VMName: String;
    class operator Equal(a, b: TVM): Boolean;
  end;

  { TGeneration }

  TGeneration = record
    ID, GroupID: Integer;
    Name: String;
    class operator Equal(a, b: TGeneration): Boolean;
  end;

implementation

{ TGeneration }

class operator TGeneration.Equal(a, b: TGeneration): Boolean;
begin
  Result := a.ID = b.ID;
end;

{ TVM }

class operator TVM.Equal(a, b: TVM): Boolean;
begin
  result := (a.ItemID = b.ItemID) and (a.AttackID = b.AttackID);
end;

{ TLanguage }

class operator TLanguage.Equal(a, b: TLanguage): Boolean;
begin
  Result := a.ID = b.ID;
end;

end.

