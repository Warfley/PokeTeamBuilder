unit pktb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, PkTBTypes, listrecords, math;

type
  TSelectLanguageEvent = function(const Languages: TLanguageList): integer of object;
  TSelectGenerationEvent = function(const Generations: TGenerationList): integer of
    object;
  TSelectMovesEvent = function(const Moves: TMoveList): TDynIntArray of object;
  TSelectObtainKindsEvent = function: TObtainableKinds of object;
  TSelectPokemonEvent = function(const Pokemon: TPokemonList): TDynIntArray of object;
  TSelectIgnoresEvent = function(const Pokemon: TPokemonList): TDynIntArray of object;
  TTeamGeneratedEvent = procedure(const Team: TTeam) of object;

  EInvalidParamsException = class(Exception);

  { TPKTB }

  TPKTB = class
  private
    FBaseTeam: TTeam;
    FDB: TDBConnection;
    FLanguage: TLanguage;
    FMoves: TMoveList;
    FEdition: TEdition;
    FPokePool: TPokemonList;
    FStrengthTable: TStrengthTable;
    // Events
    FTeamGenerated: TTeamGeneratedEvent;

    procedure GenTeam(var Team: TTeam; Count: integer; var Pool: TPokemonList;
      var Moves: TMoveList);
    procedure CalcStrength(var Team: TTeam);
  public
    constructor Create(DB: TDBConnection);
    destructor Destroy; override;
    procedure GenerateTeams(TeamSize, TeamCount: integer;
      SelectLanguage: TSelectLanguageEvent; SelectGeneration: TSelectGenerationEvent;
      SelectMoves: TSelectMovesEvent; SelectObtainableKinds: TSelectObtainKindsEvent;
      SelectPokemon: TSelectPokemonEvent; SelectIgnore: TSelectIgnoresEvent;
      TeamGenerated: TTeamGeneratedEvent);
  end;

const
  MoveValue = 10;
  FactorDivisor = 40;
  Shuffle = 3;


implementation

{ TPKTB }

function GetStrength(TID: Integer; Team: TTeam): Integer;
var
  i: Integer;
begin
  for i:=0 to Length(Team.Strength) -1 do
    if Team.Strength[i].TID = TID then
    begin
      Result:=Team.Strength[i].Factor;
      Exit;
    end;
  Result:=0;
end;

procedure TPKTB.GenTeam(var Team: TTeam; Count: integer; var Pool: TPokemonList;
  var Moves: TMoveList);
var
  Weight: array of Integer;
  pkmn: TPokemon;
  w, j, i: Integer;
  t: TTypeStrength;
begin
  CalcStrength(Team);
  if Count = 0 then
    exit;
  SetLength(Weight, Pool.Count);
  for i:=0 to Pool.Count -1 do
  begin
    w:=0;
    pkmn:=Pool[i];
    for j:=0 to Length(pkmn.Moves) - 1 do
    begin
      if pkmn.Moves[j].Available and (Moves.FindMove(pkmn.Moves[j].MID) >= 0) then
        if (pkmn.Type1=Moves[j].TypeID) or (pkmn.Type2=Moves[j].TypeID) then
           inc(w, MoveValue*2)
        else
          inc(w, MoveValue);
    end;
    j:=FStrengthTable.FindType(pkmn.Type1);
    if j<0 then j:=FStrengthTable.FindType(1);
    t:=FStrengthTable[j];
    for j := 0 to Length(t.Factors) - 1 do
      if GetStrength(t.Factors[j].TID, Team) = 0 then
        inc(w, t.Factors[j].Factor div FactorDivisor);
    if pkmn.Type2>0 then
    begin
      t:=FStrengthTable[FStrengthTable.FindType(pkmn.Type1)];
      for j := 0 to Length(t.Factors) - 1 do
        if GetStrength(t.Factors[j].TID, Team) = 0 then
          inc(w, t.Factors[j].Factor div FactorDivisor);
    end;
    Weight[i]:=w+Random(Count**Shuffle);
  end;
  i:=-1;
  j:=-1;
  for w:=0 to Pool.Count-1 do
    if Weight[w] > j then
    begin
      j:=Weight[w];
      i:=w;
      if j > 100 then if Random(1000)<Shuffle*Count*2 then Break;
    end;
  for j:=0 to Length(Team.Pokemon) -1 do
    if Team.Pokemon[j].ID = 0 then
    begin
      Team.Pokemon[j] := Pool[i];
      Pool.Delete(i);
      for w:=0 to Length(Team.Pokemon[j].Moves)-1 do
        if Team.Pokemon[j].Moves[w].Available then
        begin
          i:=Moves.FindMove(Team.Pokemon[j].Moves[w].MID);
          if i >=0 then
            Moves.Delete(i);
        end;
      Break;
    end;
  GenTeam(Team, Count-1, Pool, Moves);
end;

procedure TPKTB.CalcStrength(var Team: TTeam);

  procedure AddType(tp, val: integer);
  var
    i: integer;
  begin
    for i := 0 to Length(Team.Strength) - 1 do
      if Team.Strength[i].TID = tp then
        Team.Strength[i].Factor += val;
  end;

var
  i, t1, t2, j: integer;
begin
  for i := 0 to Length(Team.Strength) - 1 do
    Team.Strength[i].Factor := 0;
  for i := 0 to Length(Team.Pokemon) - 1 do
  begin
    if Team.Pokemon[i].ID=0 then Continue;
    t1 := FStrengthTable.FindType(Team.Pokemon[i].Type1);
    if t1 < 0 then t1 := FStrengthTable.FindType(1);
    for j := 0 to length(FStrengthTable[t1].Factors) - 1 do
      AddType(FStrengthTable[t1].Factors[j].TID, FStrengthTable[t1].Factors[j].Factor);
    if Team.Pokemon[i].Type2 > 0 then
    begin
      t2 := FStrengthTable.FindType(Team.Pokemon[i].Type2);   
      if t2 < 0 then t2 := FStrengthTable.FindType(1);
      if t2 = t1 then Continue;
      for j := 0 to length(FStrengthTable[t2].Factors) - 1 do
        AddType(FStrengthTable[t2].Factors[j].TID, FStrengthTable[t2].Factors[j].Factor);
    end;
  end;
end;

constructor TPKTB.Create(DB: TDBConnection);
begin
  Randomize;
  FDB := DB;
  FMoves := TMoveList.Create;
  FPokePool := TPokemonList.Create;
  FStrengthTable := TStrengthTable.Create;
end;

destructor TPKTB.Destroy;
begin
  FMoves.Free;
  FPokePool.Free;
  inherited Destroy;
end;

procedure TPKTB.GenerateTeams(TeamSize, TeamCount: integer;
  SelectLanguage: TSelectLanguageEvent; SelectGeneration: TSelectGenerationEvent;
  SelectMoves: TSelectMovesEvent; SelectObtainableKinds: TSelectObtainKindsEvent;
  SelectPokemon: TSelectPokemonEvent; SelectIgnore: TSelectIgnoresEvent;
  TeamGenerated: TTeamGeneratedEvent);
var
  ll: TLanguageList;
  lg: TGenerationList;
  lm: TMoveList;
  ia: TDynIntArray;
  mv: TDynIntArray;
  i: integer;
  t: TTeam;
  btSize: Integer;
  localPool: TPokemonList;
begin
  if not (Assigned(SelectLanguage) and Assigned(SelectGeneration) and
    Assigned(SelectMoves) and Assigned(SelectObtainableKinds) and
    Assigned(SelectPokemon) and Assigned(SelectIgnore) and
    Assigned(TeamGenerated) and (TeamSize > 0) and (TeamCount > 0)) then
    raise EInvalidParamsException.Create('Invalid parameter');

  FTeamGenerated := TeamGenerated;

  ll := TLanguageList.Create;
  try
    ll.LoadLanguages(FDB);
    FLanguage := ll[SelectLanguage(ll)];
  finally
    ll.Free;
  end;
  lg := TGenerationList.Create;
  try
    lg.LoadGenerations(FDB, FLanguage.ID);
    FEdition := lg[SelectGeneration(lg)];
  finally
    lg.Free;
  end;
  lm := TMoveList.Create;
  try
    lm.LoadMoves(FDB, FLanguage.ID, FEdition.GenerationID);
    FMoves.Clear;
    ia := SelectMoves(lm);
    for i in ia do
      FMoves.Add(lm[i]);
  finally
    lm.Free;
  end;
  SetLength(ia, 0);
  SetLength(FBaseTeam.Pokemon, TeamSize);
  FStrengthTable.LoadStrengthTable(FDB, FLanguage.ID, FEdition.GenerationID);
  SetLength(FBaseTeam.Strength, FStrengthTable.Count);
  for i := 0 to Length(FBaseTeam.Strength) - 1 do
  begin
    FBaseTeam.Strength[i].TID := FStrengthTable[i].ID;  
    FBaseTeam.Strength[i].Name := FStrengthTable[i].Name;
  end;

  SetLength(mv, FMoves.Count);
  for i:=0 to FMoves.Count-1 do
    mv[i]:=FMoves[i].AttackID;

  btSize:=0;

  FPokePool.LoadPokemons(FDB, FLanguage.ID, FEdition, SelectObtainableKinds(), mv);
  ia := SelectPokemon(FPokePool);
  for i := 0 to length(ia) - 1 do
  begin
    inc(btSize);
    FBaseTeam.Pokemon[i] := FPokePool[ia[i] - i];
    FPokePool.Delete(ia[i] - i);
  end;
  SetLength(ia,0);

  ia := SelectIgnore(FPokePool);
  for i := 0 to length(ia) - 1 do
    FPokePool.Delete(ia[i] - i);

  localPool:=TPokemonList.Create;
  lm:=TMoveList.Create;
  try
  for i := 0 to TeamCount - 1 do
  begin
    localPool.Assign(FPokePool);
    lm.Assign(FMoves);
    t:=FBaseTeam;
    SetLength(t.Pokemon, Length(t.Pokemon));
    FillChar(t.Pokemon[btSize], SizeOf(TPokemon)*TeamSize-btSize, #00);
    SetLength(t.Strength, Length(t.Strength));
    GenTeam(t, TeamSize-btSize, localPool, lm);
    TeamGenerated(t);
  end;
  finally
    lm.Free;
    localPool.Free;
  end;
end;

end.

