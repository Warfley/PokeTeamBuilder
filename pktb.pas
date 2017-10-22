unit pktb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, PkTBTypes, listrecords, Math;

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
    property Moves: TMoveList read FMoves;
    property StrengthTable: TStrengthTable read FStrengthTable;
  end;

const
  MoveValue = 200;
  FactorDivisor = 5;
  Shuffle = 5;


implementation

{ TPKTB }

function GetStrength(TID: integer; Team: TTeam): integer;
var
  i: integer;
begin
  for i := 0 to Length(Team.Strength) - 1 do
    if Team.Strength[i].TID = TID then
    begin
      Result := Team.Strength[i].Factor;
      Exit;
    end;
  Result := 0;
end;

function GetWeakness(TID: integer; Team: TTeam): integer;
var
  i: integer;
begin
  for i := 0 to Length(Team.Weakness) - 1 do
    if Team.Weakness[i].TID = TID then
    begin
      Result := Team.Weakness[i].Factor;
      Exit;
    end;
  Result := 0;
end;

procedure TPKTB.GenTeam(var Team: TTeam; Count: integer; var Pool: TPokemonList;
  var Moves: TMoveList);

  function GetTypeWeight(tid: integer; const SAvg, WAvg: integer): integer;
  var
    i, j: integer;
  begin
    Result := 0;
    if FStrengthTable.FindType(tid) < 0 then
      tid := 1;
    for i := 0 to FStrengthTable.Count - 1 do
      if FStrengthTable[i].ID = tid then
      begin
        for j := 0 to Length(FStrengthTable[i].Factors) - 1 do
          if GetStrength(tid, Team) <= SAvg then
          begin
            Inc(Result, FStrengthTable[i].Factors[j].Factor div FactorDivisor);
          end;
      end
      else if GetWeakness(FStrengthTable[i].ID, Team) >= WAvg then
        for j := 0 to Length(FStrengthTable[i].Factors) - 1 do
          if (FStrengthTable[i].Factors[j].TID = tid) then
          begin
            Dec(Result, FStrengthTable[i].Factors[j].Factor div FactorDivisor);
          end;

  end;

var
  Weight: array of integer;
  pkmn: TPokemon;
  w, j, i: integer;
  SAvg, WAvg: integer;
begin
  CalcStrength(Team);
  Savg := 0;
  Wavg:=0;
  for i := 0 to Length(Team.Strength) - 1 do
  begin
    Inc(Savg, Team.Strength[i].Factor);
    Inc(Wavg, Team.Weakness[i].Factor);
  end;
  Savg := Savg div Length(Team.Strength);
  WAvg := Wavg div Length(Team.Weakness);
  if Count = 0 then
    exit;
  SetLength(Weight, Pool.Count);
  for i := 0 to Pool.Count - 1 do
  begin
    w := 0;
    pkmn := Pool[i];
    for j := 0 to Length(pkmn.Moves) - 1 do
    begin
      if pkmn.Moves[j].Available and (Moves.FindMove(pkmn.Moves[j].MID) >= 0) then
        if (pkmn.Type1 = Moves[j].TypeID) or (pkmn.Type2 = Moves[j].TypeID) then
          Inc(w, (MoveValue div Count) * 2)
        else
          Inc(w, MoveValue div Count);
    end;
    Inc(w, GetTypeWeight(pkmn.Type1, SAvg, WAvg) div Count);
    if pkmn.Type2 > 0 then
      Inc(w, GetTypeWeight(pkmn.Type2, SAvg, WAvg) div Count);
    Weight[i] := w + Random((Count div 2) * Shuffle);
  end;
  i := -1;
  j := -1;
  for w := 0 to Pool.Count - 1 do
    if Weight[w] > j then
    begin
      j := Weight[w];
      i := w;
    end;
  for j := 0 to Length(Team.Pokemon) - 1 do
    if Team.Pokemon[j].ID = 0 then
    begin
      Team.Pokemon[j] := Pool[i];
      Pool.Delete(i);
      for w := 0 to Length(Team.Pokemon[j].Moves) - 1 do
        if Team.Pokemon[j].Moves[w].Available then
        begin
          i := Moves.FindMove(Team.Pokemon[j].Moves[w].MID);
          if i >= 0 then
            Moves.Delete(i);
        end;
      Break;
    end;
  GenTeam(Team, Count - 1, Pool, Moves);
end;

procedure TPKTB.CalcStrength(var Team: TTeam);

  procedure AddStrength(tp, val: integer);
  var
    i: integer;
  begin
    for i := 0 to Length(Team.Strength) - 1 do
      if Team.Strength[i].TID = tp then
        Team.Strength[i].Factor += val;
  end;

  procedure AddWeakness(tp, val: integer);
  var
    i: integer;
  begin
    for i := 0 to Length(Team.Weakness) - 1 do
      if Team.Weakness[i].TID = tp then
        Team.Weakness[i].Factor += val;
  end;

var
  i, t1, t2, j, k: integer;
begin
  for i := 0 to Length(Team.Strength) - 1 do
  begin
    Team.Strength[i].Factor := 0;
    Team.Weakness[i].Factor := 0;
  end;

  for i := 0 to Length(Team.Pokemon) - 1 do
  begin
    t1 := Team.Pokemon[i].Type1;
    if FStrengthTable.FindType(t1) < 0 then
      t1 := 1;
    t2 := Team.Pokemon[i].Type2;
    if (t2 > 0) and (FStrengthTable.FindType(t2) < 0) then
      t2 := 1;
    for j := 0 to FStrengthTable.Count - 1 do
      if (FStrengthTable[j].ID = t1) or (FStrengthTable[j].ID = t2) then
        for k := 0 to Length(FStrengthTable[j].Factors) - 1 do
          AddStrength(FStrengthTable[j].Factors[k].TID,
            FStrengthTable[j].Factors[k].Factor)
      else
        for k := 0 to Length(FStrengthTable[j].Factors) - 1 do
          if (FStrengthTable[j].Factors[k].TID = t1) or
            (FStrengthTable[j].Factors[k].TID = t2) then
            AddWeakness(FStrengthTable[j].ID, FStrengthTable[j].Factors[k].Factor);
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
  FStrengthTable.Free;
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
  btSize: integer;
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

  SetLength(FBaseTeam.Weakness, FStrengthTable.Count);
  for i := 0 to Length(FBaseTeam.Weakness) - 1 do
  begin
    FBaseTeam.Weakness[i].TID := FStrengthTable[i].ID;
    FBaseTeam.Weakness[i].Name := FStrengthTable[i].Name;
  end;

  SetLength(mv, FMoves.Count);
  for i := 0 to FMoves.Count - 1 do
    mv[i] := FMoves[i].AttackID;

  btSize := 0;

  FPokePool.LoadPokemons(FDB, FLanguage.ID, FEdition, SelectObtainableKinds(), mv);
  ia := SelectPokemon(FPokePool);
  for i := 0 to length(ia) - 1 do
  begin
    Inc(btSize);
    FBaseTeam.Pokemon[i] := FPokePool[ia[i] - i];
    FPokePool.Delete(ia[i] - i);
  end;
  SetLength(ia, 0);

  ia := SelectIgnore(FPokePool);
  for i := 0 to length(ia) - 1 do
    FPokePool.Delete(ia[i] - i);

  localPool := TPokemonList.Create;
  lm := TMoveList.Create;
  try
    for i := 0 to TeamCount - 1 do
    begin
      localPool.Assign(FPokePool);
      lm.Assign(FMoves);
      t := FBaseTeam;
      SetLength(t.Pokemon, Length(t.Pokemon));
      FillChar(t.Pokemon[btSize], SizeOf(TPokemon) * TeamSize - btSize, #00);
      SetLength(t.Strength, Length(t.Strength));
      SetLength(t.Weakness, Length(t.Weakness));
      GenTeam(t, TeamSize - btSize, localPool, lm);
      TeamGenerated(t);
    end;
  finally
    lm.Free;
    localPool.Free;
  end;
end;

end.
