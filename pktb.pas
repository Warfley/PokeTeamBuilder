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

    procedure GetTypeValues(tid: integer; var SC, WC: array of integer);
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
  FactorDivisor = 20;
  Shuffle = 20;
  SingleTypeBonus = 30;


implementation


procedure TPKTB.GetTypeValues(tid: integer; var SC, WC: array of integer);
var
  i, j: integer;
begin
  if FStrengthTable.FindType(tid) < 0 then
    tid := 1;
  for i := 0 to FStrengthTable.Count - 1 do
    if FStrengthTable[i].ID = tid then
    begin
      for j := 0 to Length(FStrengthTable[i].Factors) - 1 do
        if SC[j] < FStrengthTable[i].Factors[j].Factor then
        begin
          SC[j] := FStrengthTable[i].Factors[j].Factor;
        end;
    end
    else
    begin
      for j := 0 to Length(FStrengthTable[i].Factors) - 1 do
        if (FStrengthTable[i].Factors[j].TID = tid) and
          ((WC[i] = 100) or (WC[i] < 0) or ((WC[i] > 100) and
          (FStrengthTable[i].Factors[j].Factor < 100)) or (WC[i] < 100) and
          (FStrengthTable[i].Factors[j].Factor < WC[i])) then
        begin
          WC[i] := FStrengthTable[i].Factors[j].Factor;
        end;
    end;

end;

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
var
  Weight: array of integer;
  pkmn: TPokemon;
  w, j, i: integer;
  SAvg, WAvg: int64;
  sc, wc: array of integer;
begin
  CalcStrength(Team);
  if Count = 0 then
    exit;
  // Strength and Weaknessfactors as metric
  Savg := 0;
  Wavg := 0;
  for i := 0 to Length(Team.Strength) - 1 do
  begin
    Inc(Savg, Team.Strength[i].Factor);
    Inc(Wavg, Team.Weakness[i].Factor);
  end;
  Savg := Savg div Length(Team.Strength);
  WAvg := Wavg div Length(Team.Weakness);
  SetLength(SC, FStrengthTable.Count);
  SetLength(WC, FStrengthTable.Count);
  // Weighting
  SetLength(Weight, Pool.Count);
  for i := 0 to Pool.Count - 1 do
  begin
    for j := 0 to FStrengthTable.Count - 1 do
    begin
      SC[j] := -1;
      WC[j] := -1;
    end;
    w := 0;
    pkmn := Pool[i];
    // Weakness, Strengths
    GetTypeValues(pkmn.Type1, SC, WC);
    if pkmn.Type2 > 0 then
      GetTypeValues(pkmn.Type2, SC, WC)
    else
      Inc(w, SingleTypeBonus*Count*FStrengthTable.Count div FactorDivisor);
    for j := 0 to FStrengthTable.Count - 1 do
    begin
      if (Team.Strength[j].Factor <= SAvg) and (sc[i]>=0) then
        Inc(w, sc[j] div FactorDivisor * FStrengthTable.Count);
      if (Team.Weakness[j].Factor >= WAvg) and (wc[i]>=0) then
        Dec(w, WC[j] div FactorDivisor);
    end;
    for j := 0 to Length(pkmn.Moves) - 1 do
    begin
      if pkmn.Moves[j].Available and (Moves.FindMove(pkmn.Moves[j].MID) >= 0) then
        if (pkmn.Type1 = Moves[j].TypeID) or (pkmn.Type2 = Moves[j].TypeID) then
          Inc(w, (MoveValue div Count) * 2)
        else
          Inc(w, MoveValue div Count);
    end;
    Weight[i] := w + Random(Count * Shuffle) * FactorDivisor;
  end;
  i := -1;
  j := integer.MinValue;
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
var
  i, j: integer;
  s, w: array of integer;
begin
  SetLength(s, Length(Team.Strength));
  SetLength(w, Length(Team.Weakness));
  for i := 0 to Length(Team.Strength) - 1 do
  begin
    Team.Strength[i].Factor := 0;
    Team.Weakness[i].Factor := 0;
  end;

  for i := 0 to Length(Team.Pokemon) - 1 do
  begin
    if Team.Pokemon[i].ID = 0 then
      Continue;

    for j := 0 to Length(s) - 1 do
    begin
      s[j] := -1;
      w[j] := -1;
    end;

    GetTypeValues(Team.Pokemon[i].Type1, s, w);
    if Team.Pokemon[i].Type2 > 0 then
      GetTypeValues(Team.Pokemon[i].Type2, s, w);

    for j := 0 to Length(s) - 1 do
    begin
      if s[j] < 0 then s[j] := 100;
      if w[j] < 0 then w[j] := 100;
      Team.Strength[j].Factor := Team.Strength[j].Factor + s[j];
      Team.Weakness[j].Factor := Team.Weakness[j].Factor + w[j];
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
