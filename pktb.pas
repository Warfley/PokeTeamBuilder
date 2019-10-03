unit pktb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, PkTBTypes, listrecords, Math, Sampler;

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

  TPokemonSampler = specialize TWeightedSampler<TPokemon>;

  { TPKTB }

  TPKTB = class
  private
    FRequiredPokemon: TPokemonList;   
    FStrengthTable: TStrengthTable;
    FBannedPokemon: TPokemonList;
    FRequiredMoves: TMoveList;
    FTeamSize: Integer;
    FSampledTeam: TTeam;
    FStillRequiredMoves: TMoveList;

    FSampler: TPokemonSampler;

    function ComputeWeight(const Item: TPokemon): extended;
    procedure NewTeam(const Size: Integer);
    procedure AddToTeam(const pkmn: TPokemon);
    procedure GetStrengthAndWeaknesses(const pkmn: TPokemon; var Strength: TDynIntArray; var Weakness: TDynIntArray);

    function GetPokePool: TPokemonList;
    procedure SetBannedPokemon(AValue: TPokemonList);
    procedure SetPokePool(AValue: TPokemonList);
    procedure SetRequiredMoves(AValue: TMoveList);
    procedure SetRequiredPokemon(AValue: TPokemonList);
    procedure SetStrengthTable(AValue: TStrengthTable);
  public
    constructor Create;
    destructor Destroy; override;

    function SampleTeam(const Size: Integer): TTeam;

    property RequiredPokemon: TPokemonList read FRequiredPokemon write SetRequiredPokemon;
    property RequiredMoves: TMoveList read FRequiredMoves write SetRequiredMoves;
    property BannedPokemon: TPokemonList read FBannedPokemon write SetBannedPokemon;
    property StrengthTable: TStrengthTable read FStrengthTable write SetStrengthTable;
    property PokePool: TPokemonList read GetPokePool write SetPokePool;
  end;


implementation

{ TPKTB }

procedure TPKTB.NewTeam(const Size: Integer);
var
  i: Integer;
begin
  SetLength(FSampledTeam.Pokemon, Size);
  SetLength(FSampledTeam.Weakness, FStrengthTable.Count);
  SetLength(FSampledTeam.Strength, FStrengthTable.Count);
  for i:=0 to FStrengthTable.Count-1 do
  begin
    FSampledTeam.Weakness[i].Factor:=0;
    FSampledTeam.Weakness[i].Name:=FStrengthTable[i].Name;
    FSampledTeam.Weakness[i].TID:=FStrengthTable[i].ID;
    FSampledTeam.Strength[i].Factor:=0;
    FSampledTeam.Strength[i].Name:=FStrengthTable[i].Name;
    FSampledTeam.Strength[i].TID:=FStrengthTable[i].ID;
  end;
  FSampledTeam.CurrentSize := 0;
end;

function TPKTB.ComputeWeight(const Item: TPokemon): extended;
var
  w, s: TDynIntArray;
  SDistance: Extended;
  WDistance: Extended;
  i: Integer;
  m, m2: TMove;
  moveFound: Boolean;
begin       
  Result:=0;
  // Dont sample twice
  for i:=0 to FSampledTeam.CurrentSize-1 do
    if FSampledTeam.Pokemon[i].ID = Item.ID then Exit;
                                               
  // if there are still required moves left
  // this pokemon needs to know at least one of them
  // to get any weight
  moveFound := FStillRequiredMoves.Count = 0;
  for m in FStillRequiredMoves do
    for m2 in Item.Moves do
      if m.AttackID = m2.AttackID then
        moveFound:=True;
  if not moveFound then
    Exit;

  Result:=1;

  if FSampledTeam.CurrentSize = 0 then
  begin
    // For the first pokemon we want equal chances
    Exit;
  end;

  SetLength(SDistances, FStrengthTable.Count);
  SetLength(WDistances, FStrengthTable.Count);
  GetStrengthAndWeaknesses(Item, s, w);
  for i:=0 to FStrengthTable.Count-1 do
  begin
    SDistance := s[i] / (FSampledTeam.Strength[i] / FSampledTeam.CurrentSize);
    WDistance := (FSampledTeam.Strength[i] / FSampledTeam.CurrentSize) / w[i];
    Result := Result * SDistance * WDistance;
  end;
  Result := Max(Result, 0);
end;

procedure TPKTB.AddToTeam(const pkmn: TPokemon);
var
  w, s: TDynIntArray;
  i: Integer;
begin
  FSampledTeam.Pokemon[FSampledTeam.CurrentSize] := pkmn;
  GetStrengthAndWeaknesses(pkmn, s, w);
  for i:=0 to FStrengthTable.Count-1 do
  begin
    FSampledTeam.Weakness[i].Factor += w[i];
    FSampledTeam.Strength[i].Factor += s[i];
  end;
  FSampledTeam.CurrentSize:=FSampledTeam.CurrentSize + 1;
end;

procedure TPKTB.GetStrengthAndWeaknesses(const pkmn: TPokemon;
  var Strength: TDynIntArray; var Weakness: TDynIntArray);
var
  f: Extended;
  i, j: Integer;
begin
  SetLength(Strength, FStrengthTable.Count);
  SetLength(Weakness, FStrengthTable.Count);
  // initial weakness 100, multiplied for each
  FillDWord(Weakness, SizeOf(Integer) * FStrengthTable.Count, 100);
  for i:=0 to FStrengthTable.Count-1 do
  begin
    for j:=0 to FStrengthTable.Count-1 do
    begin
      // compute strengths
      if (FStrengthTable[i].ID = pkmn.Type1) Or (FStrengthTable[i].ID = pkmn.Type2) then
      begin
        // we are i, strong against j
        Strength[j] += FStrengthTable[i].Factors[j].Factor;
      end;
      // compute weakness
      if (FStrengthTable[i].Factors[j].TID = pkmn.Type1) or (FStrengthTable[i].Factors[j].TID = pkmn.Type2) then
      begin
        // we are j, weak against i
        // if effective this is 2, if weak this is 1/2
        f := FStrengthTable[i].Factors[j].Factor / 100;
        Weakness[i] := trunc(Weakness[i] * f);
      end;
    end;
  end;
end;

function TPKTB.GetPokePool: TPokemonList;
begin
  Result := FSampler.SampleableItems;
end;

procedure TPKTB.SetBannedPokemon(AValue: TPokemonList);
begin
  if FBannedPokemon=AValue then Exit;
  FBannedPokemon.Assign(AValue);
end;

procedure TPKTB.SetPokePool(AValue: TPokemonList);
begin
  FSampler.SampleableItems:=AValue;
end;

procedure TPKTB.SetRequiredMoves(AValue: TMoveList);
begin
  if FRequiredMoves=AValue then Exit;
  FRequiredMoves.Assign(AValue);
end;

procedure TPKTB.SetRequiredPokemon(AValue: TPokemonList);
begin
  if FRequiredPokemon=AValue then Exit;
  FRequiredPokemon.Assign(AValue);
end;

procedure TPKTB.SetStrengthTable(AValue: TStrengthTable);
begin
  if FStrengthTable=AValue then Exit;
  FStrengthTable.Assign(AValue);
end;

constructor TPKTB.Create;
begin
  FStrengthTable := TStrengthTable.Create;
  FRequiredMoves := TMoveList.Create;
  FRequiredPokemon := TPokemonList.Create;
  FBannedPokemon := TPokemonList.Create;
  FStillRequiredMoves := TMoveList.Create;
  FSampler:=TPokemonSampler.Create;
  FSampler.addWeighter(@ComputeWeight);
end;

destructor TPKTB.Destroy;
begin
  FRequiredMoves.Free;
  FRequiredPokemon.Free;
  FBannedPokemon.Free;
  FStillRequiredMoves.Free;
  FSampler.Free;
  inherited Destroy;
end;

function TPKTB.SampleTeam(const Size: Integer): TTeam;
var
  p: TPokemon;
begin
  FSampledTeam := newTeam(Size);
  for p in RequiredPokemon do
    AddToTeam(p);
  FStillRequiredMoves.Assign(FRequiredMoves);
  for i:=FSampledTeam.CurrentSize to Size-1 do
  begin
    FSampler.refresh;
    p := FSampler.Sample;
    AddToTeam(p);
  end;
  Result:=FSampledTeam;
end;

end.
