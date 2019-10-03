unit Sampler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gvector;

type
    ENoSamplesException = class(Exception);

  { WeightedSampler }

  generic WeightedSampler<T> = class
  private
    type TWeightEntry = record
      Value: T;
      Weight: extended;
      OffSet: extended;
    end;
    TWeightList = specialize TVector<TWeightEntry>;
    TSampleList = specialize TVector<T>;
    TGetWeightMethod =
    function(const Item: T): extended of object;
    TGetWeightFunction =
    function(const Item: T): extended;
    TWeightMethodList = specialize TVector<TGetWeightMethod>;
    TWeightFunctionList = specialize TVector<TGetWeightFunction>;
  private
    FWeights: TWeightList;
    FSampleableItems: TSampleList;
    FTotalWeight: extended;
    FWeighterMethods: TWeightMethodList;
    FWeighterFunctions: TWeightFunctionList;
    FUpdate: boolean;

    procedure ConstructWeightList;

    function LookUpEntry(const Value: extended): T;

  public
    constructor Create;
    destructor Destroy; override;

    procedure addObject(const Item: T);
    procedure addWeighter(const Weighter: TGetWeightMethod);
    procedure addWighter(const Weighter: TGetWeightFunction);
    procedure ClearItems;
    procedure ClearWeighter;
    procedure refresh;

    function Sample: T;
  end;

implementation

{ WeightedSampler }

procedure WeightedSampler.ConstructWeightList;
var
  itm: T;
  w1: TGetWeightFunction;
  w2: TGetWeightMethod;
  weight: extended;
  tmp: TWeightEntry;
begin
  FWeights.Clear;
  FTotalWeight := 0;
  for itm in FSampleableItems do
  begin
    weight := 1.0;
    for w1 in FWeighterFunctions do
      weight *= w1(itm);
    for w2 in FWeighterMethods do
      weight *= w2(itm);

    if weight = 0 then
      continue;

    FTotalWeight += weight;

    if FWeights.IsEmpty then
      tmp.OffSet := 0
    else
      tmp.OffSet := FWeights.Back.OffSet + FWeights.Back.Weight;
    tmp.Weight := weight;
    tmp.Value := itm;
  end;
end;

function WeightedSampler.LookUpEntry(const Value: extended): T;
var
  currMin, currMax, idx: integer;
  tmp: TWeightEntry;
begin
  // binary search
  currMin := 0;
  currMax := integer(FWeights.Size) - 1;
  while currMin <= currMax do
  begin
    idx := (currMax - currMin) div 2 + currMin;
    tmp := FWeights[idx];
    if (Value >= tmp.OffSet) and (Value < tmp.OffSet + tmp.Weight) then
    begin
      Result := tmp.Value;
      break;
    end
    else if Value < tmp.OffSet then
    begin
      currMax := idx - 1;
    end
    else
    begin
      currMin := idx + 1;
    end;

  end;
end;

constructor WeightedSampler.Create;
begin
  FWeights := TWeightList.Create;
  FSampleableItems := TSampleList.Create;
  FTotalWeight := 0;
  FWeighterMethods := TWeightMethodList.Create;
  FWeighterFunctions := TWeightFunctionList.Create;
  FUpdate := False;
end;

destructor WeightedSampler.Destroy;
begin
  FWeights.Free;
  FSampleableItems.Free;
  FWeighterMethods.Free;
  FWeighterFunctions.Free;
  inherited Destroy;
end;

procedure WeightedSampler.addObject(const Item: T);
begin
  FSampleableItems.PushBack(Item);
  FUpdate := True;
end;

procedure WeightedSampler.addWeighter(const Weighter: TGetWeightMethod);
begin
  FWeighterMethods.PushBack(Weighter);
  FUpdate := True;
end;

procedure WeightedSampler.addWighter(const Weighter: TGetWeightFunction);
begin
  FWeighterFunctions.PushBack(Weighter);
  FUpdate := True;
end;

procedure WeightedSampler.ClearItems;
begin
  FSampleableItems.Clear;
  FUpdate := True;
end;

procedure WeightedSampler.ClearWeighter;
begin
  FWeighterFunctions.Clear;
  FWeighterMethods.Create;
  FUpdate := True;
end;

procedure WeightedSampler.refresh;
begin
  FUpdate := True;
end;

function WeightedSampler.Sample: T;
var
  val: Extended;
begin
  if FUpdate then
  begin
    ConstructWeightList;
    FUpdate:=False;
  end;
  if FWeights.IsEmpty then raise ENoSamplesException.Create('No items to sample');

  val := Random * FTotalWeight;
  Result := LookUpEntry(val);
end;

end.
