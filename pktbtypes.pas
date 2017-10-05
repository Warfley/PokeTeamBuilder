unit PkTBTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, listrecords, fgl, sqldb, sqlite3conn;

type    
  TDynIntArray = array of integer;

  { TDBConnection }

  TDBConnection = class
  private
    FConnection: TSQLite3Connection;
    FTransaction: TSQLTransaction;
    FQuery: TSQLQuery;
  public
    constructor Create(FileName: string);
    destructor Destroy; override;
    property Connection: TSQLite3Connection read FConnection;
    property Transaction: TSQLTransaction read FTransaction;
    property Query: TSQLQuery read FQuery;
  end;

  TLanguageList = specialize TFPGList<TLanguage>;

  { TLanguageListHelper }

  TLanguageListHelper = class helper for TLanguageList
    procedure LoadLanguages(DB: TDBConnection);
  end;

  TMoveList = specialize TFPGList<TMove>;

  { TMoveListHelper }

  TMoveListHelper = class helper for TMoveList
    procedure LoadMoves(DB: TDBConnection; LanguageID, Gen: integer);
    function FindMove(Name: String): Integer;
    function FindMove(ID: Integer): Integer;
  end;

  TGenerationList = specialize TFPGList<TEdition>;

  { TGenerationListHelper }

  TGenerationListHelper = class helper for TGenerationList
    procedure LoadGenerations(DB: TDBConnection; LanguageID: integer);
  end;

  TObtainableKinds = set of TObtainableKind;

  TPokemonList = specialize TFPGList<TPokemon>;

  { TPokemonListHelper }

  TPokemonListHelper = class helper for TPokemonList
    procedure LoadPokemons(DB: TDBConnection; LanguageID: Integer;
      Edition: TEdition; ObtainibleKinds: TObtainableKinds; Moves: TDynIntArray
  );
    function FindPokemon(Name: String): Integer;
  end;

  TStrengthTable = specialize TFPGList<TTypeStrength>;
   
  { TStrengthTableHelper }

  TStrengthTableHelper = class helper for TStrengthTable
    procedure LoadStrengthTable(DB: TDBConnection; LanguageID: Integer;
      GenerationID: Integer);
    function FindType(TypeID: Integer): Integer;
  end;



const
  FullKinds = [okCatchable, okSwarm, okDualSlot, okRecieved, okEvolves,
  okBackEvolve, okEvent];

implementation

{ TStrengthTableHelper }

procedure TStrengthTableHelper.LoadStrengthTable(DB: TDBConnection;
  LanguageID: Integer; GenerationID: Integer);

function FindFirstFree(ts: TTypeStrength): Integer;
begin
  for Result:=0 to length(ts.Factors)-1 do
    if ts.Factors[Result].TID = 0 then exit;
  Result:=-1;
end;

var c: Integer;
  st: TTypeStrength;
  i, j: Integer;
begin
  DB.Transaction.Active := True;
  DB.Query.SQL.Text:='select Count(*) as c from `types` where generation_id = :gid ' +
                      'and id < 1000';     
  DB.Query.ParamByName('gid').AsInteger := GenerationID;
  DB.Query.Open;
  try
    DB.Query.First;
    c:=DB.Query.FieldByName('c').AsInteger;
  finally
    DB.Query.Close;
  end;
  DB.Query.SQL.Text := 'select damage_type_id as aid, target_type_id as tid, '+
                       'name, damage_factor as df from `type_efficacy` join '+
                       '(select name, type_id from `type_names` where '+
                       'local_language_id = :lid) on type_id = damage_type_id ' +
                       'join `types` as t1 on t1.id = type_id and '+
                       't1.generation_id = :gid join `types` as t2 on '+
                       't2.id = target_type_id and t2.generation_id = :gid';
  DB.Query.ParamByName('lid').AsInteger := LanguageID;
  DB.Query.ParamByName('gid').AsInteger := GenerationID;
  DB.Query.Open;
  with DB.Query do
    try
      First;
      while not EOF do
      begin
        i:=FindType(FieldByName('aid').AsInteger);
        if i<0 then
        begin
          st.ID:=FieldByName('aid').AsInteger;  
          st.Name:=FieldByName('name').AsString; 
          SetLength(st.Factors, 0);
          SetLength(st.Factors, c);
          st.Factors[0].TID:=FieldByName('tid').AsInteger;
          st.Factors[0].Factor:=FieldByName('df').AsInteger;
          Add(st);
        end
        else
        begin
          j:=FindFirstFree(Items[i]);
          if j<0 then raise Exception.Create('This should never happen');
          Items[i].Factors[j].TID :=FieldByName('tid').AsInteger;
          Items[i].Factors[j].Factor:=FieldByName('df').AsInteger;
        end;
        Next;
      end;

    finally
      DB.Transaction.Commit;
    end;
end;

function TStrengthTableHelper.FindType(TypeID: Integer
  ): Integer;
begin
  For Result:=0 to self.Count-1 do
    if self[Result].ID=TypeID then
      Exit;
  Result:=-1;
end;

{ TPokemonListHelper }

procedure TPokemonListHelper.LoadPokemons(DB: TDBConnection;
  LanguageID: Integer; Edition: TEdition; ObtainibleKinds: TObtainableKinds;
  Moves: TDynIntArray); var
  p: TPokemon;
  i, AsInteger: Integer;

function BuildOKWhereString: String;
begin
  Result:='';
  if (ObtainibleKinds = FullKinds) or (ObtainibleKinds = []) then exit;;
  Result:='And (';
  if okCatchable in ObtainibleKinds then Result+='kind = "Catchable" or ';
  if okSwarm in ObtainibleKinds then Result+='kind = "Swarm" or ';
  if okDualSlot in ObtainibleKinds then Result+='kind = "DualSlot" or ';
  if okRecieved in ObtainibleKinds then Result+='kind = "Recieve" or ';
  if okEvolves in ObtainibleKinds then Result+='kind = "Evolves" or ';
  if okBackEvolve in ObtainibleKinds then Result+='kind = "Backevolution" or ';
  if okEvent in ObtainibleKinds then Result+='kind = "Event" or ';
  Result := Result.Substring(0, Result.Length - 3);
  Result+=')';
end;

function getOK(str: String): TObtainableKind;
begin
    if (str = 'Catchable') then Result:=okCatchable
  else if (str = 'Swarm') then Result:=okSwarm
  else if (str = 'DualSlot') then Result:=okDualSlot
  else if (str = 'Recieve') then Result:=okRecieved
  else if (str = 'Evolves') then Result:=okEvolves
  else if (str = 'Backevolution') then Result:=okBackEvolve
  else if (str = 'Event') then Result:=okEvent
end;

begin
  DB.Transaction.Active := True;
  DB.Query.SQL.Text := 'select * from '+
    '(select id, name, t1, t2, kind as obtain_kind from '+
    '(select Max(p.id) as id, kind from '+
    '(select * from `pokemon_species` as p'+
    ' join (select * from `obtainable` where version_id = :vid '+
    BuildOKWhereString + ') '+
    'on pokemon_id = p.id) as p group by p.evolution_chain_id) as p '+
    'join (select name, pokemon_species_id from '+
    '`pokemon_species_names` where local_language_id = :lid) as pn '+
    'on pn.pokemon_species_id = p.id join '+
    '(select type_id as t1, pokemon_id from `pokemon_types` where slot = 1) as t1 '+
    'on t1.pokemon_id = p.id left '+
    'join (select type_id as t2, pokemon_id from '+
    '`pokemon_types` where slot = 2) as t2 on t2.pokemon_id = p.id group by p.id) as p';
  for i:=0 to Length(Moves) - 1 do
    DB.Query.SQL.Add('left join (select 1 as m'+Moves[i].ToString+', move_id,'+
    ' pokemon_id from `pokemon_moves` where move_id = '+Moves[i].ToString+
    ' and version_group_id = :vgid) as mp'+Moves[i].ToString+' on mp'+Moves[i].ToString+'.pokemon_id = p.id');
  DB.Query.ParamByName('lid').AsInteger := LanguageID;
  DB.Query.ParamByName('vid').AsInteger := Edition.GenerationID;
  if Length(Moves)> 0 then
   DB.Query.ParamByName('vgid').AsInteger := Edition.GroupID;
  DB.Query.Open;
  with DB.Query do
    try
      First;
      while not EOF do
      begin
        p.Name:=FieldByName('name').AsString;
        p.ID:=FieldByName('id').AsInteger;
        p.ObtainableKind:=GetOK(FieldByName('obtain_kind').AsString);
        p.Type1:=FieldByName('t1').AsInteger;
        if FieldByName('t2').IsNull then
          p.Type2:=0
        else p.Type2:=FieldByName('t2').AsInteger;
        SetLength(p.Moves, Length(Moves));
        for i:=0 to Length(p.Moves)-1 do
        begin
          p.Moves[i].MID:=Moves[i];
          p.Moves[i].Available:=not FieldByName('m'+Moves[i].ToString).IsNull;
        end;
        Add(p);
        Next;
      end;

    finally
      DB.Transaction.Commit;
    end;
end;

function TPokemonListHelper.FindPokemon(Name: String): Integer;
begin
  for Result:=0 to Count-1 do
    if Items[Result].Name.ToLower = Name.ToLower then
      exit;
  Result:=-1;
end;

{ TGenerationListHelper }

procedure TGenerationListHelper.LoadGenerations(DB: TDBConnection; LanguageID: integer);
var
  cg: TEdition;
begin
  DB.Transaction.Active := True;
  DB.Query.SQL.Text := 'select v.id as id, vn.name as nme, vg.id as gid, '+
    'vg.generation_id as gen from ' +
    '`versions` as v join (select * from `version_names` ' +
    'as vn where vn.local_language_id = :lid) as vn on v.id ' +
    '= vn.version_id join `version_groups` as vg ' +
    'on v.version_group_id = vg.id';
  DB.Query.ParamByName('lid').AsInteger := LanguageID;
  DB.Query.Open;
  with DB.Query do
    try
      First;
      while not EOF do
      begin
        cg.ID := FieldByName('id').AsInteger;
        cg.GroupID := FieldByName('gid').AsInteger;
        cg.Name := FieldByName('nme').AsString;
        cg.GenerationID:=FieldByName('gen').AsInteger;
        Add(cg);
        Next;
      end;

    finally
      DB.Transaction.Commit;
    end;
end;

{ TMoveListHelper }

procedure TMoveListHelper.LoadMoves(DB: TDBConnection; LanguageID, Gen: integer);
var
  mv: TMove;
begin
  DB.Transaction.Active := True;
  DB.Query.SQL.Text :=
  'select m.id as id, mn.name as name, m.type_id as tid from (select * from `moves` where'+
  ' generation_id = :g) as m join (select * from `move_names` where '+
  'local_language_id = :lid) as mn where m.id = mn.move_id';

  DB.Query.ParamByName('g').AsInteger := Gen;
  DB.Query.ParamByName('lid').AsInteger := LanguageID;
  DB.Query.Open;
  with DB.Query do
    try
      First;
      while not EOF do
      begin
        mv.AttackName:=FieldByName('name').AsString;
        mv.AttackID:=FieldByName('id').AsInteger;
        mv.TypeID:=FieldByName('tid').AsInteger;
        Add(mv);
        Next;
      end;
    finally
      Close;
    end;
end;

function TMoveListHelper.FindMove(Name: String): Integer;
begin
  for Result:=0 to Count-1 do
    if Items[Result].AttackName.ToLower = Name.ToLower then
      exit;
  Result:=-1;
end;

function TMoveListHelper.FindMove(ID: Integer): Integer;
begin
  for Result:=0 to Count-1 do
    if Items[Result].AttackID = ID then
      exit;
  Result:=-1;
end;

{ TLanguageListHelper }

procedure TLanguageListHelper.LoadLanguages(DB: TDBConnection);
var
  ci: integer = -1;
  cl: TLanguage;
begin
  DB.Transaction.Active := True;
  DB.Query.SQL.Text := 'select l.id as i, l.identifier as id, ' +
    'ln.local_language_id as lli, ln.name as n from  ' +
    '`languages` as l join `language_names` as ln on ' +
    'l.id = ln.language_id order by l.id';
  DB.Query.Open;
  with DB.Query do
    try
      First;
      while not EOF do
      begin
        if (ci < 0) or (Self.Items[ci].ID <> FieldByName('i').AsInteger) then
        begin
          cl.ID := FieldByName('i').AsInteger;
          cl.Identefier := FieldByName('id').AsString;
          cl.Name := '';
          ci := Add(cl);
        end;
        if (cl.ID = FieldByName('lli').AsInteger) or
          ((cl.Name.Length = 0) and (FieldByName('lli').AsInteger = 9)) then
        begin
          cl.Name := FieldByName('n').AsString;
          Items[ci] := cl;
        end;
        Next;
      end;

    finally
      DB.Transaction.Commit;
    end;
end;

{ TDBConnection }

constructor TDBConnection.Create(FileName: string);
begin
  FConnection := TSQLite3Connection.Create(nil);
  FTransaction := TSQLTransaction.Create(nil);
  FQuery := TSQLQuery.Create(nil);
  Connection.DatabaseName := FileName;
  Connection.Transaction := FTransaction;
  Transaction.DataBase := FConnection;
  Query.Transaction := FTransaction;
  Query.DataBase := FConnection;
  Connection.Open;
end;

destructor TDBConnection.Destroy;
begin
  Query.Close;
  Query.Free;
  Transaction.Free;
  Connection.Close;
  Connection.Free;
  inherited Destroy;
end;

end.

