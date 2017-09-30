unit PkTBTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, listrecords, fgl, sqldb, sqlite3conn;

type

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

  TVMList = specialize TFPGList<TVM>;

  { TVMListHelper }

  TVMListHelper = class helper for TVMList
    procedure LoadVMs(DB: TDBConnection; LanguageID, GenerationGroup: integer);
  end;

  TGenerationList = specialize TFPGList<TGeneration>;

  { TGenerationListHelper }

  TGenerationListHelper = class helper for TGenerationList
    procedure LoadGenerations(DB: TDBConnection; LanguageID: integer);
  end;


implementation

{ TGenerationListHelper }

procedure TGenerationListHelper.LoadGenerations(DB: TDBConnection; LanguageID: integer);
var
  cg: TGeneration;
begin
  DB.Transaction.Active := True;
  DB.Query.SQL.Text := 'select v.id as id, vn.name as nme, vg.id as gid from ' +
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
        Add(cg);
        Next;
      end;

    finally
      DB.Transaction.Commit;
    end;
end;

{ TVMListHelper }

procedure TVMListHelper.LoadVMs(DB: TDBConnection; LanguageID, GenerationGroup: integer);
var
  vm: TVM;
begin
  DB.Transaction.Active := True;
  DB.Query.SQL.Text := 'select m.move_id as mid, i.item_id as iid, mn.name as ' +
    'mname, i.name as iname from (select * from `machines` as' +
    ' where m.version_group_id = :GenerationGroup) as m join (select * from '
    +
    '`move_names`as mn where mn.local_language_id = :LanguageID) as mn ' +
    'on m.move_id = mn.move_id join (select * from ' +
    '`item_names`as i where i.local_language_id = :LanguageID ) as i on' +
    ' i.item_id = m.item_id';

  DB.Query.ParamByName('GenerationGroup').AsInteger := GenerationGroup;
  DB.Query.ParamByName('LanguageID').AsInteger := LanguageID;
  DB.Query.Open;
  with DB.Query do
    try

    finally
      Close;
    end;
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

