unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ComCtrls, laz2_DOM, laz2_XMLRead;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    OpenDialog2: TOpenDialog;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
type
  TObtainableKind = (okNone=0, okCatchable, okSwarm, okDualSlot, okRecieved, okEvolves, okBackEvolve, okEvent);

function GetOk(str: String): TObtainableKind;
begin
  if (str = 'c') then Result:=okCatchable
  else if (str = 's') then Result:=okSwarm
  else if (str = 'd') then Result:=okDualSlot
  else if (str = 'r') then Result:=okRecieved
  else if (str = 'e') then Result:=okEvolves
  else if (str = 'b') then Result:=okBackEvolve
  else if (str = 'ev') then Result:=okEvent
  else Result:=okNone;
end;

function OkToString(ok: TObtainableKind): String;
begin
  case ok of
    okCatchable: Result:='Catchable';
    okSwarm: Result:='Swarm';
    okDualSlot: Result:='DualSlot';
    okRecieved: Result:='Recieve';
    okEvolves: Result:='Evolves';
    okBackEvolve: Result:='Backevolution';
    okEvent: Result:='Event';
  end;
end;

var
  doc: TXMLDocument;
  table: TDOMNode;
  PokeNode: TDOMNode;
  content: String;
  ok: TObtainableKind;
  currTable: Integer = 0;
  id, vid, i, k, j: Integer;
const
  VersionCount = 29;
  versionNumbers: array[0..VersionCount-1] of Integer = (1,2,0,3,4,5,6,7,8,10,11,9,19,20,12,13,14,15,16,0,17,18,21,22,0,23,24,25,26);
  tableStarts: array[0..6] of Integer = (0,4,7,14,20,25,30);
begin
  if not OpenDialog2.Execute then exit;
  if not SQLite3Connection1.Connected then
     SQLite3Connection1.Open;
  try
    ReadXMLFile(doc, OpenDialog2.FileName);
    ProgressBar2.Max:=doc.DocumentElement.ChildNodes.Count-1;
    for i:=0 to doc.DocumentElement.ChildNodes.Count-1 do
    begin
      table:=doc.DocumentElement.ChildNodes[i];
      ProgressBar1.Max:=table.ChildNodes.Count-1;
      ProgressBar1.Position:=0;
      for k:=0 to table.ChildNodes.Count-1 do
      begin
        PokeNode:=table.ChildNodes[k];
        id:=PokeNode.ChildNodes[0].TextContent.Trim.ToInteger;
        for j:=0 to VersionCount-1-tableStarts[currTable] do
        begin
          content:=PokeNode.ChildNodes[j+2].TextContent.Trim;
          ok:=GetOk(content.ToLower);
          if not Boolean(ok) then Continue;
          vid:=versionNumbers[j+tableStarts[currTable]];
          if not LongBool(vid) then Continue;
          content:=OkToString(ok);
          SQLQuery1.SQL.Text:='insert into `obtainable` (pokemon_id, '+
                              'version_id, kind) values (:pid, :vid, :kind)';
          SQLQuery1.ParamByName('pid').AsInteger:=id;
          SQLQuery1.ParamByName('vid').AsInteger:=vid;
          SQLQuery1.ParamByName('kind').AsString:=content;
          SQLQuery1.ExecSQL;
        end;
        ProgressBar1.Position:=ProgressBar1.Position+1;
      end;     
    inc(currTable);
    ProgressBar2.Position:=currTable;
    end;
  finally
    doc.Free;
    SQLTransaction1.Commit;   
    SQLite3Connection1.Close();
  end;
end;

end.

