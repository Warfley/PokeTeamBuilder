program PokeTeamBuilder;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX}
  cthreads,
  termio, {$ENDIF}
  Classes,
  LazUTF8,
  SysUtils,
  CustApp,
  PkTBTypes,
  sqlite3laz,
  Interfaces,
  listrecords;

type

  { TPokeTeamBuilder }

  TPokeTeamBuilder = class(TCustomApplication)
  private
    procedure StartGraphicalCLI(DB: TDBConnection);
    procedure StartStdCLI(DB: TDBConnection);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TPokeTeamBuilder }

  procedure TPokeTeamBuilder.StartGraphicalCLI(DB: TDBConnection);
  begin

  end;

  procedure TPokeTeamBuilder.StartStdCLI(DB: TDBConnection);
  var
    ll: TLanguageList;
    gl: TGenerationList;
    vl: TMoveList;
    i: integer;
    LanguageID: integer = -1;
    Edition: TEdition;
  begin
    ll := TLanguageList.Create;
    try
      ll.LoadLanguages(DB);
      WriteLn('Select language');
      for i := 0 to ll.Count - 1 do
        WriteLn(Format('[%d] %s', [i, ll[i].Name]));
      repeat
        try
          ReadLn(LanguageID);
          if (LanguageID < 0) or (LanguageID >= ll.Count) then
            WriteLn('unknown index');
        except
          on E: EInOutError do
            WriteLn('No number, try again');
        end;
      until (LanguageID > 0) and (LanguageID < ll.Count);
      LanguageID:=ll[LanguageID].ID;
    finally
      ll.Free;
    end;
    gl := TGenerationList.Create;
    try
      gl.LoadGenerations(DB, LanguageID);
      Edition.ID:=-1;
      WriteLn('Select generation');
      for i := 0 to gl.Count - 1 do
        WriteLn(Format('[%d] %s', [i, gl[i].Name]));
      repeat
        try
          ReadLn(Edition.ID);
          if (Edition.ID < 0) or (Edition.ID >= gl.Count) then
            WriteLn('unknown index');
        except
          on E: EInOutError do
            WriteLn('No number, try again');
        end;
      until (Edition.ID > 0) and (Edition.ID < gl.Count);
      Edition:=gl[Edition.ID];
    finally
      gl.Free;
    end;
    vl:=TMoveList.Create;
    try
      vl.LoadMoves(DB, LanguageID, Edition.GenerationID);
      for i:=0 to vl.Count-1 do
        writeln(vl[i].AttackName);
    finally
      vl.Free;
    end;
  end;

  procedure TPokeTeamBuilder.DoRun;
  var
    ErrorMsg: string;
    DB: TDBConnection;
  begin
    // quick check parameters
    ErrorMsg := CheckOptions('h', 'help');
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;
    DB := TDBConnection.Create('./veekun-pokedex.sqlite');
    try
      if not longbool(ParamCount) then
       {$IfDef UNIX}
        if longbool(IsATTY(StdInputHandle)) then
          StartGraphicalCLI(DB)
        else
       {$EndIf}
          StartStdCLI(DB);
    finally
      DB.Free;
    end;
    // stop program loop
    Terminate;
    ReadLn;
  end;

  constructor TPokeTeamBuilder.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TPokeTeamBuilder.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TPokeTeamBuilder.WriteHelp;
  begin
    { add your help code here }
    writeln('Usage: ', ExeName, ' -h');
  end;

var
  Application: TPokeTeamBuilder;
begin
  Application := TPokeTeamBuilder.Create(nil);
  Application.Title := 'Poke Team Builder';
  Application.Run;
  Application.Free;
end.
