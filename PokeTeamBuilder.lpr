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
  listrecords,
  pktb;

type

  { TPokeTeamBuilder }

  TPokeTeamBuilder = class(TCustomApplication)
  private
    function SelectGenerationSTD(const Generations: TGenerationList): integer;
    function selectISTD(const Pokemon: TPokemonList): TDynIntArray;
    function SelectLanguageSTD(const Languages: TLanguageList): integer;
    function SelectMovesSTD(const Moves: TMoveList): TDynIntArray;
    function SelectOKSTD: TObtainableKinds;
    function SelectPSTD(const Pokemon: TPokemonList): TDynIntArray;
    procedure StartGraphicalCLI(DB: TDBConnection; TeamBuilder: TPKTB);
    procedure StartStdCLI(DB: TDBConnection; TeamBuilder: TPKTB);
    procedure TeamGenerated(const Team: TTeam);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TPokeTeamBuilder }

  function TPokeTeamBuilder.SelectGenerationSTD(
  const Generations: TGenerationList): integer;
  var
    i: integer;
  begin
    WriteLn('Select generation');
    for i := 0 to Generations.Count - 1 do
      WriteLn(Format('[%d] %s', [i, Generations[i].Name]));
    repeat
      try
        ReadLn(Result);
        if (Result < 0) or (Result >= Generations.Count) then
          WriteLn('unknown index');
      except
        on E: EInOutError do
          WriteLn('No number, try again');
      end;
    until (Result > 0) and (Result < Generations.Count);
  end;

  function TPokeTeamBuilder.SelectISTD(const Pokemon: TPokemonList): TDynIntArray;
  var
    nme: string;
    i: Integer;
  begin
    while True do
    begin
      WriteLn('Select Pokemon to ignore (empty if none)');
      ReadLn(nme);
      if nme.Length = 0 then Exit;
      i:=Pokemon.FindPokemon(nme);
      if i>0 then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result)-1] := i;
      end
      else
        WriteLn('Invalid name')
      end;
  end;

  function TPokeTeamBuilder.SelectLanguageSTD(const Languages: TLanguageList): integer;
  var
    i: integer;
  begin
    WriteLn('Select language');
    for i := 0 to Languages.Count - 1 do
      WriteLn(Format('[%d] %s', [i, Languages[i].Name]));
    repeat
      try
        ReadLn(Result);
        if (Result < 0) or (Result >= Languages.Count) then
          WriteLn('unknown index');
      except
        on E: EInOutError do
          WriteLn('No number, try again');
      end;
    until (Result > 0) and (Result < Languages.Count);
  end;

  function TPokeTeamBuilder.SelectMovesSTD(const Moves: TMoveList): TDynIntArray;
  var
    nme: string;
    i: Integer;
  begin
    repeat
      WriteLn('Select moves your team must be able to do (empty if none)');
      ReadLn(nme);
      if nme.Length = 0 then Exit;
      i:=Moves.FindMove(nme);
      if i>0 then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result)-1] := i;
      end
      else
        WriteLn('Invalid name')
    until False;
  end;

  function TPokeTeamBuilder.SelectOKSTD: TObtainableKinds;
  function GetOk(str: String; out o: TObtainableKind): Boolean;
begin
  Result:=True;
  if (str = 'c') then o:=okCatchable
  else if (str = 's') then o:=okSwarm
  else if (str = 'd') then o:=okDualSlot
  else if (str = 'r') then o:=okRecieved
  else if (str = 'e') then o:=okEvolves
  else if (str = 'b') then o:=okBackEvolve
  else if (str = 'ev') then o:=okEvent
  else Result:=False;
end;
  var str, s: String;
    sl: TStringList;
    o: TObtainableKind;
  begin
    Result:=[];
    WriteLn('Enter obtainable kinds comma seperated [c, s, d, r, e, b, ev]');
    ReadLn(str);
    sl:=TStringList.Create;
    try
    sl.Delimiter:=',';
    sl.StrictDelimiter:=True;
    sl.DelimitedText:=str;
    for s in sl do
      if GetOk(s.Trim,o) then
        Result := Result + [o]
      else
        WriteLn('Unknown symbol ', s,' ignored');
    finally
      sl.Free;
    end;
  end;

  function TPokeTeamBuilder.SelectPSTD(const Pokemon: TPokemonList): TDynIntArray;
  var
    nme: string;
    i: Integer;
  begin
    while not False do
    begin
      WriteLn('Select Pokemon to be in the team (empty if none)');
      ReadLn(nme);
      if nme.Length = 0 then Exit;
      i:=Pokemon.FindPokemon(nme);
      if i>0 then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result)-1] := i;
      end
      else
        WriteLn('Invalid name')
      end;
  end;

  procedure TPokeTeamBuilder.StartStdCLI(DB: TDBConnection; TeamBuilder: TPKTB);
  var ts: Integer = -1;
    tc: Integer=-1;
  begin
    WriteLn('Select teamsize');
    repeat
      try
        ReadLn(ts);
        if (ts < 0) then
          WriteLn('invalid size');
      except
        on E: EInOutError do
          WriteLn('No number, try again');
      end;
    until (ts > 0);
    WriteLn('Select count');
    repeat
      try
        ReadLn(tc);
        if (tc < 0) then
          WriteLn('invalid count');
      except
        on E: EInOutError do
          WriteLn('No number, try again');
      end;
    until (tc > 0);
    TeamBuilder.GenerateTeams(ts, tc, @SelectLanguageSTD, @SelectGenerationSTD,
      @SelectMovesSTD, @SelectOKSTD, @SelectPSTD,
      @selectISTD, @TeamGenerated);
  end;

  procedure TPokeTeamBuilder.StartGraphicalCLI(DB: TDBConnection; TeamBuilder: TPKTB);
  begin
    StartStdCLI(DB, TeamBuilder);
  end;

  procedure TPokeTeamBuilder.TeamGenerated(const Team: TTeam);
  var
    i: Integer;
  begin
    WriteLn('----------------------------------------------------------');
    WriteLn('Team generated:');
    for i:=0 to Length(Team.Pokemon)-2 do
      Write(Team.Pokemon[i].Name, ', ');
    WriteLn(Team.Pokemon[Length(Team.Pokemon)-1].Name);
    for i:=0 to length(Team.Strength)-1 do
      WriteLn(Team.Strength[i].Name,': ', Team.Strength[i].Factor);
  end;

  procedure TPokeTeamBuilder.DoRun;
  var
    ErrorMsg: string;
    DB: TDBConnection;
    pktb: TPKTB;
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
    pktb := TPKTB.Create(DB);
    try
      if not longbool(ParamCount) then
       {$IfDef UNIX}
        if longbool(IsATTY(StdInputHandle)) then
          StartGraphicalCLI(DB, pktb)
        else
       {$EndIf}
          StartStdCLI(DB, pktb);
    finally
      pktb.Free;
      DB.Free;
    end;
    // stop program loop
    Terminate;
    {$IfDef Windows}
    ReadLn;
    {$EndIf}
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
