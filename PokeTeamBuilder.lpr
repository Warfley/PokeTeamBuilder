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
  pktb,
  TFTypes,
  TFCanvas,
  TFControls;

type

  { TPokeTeamBuilder }

  TPokeTeamBuilder = class(TCustomApplication)
  private
    FCanvas: TTextCanvas;
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
  var ws: TWindowSize;
    TabPos: Integer=0;
    c: Char;
    m: TPoint;
    ts : String = '6';
    tc: String = '10';
    tsLabel, tcLabel: TTFLabel;
  const
    GenTeams= 'Generate teams';
  begin
    FCanvas:=TTextCanvas.Create;
    try
       ws:=GetWindowSize;
       FCanvas.Resize(ws.Width-1, ws.Height-1);
       FCanvas.Clear;
       m := Point(FCanvas.Width div 2, FCanvas.Height div 2);
       tsLabel:=TTFLabel.Create(FCanvas);
       tcLabel:=TTFLabel.Create(FCanvas);
       try
         tsLabel.Text.Text:='Select team size';
         tsLabel.Top:=m.y-5;
         tsLabel.Left:=m.x-tsLabel.Width div 2;   
         tcLabel.Text.Text:='Select team count';
         tcLabel.Top:=m.y-2;
         tcLabel.Left:=m.x-tcLabel.Width div 2;
       repeat
         // Draw Textboxes 
       //team size
         if TabPos = 0 then
            FCanvas.SetColor(RGB(255,255,100),RGB(0,0,0))
         else
            FCanvas.SetColor(RGB(255,255,255),RGB(0,0,0));
         FCanvas.Rectangle(m.x-5, m.y-4,11,1);
         FCanvas.SetColor(RGB(20,20,20),Transparency);
         FCanvas.TextOut(m.x+5-ts.Length, m.y-4, ts);
         //team count
         if TabPos = 1 then
            FCanvas.SetColor(RGB(255,255,0),RGB(0,0,0))
         else
            FCanvas.SetColor(RGB(255,255,255),RGB(0,0,0));
         FCanvas.Rectangle(m.x-5, m.y-1,11,1);
         FCanvas.SetColor(RGB(20,20,20),Transparency);
         FCanvas.TextOut(m.x+5-tc.Length, m.y-1, tc);

         // draw Button 
         if TabPos = 2 then
            FCanvas.SetColor(RGB(0,0,255),RGB(0,0,255))
         else
            FCanvas.SetColor(RGB(255,255,255),RGB(255,255,255));
         FCanvas.Rectangle(m.x-10, m.y+1, 21,3); 
         FCanvas.SetColor(RGB(20,20,20),Transparency);
         FCanvas.TextOut(m.x-(GenTeams.Length div 2), m.y+2, GenTeams);

         tcLabel.Redraw;
         tsLabel.Redraw;
         FCanvas.Print();
         c:=ReadChar();
         case c of
         #8: if TabPos = 0 then
               ts:=ts.Substring(1)
             else if TabPos = 1 then
               tc:=tc.Substring(1);
         #9: TabPos:=(TabPos+1) mod 3;
         '0'..'9': if TabPos = 0 then
               ts:=c+ts
             else if TabPos = 1 then
               tc:=c+tc;
         #13: if TabPos = 2 then Break;
         #27: Exit;
         end;
       until False;
       finally
         tsLabel.Free;
         tcLabel.Free;
       end;
    finally
      FCanvas.Free;
    end;

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
        if not longbool(IsATTY(StdInputHandle)) then
          StartStdCLI(DB, pktb)
        else
       {$EndIf}
          StartGraphicalCLI(DB, pktb);
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
