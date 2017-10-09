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
  TFControls,
  TFBaseControls,
  StartUpForm,
  CheckListForm,
  SelectionForm,
  TFLists, ShowTeamForm;

type

  { TPokeTeamBuilder }

  TPokeTeamBuilder = class(TCustomApplication)
  private
    FCanvas: TTextCanvas;
    FPKTB: TPKTB;
    function SelectGenerationG(const Generations: TGenerationList): integer;
    function SelectGenerationSTD(const Generations: TGenerationList): integer;
    function selectIG(const Pokemon: TPokemonList): TDynIntArray;
    function selectISTD(const Pokemon: TPokemonList): TDynIntArray;
    function SelectLanguageG(const Languages: TLanguageList): integer;
    function SelectLanguageSTD(const Languages: TLanguageList): integer;
    function SelectMovesG(const Moves: TMoveList): TDynIntArray;
    function SelectMovesSTD(const Moves: TMoveList): TDynIntArray;
    function SelectOKG: TObtainableKinds;
    function SelectOKSTD: TObtainableKinds;
    function SelectPG(const Pokemon: TPokemonList): TDynIntArray;
    function SelectPSTD(const Pokemon: TPokemonList): TDynIntArray;
    procedure StartGraphicalCLI(DB: TDBConnection; TeamBuilder: TPKTB);
    procedure StartStdCLI(DB: TDBConnection; TeamBuilder: TPKTB);
    procedure TeamGenerated(const Team: TTeam);
    procedure TeamGeneratedG(const ATeam: TTeam);
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

function TPokeTeamBuilder.SelectGenerationG(const Generations: TGenerationList
  ): integer;
var i: Integer;
begin
  with TSelectionForm.Create(FCanvas) do
  try
    Caption:='Select edition';
    for i:=0 to Generations.Count-1 do
      ListBox.Items.Add(Generations[i].Name);
    Show;
    Result:=ListBox.ItemIndex;
  finally
    Free;
  end;
end;

function TPokeTeamBuilder.selectIG(const Pokemon: TPokemonList): TDynIntArray;
      var
  i, p: Integer;
begin
  with TCheckListForm.Create(FCanvas) do
  try
    Caption:='Select pokemon to ignore';
    for i:=0 to Pokemon.Count-1 do
      SelectionList.Items.Add(Pokemon[i].Name);
    Show;
    SetLength(Result, SelectionList.CheckCount);
    p:=0;
    for i:=0 to SelectionList.Items.Count-1 do
      if SelectionList.Checked[i] then
      begin
        Result[p]:=i;
        inc(p);
      end;
  finally
    Free;
  end;
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

function TPokeTeamBuilder.SelectLanguageG(const Languages: TLanguageList
  ): integer;
var
  i: Integer;
begin
  with TSelectionForm.Create(FCanvas) do
  try
    Caption:='Select game language';
    for i:=0 to Languages.Count-1 do
      ListBox.Items.Add(Languages[i].Name);
    Show;
    Result:=ListBox.ItemIndex;
  finally
    Free;
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

function TPokeTeamBuilder.SelectMovesG(const Moves: TMoveList): TDynIntArray;
var
  i, p: Integer;
begin
  with TCheckListForm.Create(FCanvas) do
  try
    Caption:='Select moves to be in your team';
    for i:=0 to Moves.Count-1 do
      SelectionList.Items.Add(Moves[i].AttackName);
    Show;
    p:=0;
    SetLength(Result, SelectionList.CheckCount);
    for i:=0 to SelectionList.Items.Count-1 do
      if SelectionList.Checked[i] then
      begin
        Result[p]:=i;
        inc(p);
      end;
  finally
    Free;
  end;
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

function TPokeTeamBuilder.SelectOKG: TObtainableKinds;
begin
  with TCheckListForm.Create(FCanvas) do
  try
    Caption:='Select obtainable methods';
      SelectionList.Items.Add('Catchable');
      SelectionList.Items.Add('Catchable during swarm');
      SelectionList.Items.Add('Obtainable with DualSlot'); 
      SelectionList.Items.Add('Recievable (e.g. Starter)');
      SelectionList.Items.Add('Evolves');
      SelectionList.Items.Add('Backevolution obtainable');
      SelectionList.Items.Add('Event');
      SelectionList.Checked[0]:=True; 
      SelectionList.Checked[3]:=True;
      SelectionList.Checked[4]:=True; 
      SelectionList.Checked[5]:=True;
    Show;
    Result:=[];
    if SelectionList.Checked[0] then
      Include(Result,okCatchable);
    if SelectionList.Checked[1] then
      Include(Result,okSwarm);
    if SelectionList.Checked[2] then
      Include(Result,okDualSlot);
    if SelectionList.Checked[3] then
      Include(Result,okRecieved);
    if SelectionList.Checked[4] then
      Include(Result,okEvolves);
    if SelectionList.Checked[5] then
      Include(Result,okBackEvolve);
    if SelectionList.Checked[6] then
      Include(Result,okEvent);
  finally
    Free;
  end;
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

function TPokeTeamBuilder.SelectPG(const Pokemon: TPokemonList): TDynIntArray;
var
  i, p: Integer;
begin
  with TCheckListForm.Create(FCanvas) do
  try
    Caption:='Select pokemon to choose from';
    for i:=0 to Pokemon.Count-1 do
      SelectionList.Items.Add(Pokemon[i].Name);
    Show;
    SetLength(Result, SelectionList.CheckCount);
    p:=0;
    for i:=0 to SelectionList.Items.Count-1 do
      if SelectionList.Checked[i] then
      begin
        Result[p]:=i;
        inc(p);
      end;
  finally
    Free;
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
  var
    c, s: Integer;
  begin
    FPKTB:=TeamBuilder;
    FCanvas:=TTextCanvas.Create;
    try
      with TStartForm.Create(FCanvas) do
      try
        Show;
        c:=TeamCount;
        s:=TeamSize;
      finally
        Free;
      end;
    TeamBuilder.GenerateTeams(s, c, @SelectLanguageG, @SelectGenerationG,
      @SelectMovesG, @SelectOKG, @SelectPG,
      @selectIG, @TeamGeneratedG);
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

procedure TPokeTeamBuilder.TeamGeneratedG(const ATeam: TTeam);
begin
  with TShowTeamForm.Create(FCanvas) do
  try
    PKTB:=FPKTB;
    Team:=ATeam;
    Show;
  finally
    Free;
  end;
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
    DB := TDBConnection.Create({$IFDEF Unix}'/etc/pktb/veekun-pokedex.sqlite'{$ELSE}'./veekun-pokedex.sqlite'{$Endif});
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
