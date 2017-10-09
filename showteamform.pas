unit ShowTeamForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TFBaseControls, TFControls, TFCanvas,
  TFTypes, math, TFLists, PkTBTypes, pktb, listrecords;

type

  { TShowTeamForm }

  TShowTeamForm = Class(TTextForm)
  private
    FPKTB: TPKTB;
    FTeam: TTeam;

    TeamStats: array of array[0..1] of TTFLabel;
    PokeSelector: TTFListBox;
    PokeStats: TTFLabel;

    NextButton: TTFButton;
    MovesLabel: TTFLabel;
    MoveLabel: array of TTFLabel;
    procedure NextClicked(Sender: TObject);
    procedure PokemonSelected(Sender: TObject);
  protected
    procedure Resize; override;
    function ProcessInput(inp: String): Boolean; override;
  public
    procedure Show; override;
    property PKTB: TPKTB read FPKTB write FPKTB;
    property Team: TTeam read FTeam write FTeam;
  end;


implementation

{ TShowTeamForm }

procedure TShowTeamForm.PokemonSelected(Sender: TObject);
var idx, i, j: Integer;
begin
    idx:=PokeSelector.ItemIndex;
    PokeStats.Text.Text:=Team.Pokemon[idx].Name;
    i:=PKTB.StrengthTable.FindType(Team.Pokemon[idx].Type1);

    if i<0 then i:= PKTB.StrengthTable.FindType(1);
    PokeStats.Text.Add(PKTB.StrengthTable[i].Name);
    if Team.Pokemon[idx].Type2>0 then
    begin
      i:=PKTB.StrengthTable.FindType(Team.Pokemon[idx].Type2);
      if i<0 then i:= PKTB.StrengthTable.FindType(1);
      PokeStats.Text.Add(PKTB.StrengthTable[i].Name);
    end;

    for i:=0 to PKTB.Moves.Count-1 do
    begin
      for j:=0 to Length(Team.Pokemon[idx].Moves)-1 do
        if team.Pokemon[idx].Moves[j].MID = PKTB.Moves[i].AttackID then
        begin
          if Team.Pokemon[idx].Moves[j].Available then
            MoveLabel[i].Foreground:=RGB(0,255,0)
          else
            MoveLabel[i].Foreground:=RGB(255,0,0);
          Break;
        end;
    end;
end;

procedure TShowTeamForm.NextClicked(Sender: TObject);
begin
  Close;
end;

procedure TShowTeamForm.Resize;
var m: TPoint;
  l, i: Integer;
begin
    m:=Point(Width div 2, Height div 2);
                                        
    PokeSelector.Width:=25;
    PokeSelector.Left:=(Width div 3)-PokeSelector.Width-1;
    PokeSelector.Top := m.y-3;
    PokeSelector.Height:=6;

    l:=Length(TeamStats);

    for i:=0 to l -1 do
    begin
      TeamStats[i][0].Left:=(Width div 3) * 2 + 1;
      TeamStats[i][0].Top:=m.y-(l div 2)+i;     
      TeamStats[i][1].Left:=TeamStats[i][0].Left+TeamStats[i][0].Width+1;
      TeamStats[i][1].Top:=m.y-(l div 2)+i;
    end;

    l:=PokeStats.Height+1+MovesLabel.Height+Length(MoveLabel);

    l:=m.y - (l div 2);

    PokeStats.Left:=m.x - PokeStats.Width div 2;
    PokeStats.Top:=l;

    inc(l, PokeStats.Height+1);

    MovesLabel.Top:=l;
    MovesLabel.Left:=m.x-(MovesLabel.Width div 2);
    inc(l, MovesLabel.Height);

    for i:=0 to Length(MoveLabel)-1 do
    begin
      MoveLabel[i].Left:=m.x-(MoveLabel[i].Width div 2);
      MoveLabel[i].Top:=i+l;
    end;

    NextButton.Top:=Height-NextButton.Height;
    NextButton.Left:=Width-NextButton.Width-1;
    inherited Resize;
end;

function TShowTeamForm.ProcessInput(inp: String): Boolean;
begin
  Result:=inherited ProcessInput(inp);
  if not Result and (inp=#13) then
  begin
    Result:=True;
    Close;
  end;
end;

procedure TShowTeamForm.Show;
var i,j: Integer;
begin          
    SetLength(TeamStats, Length(Team.Strength));
    for i:=0 to Length(TeamStats) -1 do
    begin
      TeamStats[i][0]:=TTFLabel.Create(Self);
      TeamStats[i][0].Text.Text:=Team.Strength[i].Name;
      TeamStats[i][1]:=TTFLabel.Create(Self);
      TeamStats[i][1].Text.Text:=Team.Strength[i].Factor.ToString;
      if Team.Strength[i].Factor <= Length(Team.Pokemon)*100+100 then
        TeamStats[i][1].Foreground:=RGB(255,0,0)
      else if Team.Strength[i].Factor <= Length(Team.Pokemon)*150+100 then
        TeamStats[i][1].Foreground:=RGB(255,255,0)
      else
        TeamStats[i][1].Foreground:=RGB(0,255,0);
    end;

    PokeSelector:=TTFListBox.Create(self);
    for i:=0 to Length(Team.Pokemon) -1 do
      PokeSelector.Items.Add(Team.Pokemon[i].Name);
    PokeSelector.OnSelect:=@PokemonSelected;

    PokeStats:=TTFLabel.Create(self);
    PokeStats.Text.Text:=Team.Pokemon[0].Name;
    i:=PKTB.StrengthTable.FindType(Team.Pokemon[0].Type1);
             
    PokeStats.Height:=3;
    PokeStats.Width:=15;
    PokeStats.Align:=alCenter;
    PokeStats.AutoSize:=false;
    if i<0 then i:= PKTB.StrengthTable.FindType(1);
    PokeStats.Text.Add(PKTB.StrengthTable[i].Name);
    if Team.Pokemon[0].Type2>0 then
    begin
      i:=PKTB.StrengthTable.FindType(Team.Pokemon[0].Type2);
      if i<0 then i:= PKTB.StrengthTable.FindType(1);
      PokeStats.Text.Add(PKTB.StrengthTable[i].Name);
    end;

    NextButton:=TTFButton.Create(self);
    NextButton.Caption:='Next';
    NextButton.OnClick:=@NextClicked;
    NextButton.Align:=alCenter;

    MovesLabel:= TTFLabel.Create(self);
    MovesLabel.Text.Text:='Moves:';
    SetLength(MoveLabel, PKTB.Moves.Count);
    for i:=0 to PKTB.Moves.Count-1 do
    begin
      MoveLabel[i]:=TTFLabel.Create(self);
      MoveLabel[i].Text.Text:=PKTB.Moves[i].AttackName;
      for j:=0 to Length(Team.Pokemon[0].Moves)-1 do
        if team.Pokemon[0].Moves[j].MID = PKTB.Moves[i].AttackID then
        begin
          if Team.Pokemon[0].Moves[j].Available then
            MoveLabel[i].Foreground:=RGB(0,255,0)
          else
            MoveLabel[i].Foreground:=RGB(255,0,0);
          Break;
        end;
    end;
  inherited Show;
end;

end.

