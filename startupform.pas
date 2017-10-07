unit StartUpForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TFBaseControls, TFControls, TFCanvas, TFTypes;

type

  { TStartForm }

  TStartForm = Class(TTextForm)
  private
    TSLabel: TTFLabel;
    TCLabel: TTFLabel;
    TeamSizeEdit: TTFEdit;
    TeamCountEdit: TTFEdit;
    Button: TTFButton;
    FTeamSize, FTeamCount: Integer;
    procedure ButtonClicked(Sender: TObject);
  protected
    procedure Resize; override;
    function ProcessChar(c: Char): Boolean; override;
  public
    constructor Create(ACanvas: TTextCanvas); override;
    destructor Destroy; override;
    property TeamSize: Integer read FTeamSize;
    property TeamCount: Integer read FTeamCount;
  end;

implementation

{ TStartForm }

procedure TStartForm.ButtonClicked(Sender: TObject);
begin
  FTeamCount:=TeamCountEdit.Text.ToInteger;
  FTeamSize:=TeamSizeEdit.Text.ToInteger;
  Close;
end;

procedure TStartForm.Resize;
var m: TPoint;
begin
  m:=Point(Width div 2, Height div 2);
  TSLabel.Left := m.x - (TCLabel.Width div 2);
  TSLabel.Top := m.y - 5;
  TCLabel.Left := m.x - (TCLabel.Width div 2);
  TCLabel.Top := m.y - 2;

  TeamSizeEdit.Left := m.x - (TeamSizeEdit.Width div 2);
  TeamSizeEdit.Top := m.y - 4;

  TeamCountEdit.Left := m.x - (TeamCountEdit.Width div 2);
  TeamCountEdit.Top := m.y - 1;

  Button.Left:= Width-Button.Width-1;
  Button.Top:=Height-Button.Height-1;
end;

function TStartForm.ProcessChar(c: Char): Boolean;
begin
  if c=#13 then ButtonClicked(Nil);
  Result:=inherited ProcessChar(c) or (c=#13);
end;

constructor TStartForm.Create(ACanvas: TTextCanvas);
begin
  inherited Create(ACanvas);
  TSLabel:=TTFLabel.Create(Self);
  TSLabel.Text.Text := 'Select teamsize';
  TCLabel:=TTFLabel.Create(Self);  
  TCLabel.Text.Text := 'Select teamcount';
  TeamSizeEdit:=TTFEdit.Create(Self);
  TeamSizeEdit.NumbersOnly:=True;
  TeamSizeEdit.Text:='6';
  TeamCountEdit:=TTFEdit.Create(Self);
  TeamCountEdit.NumbersOnly:=True;
  TeamCountEdit.Text:='10';
  Button:= TTFButton.Create(self);
  with button do
  begin
    Caption:='Generate teams';
    Align:=alCenter;
    OnClick:=@ButtonClicked;
  end;
end;

destructor TStartForm.Destroy;
begin
  inherited Destroy;
end;

end.

