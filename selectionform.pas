unit SelectionForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TFBaseControls, TFControls, TFCanvas,
  TFTypes, math, TFLists;

type

  { TSelectionForm }

  TSelectionForm = Class(TTextForm)
  private
    CaptionLabel: TTFLabel;
    FListBox: TTFListBox;
    Continue: TTFButton;
    procedure ContinueClicked(Sender: TObject);
    function getCaption: String;
    procedure SetCaption(AValue: String);
  protected
    procedure Resize; override;
    function ProcessInput(inp: String): Boolean; override;
  public
    constructor Create(ACanvas: TTextCanvas); override;
    property ListBox: TTFListBox read FListBox;
    property Caption: String read getCaption write SetCaption;
  end;

implementation

{ TSelectionForm }

procedure TSelectionForm.ContinueClicked(Sender: TObject);
begin
  Close;
end;

function TSelectionForm.getCaption: String;
begin
  Result:=CaptionLabel.Text.Text;
end;

procedure TSelectionForm.SetCaption(AValue: String);
begin
  CaptionLabel.Text.Text:=AValue;
end;

procedure TSelectionForm.Resize;
var
  m: TPoint;
begin
  m:=Point(Width div 2, Height div 2);
  CaptionLabel.Left:=m.x-(CaptionLabel.Width div 2);
  CaptionLabel.Top:=m.y-6;

  ListBox.Top:=m.y-4;
  ListBox.Left:=m.x-(ListBox.Width div 2);

  Continue.Left:=Width-Continue.Width;
  Continue.Top:=Height-Continue.Height;
end;

function TSelectionForm.ProcessInput(inp: String): Boolean;
begin
  Result:=inherited ProcessInput(inp);
  if not Result and (inp=#13) then
  begin
    Result:=True;
    Close;
  end;
end;

constructor TSelectionForm.Create(ACanvas: TTextCanvas);
begin
  inherited Create(ACanvas);
  CaptionLabel:=TTFLabel.Create(Self);
  FListBox:=TTFListBox.Create(Self);
  ListBox.Width:=20;
  ListBox.Height:=6;
  Continue:=TTFButton.Create(Self);
  Continue.Caption:='Select';
  Continue.Align:=alCenter;
  Continue.OnClick:=@ContinueClicked;
end;

end.

