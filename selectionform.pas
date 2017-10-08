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
    function ProcessChar(c: Char; Shift: TShiftState): Boolean; override;
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

  Continue.Left:=Width-Continue.Width-1;
  Continue.Top:=Height-Continue.Height-1;
end;

function TSelectionForm.ProcessChar(c: Char; Shift: TShiftState): Boolean;
begin
  if c=#13 then Close;
  Result:=inherited ProcessChar(c, Shift) or (c=#13);
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

