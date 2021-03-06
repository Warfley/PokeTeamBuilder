unit CheckListForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TFBaseControls, TFControls, TFCanvas,
  TFTypes, math, TFLists;

type

  { TCheckListForm }

  TCheckListForm = Class(TTextForm)
  private
    CaptionLabel: TTFLabel;
    ListBox: TTFCheckListBox;
    SelectAll: TTFButton;
    DeselectAll: TTFButton;
    Continue: TTFButton;
    FilterEdit: TTFEdit;
    FilterLabel: TTFLabel;
    procedure ContinueClicked(Sender: TObject);
    procedure DeSelectAllClicked(Sender: TObject);
    procedure FilterChanged(Sender: TObject);
    function getCaption: String;
    procedure SelectAllClicked(Sender: TObject);
    procedure SetCaption(AValue: String);
  protected
    procedure Resize; override;
    function ProcessInput(inp: String): Boolean; override;
  public
    constructor Create(ACanvas: TTextCanvas); override;
    property SelectionList: TTFCheckListBox read ListBox;
    property Caption: String read getCaption write SetCaption;
  end;

implementation

{ TCheckListForm }

function TCheckListForm.getCaption: String;
begin
  Result:=CaptionLabel.Text.Text;
end;

procedure TCheckListForm.SelectAllClicked(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to ListBox.Items.Count-1 do
    ListBox.Checked[i]:=True;
end;

procedure TCheckListForm.ContinueClicked(Sender: TObject);
begin
  Close;
end;

procedure TCheckListForm.DeSelectAllClicked(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to ListBox.Items.Count-1 do
    ListBox.Checked[i]:=False;
end;

procedure TCheckListForm.FilterChanged(Sender: TObject);
begin
  ListBox.Filter:=FilterEdit.Text;
end;

procedure TCheckListForm.SetCaption(AValue: String);
begin
  CaptionLabel.Text.Text:=AValue;
end;

procedure TCheckListForm.Resize;
var
  m: TPoint;
begin
  m:=Point(Width div 2, Height div 2);
    CaptionLabel.Top:=0;
    CaptionLabel.Left:=m.x-(CaptionLabel.Width div 2);

    ListBox.Height:=Height - 8;
    ListBox.Width:=Min(60, Width);
    ListBox.Left:=m.x-(ListBox.Width div 2);
    ListBox.Top:=3;

    SelectAll.Left:=ListBox.Left;
    SelectAll.Top:=ListBox.Top+ListBox.Height+1;

    DeselectAll.Left:=SelectAll.Left+SelectAll.Width + 1;
    DeselectAll.Top:=ListBox.Top+ListBox.Height+1;

    FilterLabel.Top:=ListBox.Top+ListBox.Height+3;
    FilterLabel.Left:=ListBox.Left;

    FilterEdit.Top:=ListBox.Top+ListBox.Height+4;
    FilterEdit.Left:=ListBox.Left;
    FilterEdit.Width:=ListBox.Width;

    Continue.Left:=Width-Continue.Width-1;
    Continue.Top:=Height-Continue.Height;

    if FilterEdit.Left+FilterEdit.Width>=Continue.Left then
      FilterEdit.Width:=Continue.Left-1 -FilterEdit.Left;

    inherited Resize;
end;

function TCheckListForm.ProcessInput(inp: String): Boolean;
begin
  Result:=inherited ProcessInput(inp);
  if not Result and (inp=#13) then
  begin
    Result:=True;
    Close;
  end;
end;

constructor TCheckListForm.Create(ACanvas: TTextCanvas);
begin
  inherited Create(ACanvas);
  CaptionLabel:=TTFLabel.Create(Self);
  ListBox:=TTFCheckListBox.Create(Self);
  SelectAll:=TTFButton.Create(self);
  SelectAll.OnClick:=@SelectAllClicked;
  SelectAll.Caption:='ALL';
  SelectAll.Width:=5;  
  SelectAll.Align:=alCenter;
  SelectAll.Height:=1;
  DeselectAll:=TTFButton.Create(self);
  DeselectAll.OnClick:=@DeSelectAllClicked;
  DeselectAll.Caption:='NONE';
  DeselectAll.Width:=6;
  DeselectAll.Height:=1;
  DeSelectAll.Align:=alCenter;
  FilterLabel:=TTFLabel.Create(Self);
  FilterLabel.Text.Text:='Filter';
  FilterEdit:=TTFEdit.Create(self);
  FilterEdit.OnEnterKey:=@FilterChanged;
  FilterEdit.Text:='';
  Continue:=TTFButton.Create(self);
  Continue.Align:=alCenter;
  Continue.Caption:='Continue';
  Continue.OnClick:=@ContinueClicked;
end;

end.

