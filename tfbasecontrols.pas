unit TFBaseControls;

{$Include defines.inc}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TFTypes, TFControls, TFCanvas, math;

type  

  { TTFListBox }

  TTFListBox = class(TUserControl)
  private
    FOnSelect: TNotifyEvent;
    FItems: TStringList;
    FItemIndex: Integer;
    FSelectionBG, FSelectionFG: TColor;
    FTopRow: Integer;
    FCompleteUpdate: Boolean;
    FUpdateRows: TList;
    procedure ItemsChanged(Sender: TObject);
    procedure SetItemIndex(AValue: Integer);
    procedure SetSelectionBackground(AValue: TColor);
    procedure SetSelectionForeground(AValue: TColor);
    procedure SetTopRow(AValue: Integer);
  protected
    procedure Draw(ACanvas: TTextCanvas); override;
    procedure Resize; override;
    procedure ColorChange; override;
  public
    procedure FullReadraw;
    function ProcessChar(c: Char): Boolean; override;
    constructor Create(AParent: TTextForm); override;
    destructor Destroy; override;
    procedure Update(FullRepaint: Boolean=false); override;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property Items: TStringList read FItems;
    property SelectionBackground: TColor read FSelectionBG write SetSelectionBackground;
    property SelectionForeground: TColor read FSelectionFG write SetSelectionForeground;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property TopRow: Integer read FTopRow write SetTopRow;
  end;

  { TTFCheckListBox }

  TTFCheckListBox = class(TUserControl)
  private
    FOnSelect: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FItems: TStringList;
    FItemIndex: Integer;
    FSelectionBG, FSelectionFG: TColor;
    FTopRow: Integer;
    FCompleteUpdate: Boolean;
    FUpdateRows: TList;
    function GetChecked(Item: Integer): Boolean;
    procedure ItemsChanged(Sender: TObject);
    procedure SetChecked(Item: Integer; AValue: Boolean);
    procedure SetItemIndex(AValue: Integer);
    procedure SetSelectionBackground(AValue: TColor);
    procedure SetSelectionForeground(AValue: TColor);
    procedure SetTopRow(AValue: Integer);
  protected
    procedure Draw(ACanvas: TTextCanvas); override;
    procedure Resize; override;
    procedure ColorChange; override;
  public
    procedure FullReadraw;
    function ProcessChar(c: Char): Boolean; override;
    constructor Create(AParent: TTextForm); override;
    destructor Destroy; override;
    procedure Update(FullRepaint: Boolean=false); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property Items: TStringList read FItems;
    property SelectionBackground: TColor read FSelectionBG write SetSelectionBackground;
    property SelectionForeground: TColor read FSelectionFG write SetSelectionForeground;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property TopRow: Integer read FTopRow write SetTopRow;
    property Checked[Item: Integer]: Boolean read GetChecked write SetChecked;
  end;

{ TTFLabel }

  TTFLabel = class(TTextControl)
  private
    FText: TStringList;
    FAutoSize: Boolean;
    FAlign: TAlign;
    procedure SetAlign(AValue: TAlign);
    procedure TextChanged(Sender: TObject);
  protected
    procedure Draw(ACanvas: TTextCanvas); override;
  public
    constructor Create(AParent: TTextForm); override;
    destructor Destroy; override;
    property Text: TStringList read FText;
    property AutoSize: Boolean read FAutoSize write FAutoSize;
    property Align: TAlign read FAlign write SetAlign;
  end;

  { TTFEdit }

  TTFEdit = class(TUserControl)
  private
    FNumbersOnly: Boolean;
    FOnChange: TNotifyEvent;
    FText: String;
    FAlign: TAlign;
    FCursorPos: SizeInt;
    FCursorColor: TColor;
    procedure SetAlign(AValue: TAlign);
    procedure SetCursorColor(AValue: TColor);
    procedure SetPosition(AValue: Integer);
    procedure SetText(AValue: String);
  protected
    procedure Draw(ACanvas: TTextCanvas); override;
  public
    function ProcessChar(c: Char): Boolean; override;
    constructor Create(AParent: TTextForm); override;
    destructor Destroy; override;
    property NumbersOnly: Boolean read FNumbersOnly write FNumbersOnly;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Text: String read FText write SetText;
    property CursorColor: TColor read FCursorColor write SetCursorColor;
    property Position: Integer read FCursorPos write SetPosition;
    property Align: TAlign read FAlign write SetAlign;
  end;

  { TTFButton }

  TTFButton = class(TUserControl)
  private
    FOnClick: TNotifyEvent;
    FCaption: String;
    FAlign: TAlign;
    procedure SetAlign(AValue: TAlign);
    procedure SetCaption(AValue: String);
  protected
    procedure Draw(ACanvas: TTextCanvas); override;
  public
    function ProcessChar(c: Char): Boolean; override;
    constructor Create(AParent: TTextForm); override;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property Caption: String read FCaption write SetCaption;
    property Align: TAlign read FAlign write SetAlign;
  end;


implementation

{ TTFListBox }

procedure TTFListBox.ItemsChanged(Sender: TObject);
begin
  FChanged:=True;
  FCompleteUpdate:=True;
end;

procedure TTFListBox.SetItemIndex(AValue: Integer);
begin
  if FItemIndex=AValue then Exit;
  FUpdateRows.Add(Pointer(ItemIndex));
  FItemIndex:=AValue;
  FChanged:=True;
  if (ItemIndex<0) or (ItemIndex>=FItems.Count) then Exit;
  if (ItemIndex<TopRow) then
    TopRow:=Max(0, ItemIndex-Height-1)
  else if ItemIndex>TopRow+Height-1 then
    TopRow:=Min(ItemIndex, Items.Count-Height-1);

  FUpdateRows.Add(Pointer(ItemIndex));
  if Assigned(FOnSelect) then FOnSelect(Self);
end;

procedure TTFListBox.SetSelectionBackground(AValue: TColor);
begin
  if FSelectionBG=AValue then Exit;
  FSelectionBG:=AValue;
  FChanged:=Focused;
  FUpdateRows.Add(Pointer(ItemIndex));
end;

procedure TTFListBox.SetSelectionForeground(AValue: TColor);
begin
  if FSelectionFG=AValue then Exit;
  FSelectionFG:=AValue;
  FChanged:=Focused;
  FUpdateRows.Add(Pointer(ItemIndex));
end;

procedure TTFListBox.SetTopRow(AValue: Integer);
begin
  if FTopRow=AValue then Exit;
  FTopRow:=AValue;
  FChanged:=Focused;
  FCompleteUpdate:=True;
end;

procedure TTFListBox.Draw(ACanvas: TTextCanvas);
var fg, bg, i, x, j: Integer;
  str,s: String;
begin
  if Focused then
  begin
    fg:=SelectionForeground;
    bg:=SelectionBackground;
  end
  else
  begin
    fg:=FocusedForeground;
    bg:=FocusedBackground;
  end;
  if FCompleteUpdate then
  begin
  ACanvas.SetColor(Background, Background);
  ACanvas.Rectangle(Left,Top, Width, Height);
    for i:=TopRow to Min(TopRow+Height, Items.Count)-1 do
    begin
      if i=ItemIndex then
        ACanvas.SetColor(fg, bg)
      else
        ACanvas.SetColor(Foreground, Background);
      str:=Items[i];
      SetLength(s, Width);
      s[1]:=' ';
      s[Width]:=' ';
      for x:=1 to Width-2 do
        if x<=str.Length then
          s[x+1]:=str[x]
        else s[x+1]:=' ';
      ACanvas.TextOut(Left, Top+i-TopRow, s);
    end
  end
  else
    for j:=0 to FUpdateRows.Count-1 do
    begin
      i:=Integer(FUpdateRows[j]);
      if (i<0) or (i>=Items.Count) then Continue;
      if i=ItemIndex then
        ACanvas.SetColor(fg, bg)
      else
        ACanvas.SetColor(Foreground, Background);
      str:=Items[i];
      SetLength(s, Width);
      s[1]:=' ';
      s[Width]:=' ';
      for x:=1 to Width-2 do
        if x<=str.Length then
          s[x+1]:=str[x]
        else s[x+1]:=' ';
      ACanvas.TextOut(Left, Top+i-TopRow, s);
    end;
end;

procedure TTFListBox.Resize;
begin
  inherited Resize;
  FullReadraw;
end;

procedure TTFListBox.ColorChange;
begin
  inherited ColorChange;
  FullReadraw;
end;

procedure TTFListBox.FullReadraw;
begin
  FCompleteUpdate:=True;
end;

function TTFListBox.ProcessChar(c: Char): Boolean;
function Roll(pos: Integer): Integer;
begin
  if pos < 0 then Result:=pos+Items.Count
  else Result := pos mod Items.Count;
end;

begin
  Result:=True;
  case GetArrow(c) of
  akUp: ItemIndex:=Roll(ItemIndex-1);
  akDown: ItemIndex:=Roll(ItemIndex+1);
  else
    Result:=False;
  end;
end;

constructor TTFListBox.Create(AParent: TTextForm);
begin
  FItems:=TStringList.Create;
  FItemIndex:=0;
  FItems.OnChange:=@ItemsChanged;
  FUpdateRows:=TList.Create;
  FCompleteUpdate:=True;
  inherited Create(AParent);
  Width:=40;
  Height:=10;
  Background:=RGB(255,255,255);
  Foreground:=RGB(0,0,0);
  FocusedBackground:=RGB(192,192,192);
  FocusedForeground:=Foreground;
  SelectionBackground:=RGB(0,128,255);
  SelectionForeground:=Background;
end;

destructor TTFListBox.Destroy;
begin                
  FItems.Free;
  FUpdateRows.Free;
  inherited Destroy;
end;

procedure TTFListBox.Update(FullRepaint: Boolean);
begin
  FCompleteUpdate:=FCompleteUpdate or FullRepaint;
  inherited Update(FCompleteUpdate);
  FUpdateRows.Clear;
  FCompleteUpdate:=False;
end;

{ TTFCheckListBox }

procedure TTFCheckListBox.SetItemIndex(AValue: Integer);
begin
  if FItemIndex=AValue then Exit; 
  FUpdateRows.Add(Pointer(ItemIndex));
  FItemIndex:=AValue;  
  FChanged:=True;
  if (ItemIndex<0) or (ItemIndex>=FItems.Count) then Exit;
  if (ItemIndex<TopRow) then
    TopRow:=Max(0, ItemIndex-Height-1)
  else if ItemIndex>TopRow+Height-1 then
    TopRow:=Min(ItemIndex, Items.Count-Height-1);

  FUpdateRows.Add(Pointer(ItemIndex));
  if Assigned(FOnSelect) then FOnSelect(Self);
end;

function TTFCheckListBox.GetChecked(Item: Integer): Boolean;
begin
  Result:=IntPtr(Items.Objects[Item])<>0;
end;

procedure TTFCheckListBox.ItemsChanged(Sender: TObject);
begin
  FChanged:=True;
end;

procedure TTFCheckListBox.SetChecked(Item: Integer; AValue: Boolean);
begin
  if AValue then
    Items.Objects[Item]:=TObject(-1)
  else
    Items.Objects[Item]:=TObject(0);

  FUpdateRows.Add(Pointer(Item));
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TTFCheckListBox.SetSelectionBackground(AValue: TColor);
begin
  if FSelectionBG=AValue then Exit;
  FSelectionBG:=AValue;
  FChanged:=Focused;
  FUpdateRows.Add(Pointer(ItemIndex));
end;

procedure TTFCheckListBox.SetSelectionForeground(AValue: TColor);
begin
  if FSelectionFG=AValue then Exit;
  FSelectionFG:=AValue;
  FChanged:=Focused;
end;

procedure TTFCheckListBox.SetTopRow(AValue: Integer);
begin
  if FTopRow=AValue then Exit;
  FTopRow:=AValue;
  FChanged:=True;
  FCompleteUpdate:=True;
end;

procedure TTFCheckListBox.Draw(ACanvas: TTextCanvas);
var fg, bg, i, x, j: Integer;
  str,s: String;
  mark: Char;
begin
  if Focused then
  begin
    fg:=SelectionForeground;
    bg:=SelectionBackground;
  end
  else
  begin
    fg:=FocusedForeground;
    bg:=FocusedBackground;
  end;
  if FCompleteUpdate then
  begin 
  ACanvas.SetColor(Background, Background);
  ACanvas.Rectangle(Left,Top, Width, Height);
    for i:=TopRow to Min(TopRow+Height, Items.Count)-1 do
    begin
      if i=ItemIndex then
        ACanvas.SetColor(fg, bg)
      else
        ACanvas.SetColor(Foreground, Background);
      if Checked[i] then mark:='X' else mark:='_';
      str:=mark+'|'+Items[i];
      SetLength(s, Width);
      s[1]:=' ';
      s[Width]:=' ';
      for x:=1 to Width-2 do
        if x<=str.Length then
          s[x+1]:=str[x]
        else s[x+1]:=' ';
      ACanvas.TextOut(Left, Top+i-TopRow, s);
    end
  end
  else
    for j:=0 to FUpdateRows.Count-1 do
    begin
      i:=Integer(FUpdateRows[j]);
      if (i<0) or (i>=Items.Count) then Continue;
      if i=ItemIndex then
        ACanvas.SetColor(fg, bg)
      else
        ACanvas.SetColor(Foreground, Background);
      if Checked[i] then mark:='X' else mark:='_';
      str:=mark+'|'+Items[i];
      SetLength(s, Width);
      s[1]:=' ';
      s[Width]:=' ';
      for x:=1 to Width-2 do
        if x<=str.Length then
          s[x+1]:=str[x]
        else s[x+1]:=' ';
      ACanvas.TextOut(Left, Top+i-TopRow, s);
    end;
end;

procedure TTFCheckListBox.Resize;
begin
  inherited Resize;
  FullReadraw;
end;

procedure TTFCheckListBox.ColorChange;
begin
  inherited ColorChange;
  FullReadraw;
end;

procedure TTFCheckListBox.FullReadraw;
begin
  FCompleteUpdate:=True;
end;

function TTFCheckListBox.ProcessChar(c: Char): Boolean;
function Roll(pos: Integer): Integer;
begin
  if pos < 0 then Result:=pos+Items.Count
  else Result := pos mod Items.Count;
end;

begin
  Result:=True;
  case GetArrow(c) of
  akUp: ItemIndex:=Roll(ItemIndex-1);
  akDown: ItemIndex:=Roll(ItemIndex+1);
  else
    case c of
    ' ': Checked[ItemIndex] := not Checked[ItemIndex];
    else
      Result:=False;
    end;
  end;
end;

constructor TTFCheckListBox.Create(AParent: TTextForm);
begin
  FItems:=TStringList.Create;
  FItemIndex:=0;
  FItems.OnChange:=@ItemsChanged;
  FUpdateRows:=TList.Create;
  FCompleteUpdate:=True;
  inherited Create(AParent);
  Width:=40;
  Height:=10;
  Background:=RGB(255,255,255);
  Foreground:=RGB(0,0,0);
  FocusedBackground:=RGB(192,192,192);
  FocusedForeground:=Foreground;
  SelectionBackground:=RGB(0,128,255);
  SelectionForeground:=Background;
end;

destructor TTFCheckListBox.Destroy;
begin
  FItems.Free;
  FUpdateRows.Free;
  inherited Destroy;
end;

procedure TTFCheckListBox.Update(FullRepaint: Boolean);
begin
  FCompleteUpdate:=FCompleteUpdate or FullRepaint;
  inherited Update(FCompleteUpdate);
  FUpdateRows.Clear;
  FCompleteUpdate:=False;
end;

{ TTFButton }

procedure TTFButton.SetAlign(AValue: TAlign);
begin
  if FAlign=AValue then Exit;
  FAlign:=AValue;
  FChanged:=True;
end;

procedure TTFButton.SetCaption(AValue: String);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  FChanged:=True;
end;

procedure TTFButton.Draw(ACanvas: TTextCanvas);
begin
  inherited Draw(ACanvas);
  case Align of
      alLeft:
        ACanvas.TextOut(Left, Top+1,FCaption);
      alRight:
        ACanvas.TextOut(Left+Width-FCaption.Length, Top+1,FCaption);
      alCenter:
        ACanvas.TextOut(Left+((Width - FCaption.Length) div 2), Top+1,FCaption);
    end;
end;

function TTFButton.ProcessChar(c: Char): Boolean;
begin
  Result:=c=#13;
  if Result and Assigned(FOnClick) then FOnClick(Self);
end;

constructor TTFButton.Create(AParent: TTextForm);
begin
  inherited Create(AParent);
  Width:=20;
  Height:=3;
  Foreground:=RGB(0,0,0);
  Background:=RGB(200,200,200);
  FocusedBackground:=RGB(255,0,0);
  FocusedForeground:=RGB(255,255,255);
  Caption:='Click me';
end;

{ TTFEdit }

procedure TTFEdit.SetAlign(AValue: TAlign);
begin
  if FAlign = AValue then Exit;
  FAlign:=AValue;
  FChanged:=True;
end;

procedure TTFEdit.SetCursorColor(AValue: TColor);
begin
  if FCursorColor=AValue then Exit;
  FCursorColor:=AValue;
  FChanged:=True;
end;

procedure TTFEdit.SetPosition(AValue: Integer);
begin
  if FCursorPos=AValue then Exit;
  FCursorPos:=AValue;
  FChanged:=True;
end;

procedure TTFEdit.SetText(AValue: String);
begin
  if FText=AValue then Exit;
  FText:=AValue;
  FChanged:=True;
  if FText.Length<FCursorPos then FCursorPos:=FText.Length;
end;

procedure TTFEdit.Draw(ACanvas: TTextCanvas);
var fg, bg: TColor;
begin
  if Focused then
  begin
    fg:=FocusedForeground;
    bg:=FocusedBackground;
  end
  else
  begin
    fg:=Foreground;
    bg:=Background;
  end;
  inherited Draw(ACanvas);
  case Align of
  alLeft:
  begin
    ACanvas.SetColor(fg, bg);
    ACanvas.TextOut(Left, Top,Text.Substring(0, FCursorPos));
    ACanvas.SetColor(fg,CursorColor);
    if Text.Length=FCursorPos then
      ACanvas.TextOut(left+FCursorPos, Top, ' ')
    else
    begin
      ACanvas.TextOut(Left+FCursorPos, Top, Text.Substring(FCursorPos, 1));
      ACanvas.SetColor(fg, bg);
      ACanvas.TextOut(Left+FCursorPos+1, Top,Text.Substring(FCursorPos+1));
    end;
  end;
  alRight:
    ACanvas.TextOut(Left+Width-Text.Length, Top,Text);
  alCenter:
    ACanvas.TextOut(Left+((Width - Text.Length) div 2), Top, Text);
  end;
end;

function TTFEdit.ProcessChar(c: Char): Boolean;
begin
  Result:=True;
  case GetArrow(c) of
  akLeft: Position:=Max(0, Position-1);
  akRight: Position:=Min(Text.Length, Position+1);
  akUp, akDown: Result:=False;
  akNone:
    case c of
    #8:
     begin
       // FText because of FCursorPos
      FText:=FText.Substring(0, FCursorPos-1)+FText.Substring(FCursorPos);
      dec(FCursorPos);
      FChanged:=True;
       if Assigned(FOnChange) then
        FOnChange(Self);
     end;
    #33..#254:
     if (NumbersOnly and (c in ['0'..'9'])) or not NumbersOnly then
     begin
      Text:=FText.Substring(0, FCursorPos)+c+FText.Substring(FCursorPos);
       inc(FCursorPos);
       if Assigned(FOnChange) then
        FOnChange(Self);
     end;
    else Result:=False;
    end;
  end;
end;

constructor TTFEdit.Create(AParent: TTextForm);
begin
  inherited Create(AParent);
  Width:=20;
  Height:=1;
  Align:=alLeft;
  Background:=RGB(255,255,255);
  Foreground:=RGB(0,0,0);
  FCursorColor:=RGB(196,196,196);
  FocusedBackground:=RGB(0,255,255);
  FocusedForeground:=Foreground;
  FText:='Edit';
  FCursorPos:=FText.Length;
end;

destructor TTFEdit.Destroy;
begin
  inherited Destroy;
end;


{ TTFLabel }

procedure TTFLabel.SetAlign(AValue: TAlign);
begin
  if FAlign=AValue then Exit;
  FAlign:=AValue;
  FChanged:=True;
end;

procedure TTFLabel.TextChanged(Sender: TObject);
var
  s: String;
begin
  FChanged:=True;
  if AutoSize then
  begin
    Height:=Text.Count;
    for s in Text do
      if s.Length>Width then Width:=s.Length;
  end;
end;

procedure TTFLabel.Draw(ACanvas: TTextCanvas);
var
  i: Integer;
begin
  inherited Draw(ACanvas);
    for i:=0 to Text.Count-1 do
      case Align of
      alLeft:
        ACanvas.TextOut(Left, Top+i,Text[i]);
      alRight:
        ACanvas.TextOut(Left+Width-Text[i].Length, Top+i,Text[i]);
      alCenter:
        ACanvas.TextOut(Left+((Width - Text[i].Length) div 2), Top+i,Text[i]);
    end;
end;

constructor TTFLabel.Create(AParent: TTextForm);
begin
  inherited Create(AParent);
  Foreground:=RGB(255,255,255);
  Background:=Transparency;
  AutoSize:=True;
  FText:=TStringList.Create;
  FText.OnChange:=@TextChanged;
end;

destructor TTFLabel.Destroy;
begin
  FText.Free;
  inherited Destroy;
end;

end.

