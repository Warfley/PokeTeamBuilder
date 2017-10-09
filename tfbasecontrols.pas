unit TFBaseControls;

{$Include defines.inc}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TFTypes, TFControls, TFCanvas, math;

type

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
    FCursorPos: Integer;
    FCursorColor: TColor;
    procedure SetAlign(AValue: TAlign);
    procedure SetCursorColor(AValue: TColor);
    procedure SetPosition(AValue: Integer);
    procedure SetText(AValue: String);
  protected
    procedure Draw(ACanvas: TTextCanvas); override;
  public
    function ProcessInput(inp: String): Boolean; override;
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
    function ProcessInput(inp: String): Boolean; override;
    constructor Create(AParent: TTextForm); override;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property Caption: String read FCaption write SetCaption;
    property Align: TAlign read FAlign write SetAlign;
  end;


implementation

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

function TTFButton.ProcessInput(inp: String): Boolean;
begin
  Result:=inp=#13;
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

function TTFEdit.ProcessInput(inp: String): Boolean;
begin
  Result:=True;
  case GetArrow(inp) of
  akLeft: Position:=Max(0, Position-1);
  akRight: Position:=Min(Text.Length, Position+1);
  akUp, akDown: Result:=False;
  akNone:
    case inp[1] of
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
     if (NumbersOnly and (inp[1] in ['0'..'9'])) or not NumbersOnly then
     begin
      Text:=FText.Substring(0, FCursorPos)+inp[1]+FText.Substring(FCursorPos);
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
  FocusedBackground:=RGB(0,128,255);
  FocusedForeground:=Background;
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

