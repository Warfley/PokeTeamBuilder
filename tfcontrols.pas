unit TFControls;
           
{$Include defines.inc}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TFTypes, TFCanvas;

type

  { TTextControl }

  TTextControl = class
  private
    FCanvas: TTextCanvas;
    FLeft, FTop: Integer;
    FWidth, FHeight: Integer;
    FBackground: TColor;
    FForeground: TColor;
    procedure SetBG(AValue: TColor);
    procedure SetFG(AValue: TColor);
    procedure SetHeight(AValue: Integer);
    procedure SetLeft(AValue: Integer);
    procedure SetTop(AValue: Integer);
    procedure SetWidth(AValue: Integer);
  protected   
    FChanged: Boolean;
    procedure Draw(ACanvas: TTextCanvas); virtual;
  public
    constructor Create(ACanvas: TTextCanvas);
    destructor Destroy; override;
    procedure Redraw;
    property Canvas: TTextCanvas read FCanvas;
    property Left: Integer read FLeft write SetLeft;
    property Top: Integer read FTop write SetTop;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Background: TColor read FBackground write SetBG;
    property Foreground: TColor read FForeground write SetFG;
  end;

  TAlign = (alLeft, alCenter, alRight);

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
    constructor Create(ACanvas: TTextCanvas);
    destructor Destroy; override;
    property Text: TStringList read FText;
    property AutoSize: Boolean read FAutoSize write FAutoSize;
    property Align: TAlign read FAlign write SetAlign;
  end;

implementation

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

constructor TTFLabel.Create(ACanvas: TTextCanvas);
begin
  inherited Create(ACanvas);
  FForeground:=RGB(255,255,255);
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

{ TTextControl }

procedure TTextControl.SetBG(AValue: TColor);
begin
  if FBackground=AValue then Exit;
  FBackground:=AValue;
  FChanged:=True;
end;

procedure TTextControl.SetFG(AValue: TColor);
begin
  if FForeground=AValue then Exit;
  FForeground:=AValue;
  FChanged:=True;
end;

procedure TTextControl.SetHeight(AValue: Integer);
begin
  if FHeight=AValue then Exit;
  FHeight:=AValue;
  FChanged:=True;
end;

procedure TTextControl.SetLeft(AValue: Integer);
begin
  if FLeft=AValue then Exit;
  FLeft:=AValue;
  FChanged :=True;
end;

procedure TTextControl.SetTop(AValue: Integer);
begin
  if FTop=AValue then Exit;
  FTop:=AValue;
  FChanged:=True;
end;

procedure TTextControl.SetWidth(AValue: Integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
  FChanged:=True;
end;

procedure TTextControl.Draw(ACanvas: TTextCanvas);
begin
  ACanvas.SetColor(Background, Background);
  ACanvas.Rectangle(Left,Top,Width,Height);
  ACanvas.SetColor(Foreground, Background);
end;

constructor TTextControl.Create(ACanvas: TTextCanvas);
begin
  FChanged:=True;
  FCanvas:=ACanvas;
end;

destructor TTextControl.Destroy;
begin
  inherited Destroy;
end;

procedure TTextControl.Redraw;
begin
  if not FChanged then Exit;
  Draw(FCanvas);
  FChanged:=False;
end;

end.

