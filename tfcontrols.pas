unit TFControls;
           
{$Include defines.inc}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TFTypes, TFCanvas, fgl, Controls;

type
  TTextControl = class;
  TUserControl = class;

  TControlList = specialize TFPGObjectList<TTextControl>;
  TUCList = specialize TFPGObjectList<TUserControl>;

  { TTextForm }

  TTextForm = class
  private
    FCanvas: TTextCanvas;
    FControls: TControlList;
    FUserControls: TUCList;
    FTabPosition: SizeInt;
    FWidth, FHeight: Integer;
    FClosed: Boolean;
    FBackground: TColor;
    forceUpdate: Boolean;
    procedure SetBackground(AValue: TColor);
  protected
    procedure Resize; virtual; abstract;   
    function ProcessInput(inp: String): Boolean; virtual;
  public
    procedure SetFocus(Item: TUserControl);
    procedure Close;
    constructor Create(ACanvas: TTextCanvas); virtual;
    destructor Destroy; override;
    procedure Show; virtual;
    procedure Add(Ctrl: TTextControl);
    procedure Remove(Ctrl: TTextControl);
    property Canvas: TTextCanvas read FCanvas;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Closed: Boolean read FClosed write FClosed;
    property Background: TColor read FBackground write SetBackground;
  end;

  { TTextControl }

  TTextControl = class
  private
    FParent: TTextForm;
    FLeft, FTop: Integer;
    FWidth, FHeight: Integer;
    FBackground: TColor;
    FForeground: TColor;
    function GetCanvas: TTextCanvas;
    procedure SetBG(AValue: TColor);
    procedure SetFG(AValue: TColor);
    procedure SetHeight(AValue: Integer);
    procedure SetLeft(AValue: Integer);
    procedure SetParent(AValue: TTextForm);
    procedure SetTop(AValue: Integer);
    procedure SetWidth(AValue: Integer);
  protected   
    FChanged: Boolean;
    procedure Draw(ACanvas: TTextCanvas); virtual;
    procedure Resize; virtual;
    procedure ColorChange; virtual;
    procedure PositionChanged; virtual;
  public
    constructor Create(AParent: TTextForm);virtual;
    destructor Destroy; override;
    procedure Update(FullRepaint: Boolean = false); virtual;
    property Parent: TTextForm read FParent write SetParent;
    property Canvas: TTextCanvas read GetCanvas;
    property Left: Integer read FLeft write SetLeft;
    property Top: Integer read FTop write SetTop;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Background: TColor read FBackground write SetBG;
    property Foreground: TColor read FForeground write SetFG;
  end;

  TAlign = (alLeft, alCenter, alRight);

  { TUserControl }

  TUserControl = class(TTextControl)
  private
    FFocused: Boolean;
    FFocusedBG: TColor;
    FFocusedFG: TColor;
    procedure SetFBG(AValue: TColor);
    procedure SetFFG(AValue: TColor);
    procedure SetFocused(AValue: Boolean);
  protected
    procedure Draw(ACanvas: TTextCanvas); override;
    procedure FocusChanged; virtual;
  public
    function ProcessInput(inp: String): Boolean; virtual; abstract;
    property Focused: Boolean read FFocused write SetFocused;
    property FocusedForeground: TColor read FFocusedFG write SetFFG;
    property FocusedBackground: TColor read FFocusedBG write SetFBG;
  end;

  { TTFListBox }

  { TDeltaUpdateControl }

  TDeltaUpdateControl = class(TUserControl)
  private
    FCompleteUpdate: Boolean;
  protected
    procedure DeltaDraw(ACanvas: TTextCanvas); virtual;
    procedure Resize; override;
    procedure ColorChange; override;
    procedure DrawDone; virtual;
  public
    procedure FullReadraw; virtual;
    constructor Create(AParent: TTextForm); override;
    procedure Update(FullRepaint: Boolean=false); override;
  end;

implementation

{ TDeltaUpdateControl }

procedure TDeltaUpdateControl.DeltaDraw(ACanvas: TTextCanvas);
begin
  // NOOP;
end;

procedure TDeltaUpdateControl.Resize;
begin
  inherited Resize;
  FCompleteUpdate:=True;
end;

procedure TDeltaUpdateControl.ColorChange;
begin
  inherited ColorChange;
  FCompleteUpdate:=True;
end;

procedure TDeltaUpdateControl.DrawDone;
begin
  FCompleteUpdate:=False;
  FChanged:=False;
end;

procedure TDeltaUpdateControl.FullReadraw;
begin
  FCompleteUpdate:=True;
  FChanged:=True;
end;

constructor TDeltaUpdateControl.Create(AParent: TTextForm);
begin
  FCompleteUpdate:=False;
  inherited Create(AParent);
end;

procedure TDeltaUpdateControl.Update(FullRepaint: Boolean);
begin
  FCompleteUpdate:=FCompleteUpdate or FullRepaint;
  if FCompleteUpdate then
    Draw(Canvas)
  else
    DeltaDraw(Canvas);
  DrawDone;
end;

{ TTextForm }

procedure TTextForm.SetBackground(AValue: TColor);
begin
  if FBackground=AValue then Exit;
  FBackground:=AValue;
  ForceUpdate:=True;
end;

function TTextForm.ProcessInput(inp: String): Boolean;
function Roll(i: Integer): Integer;
begin
  if i<0 then
    Result:=i+FUserControls.Count
  else
    Result:=i mod FUserControls.Count;
end;

begin
  Result:=True;
  if inp=#9 then
        SetFocus(FUserControls[Roll(FTabPosition+1)])
  else
    Result:=False;
end;

procedure TTextForm.SetFocus(Item: TUserControl);
var
  i: Integer;
begin
  for i:=0 to FUserControls.Count-1 do
    if FUserControls[i]= Item then
    begin
      if i= FTabPosition then Exit;
      FUserControls[FTabPosition].SetFocused(False);
      FTabPosition:=i;
      FUserControls[i].SetFocused(True);
      Break;
    end;
end;

procedure TTextForm.Close;
begin
  Closed:=True;
end;

constructor TTextForm.Create(ACanvas: TTextCanvas);
begin
  FCanvas:=ACanvas;
  FControls:=TControlList.Create();
  FUserControls:=TUCList.Create(False);
  FBackground:=RGB(0,0,0);
end;

destructor TTextForm.Destroy;
begin
  FControls.Free;
  FUserControls.Free;
  inherited Destroy;
end;

procedure TTextForm.Show;
var
  i: Integer;
  ws: TWindowSize;
  f: Boolean;
  inp: String;
begin
  Closed:=False;
  repeat 
    f:=forceUpdate;
    forceUpdate:=False;
    ws:=GetWindowSize;
    if (ws.Width<>Width+1) or (ws.Height<>Height+1) then
    begin
      FCanvas.Resize(ws.Width, ws.Height-1);
      FHeight:=ws.Height-1;
      FWidth:=ws.Width;
      Resize;
      f:=True;
      FCanvas.Clear;
      FCanvas.SetColor(FBackground, FBackground);
      FCanvas.Rectangle(0,0, Width,Height);
    end;
    for i :=0 to FControls.Count-1 do
      FControls[i].Update(f);
    FCanvas.Print(f);
    inp:=ReadSequence;
    f:=False;
    if FUserControls.Count>0 then
      if FUserControls[FTabPosition].ProcessInput(inp) then
      begin
        inp:='';
        f:=True;
      end;
    if not f then ProcessInput(inp)
  until Closed;
end;

procedure TTextForm.Add(Ctrl: TTextControl);
begin
  FControls.Add(Ctrl);
  if Ctrl is TUserControl then
  begin
    if FUserControls.Count=0 then (Ctrl as TUserControl).SetFocused(True);
    FUserControls.Add(Ctrl as TUserControl);
   end;
end;

procedure TTextForm.Remove(Ctrl: TTextControl);
var
  i: Integer;
begin
  FControls.FreeObjects:=False;
  try
  FControls.Remove(Ctrl);
  finally
    FControls.FreeObjects:=True;
  end;
  if Ctrl is TUserControl then
  begin
    for i:=0 to FUserControls.Count-1 do
      if FUserControls[i] = Ctrl then
      begin
        if i > FTabPosition then
           dec(FTabPosition)
        else if i = FTabPosition then
          FUserControls[i+1].Focused:=True;
        FUserControls.Delete(i);
        Break;
      end;
  end;
end;

{ TUserControl }

procedure TUserControl.SetFBG(AValue: TColor);
begin
  if FFocusedBG=AValue then Exit;
  FFocusedBG:=AValue; 
  ColorChange;
  FChanged:=FFocused;
end;

procedure TUserControl.SetFFG(AValue: TColor);
begin
  if FFocusedFG=AValue then Exit;
  FFocusedFG:=AValue; 
  ColorChange;
  FChanged:=FFocused;
end;

procedure TUserControl.SetFocused(AValue: Boolean);
begin
  if FFocused=AValue then Exit;
  FFocused:=AValue;
  FChanged:=True;
  if AValue then Parent.SetFocus(Self);
  FocusChanged;
end;

procedure TUserControl.Draw(ACanvas: TTextCanvas);
var
  fg, bg: TColor;
begin
  if Focused then
  begin
    fg:=Foreground;
    Foreground:=FocusedForeground;
    bg:=Background;
    Background:=FocusedBackground;
  end;
  inherited Draw(ACanvas);  
  if Focused then
  begin
    Foreground:=fg;
    Background:=bg;
  end;
end;

procedure TUserControl.FocusChanged;
begin
  //NOOP
end;

{ TTextControl }

procedure TTextControl.SetBG(AValue: TColor);
begin
  if FBackground=AValue then Exit;
  FBackground:=AValue;
  FChanged:=True;    
  ColorChange;
end;

function TTextControl.GetCanvas: TTextCanvas;
begin
  Result:=Parent.Canvas;
end;

procedure TTextControl.SetFG(AValue: TColor);
begin
  if FForeground=AValue then Exit;
  FForeground:=AValue;
  FChanged:=True;
  ColorChange;
end;

procedure TTextControl.SetHeight(AValue: Integer);
begin
  if FHeight=AValue then Exit;
  FHeight:=AValue;
  Resize;
  FChanged:=True;
end;

procedure TTextControl.SetLeft(AValue: Integer);
begin
  if FLeft=AValue then Exit;
  FLeft:=AValue;
  FChanged :=True;
  PositionChanged;
end;

procedure TTextControl.SetParent(AValue: TTextForm);
begin
  if FParent=AValue then Exit;
  if Assigned(FParent) then
    FParent.Remove(Self);
  FParent:=AValue;
  if Assigned(FParent) then
    FParent.Add(Self);
  FChanged:=True;
end;

procedure TTextControl.SetTop(AValue: Integer);
begin
  if FTop=AValue then Exit;
  FTop:=AValue;
  FChanged:=True;
  PositionChanged;
end;

procedure TTextControl.SetWidth(AValue: Integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
  Resize;
  FChanged:=True;
end;

procedure TTextControl.Draw(ACanvas: TTextCanvas);
begin
  ACanvas.SetColor(Background, Background);
  ACanvas.Rectangle(Left,Top,Width,Height);
  ACanvas.SetColor(Foreground, Background);
end;

procedure TTextControl.Resize;
begin
  //NOOP
end;

procedure TTextControl.ColorChange;
begin
  //NOOP
end;

procedure TTextControl.PositionChanged;
begin

end;

constructor TTextControl.Create(AParent: TTextForm);
begin
  FChanged:=True;
  Parent:=AParent;
end;

destructor TTextControl.Destroy;
begin
  inherited Destroy;
end;

procedure TTextControl.Update(FullRepaint: Boolean);
begin
  if not (FullRepaint or FChanged) then Exit;
  Draw(Canvas);
  FChanged:=False;
end;

end.

