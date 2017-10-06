unit TFControls;
           
{$Include defines.inc}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TFTypes, TFCanvas, fgl;

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
  protected
    procedure Resize; virtual; abstract;   
    function ProcessChar(c: Char): Boolean; virtual;
  public
    procedure Close;
    constructor Create(ACanvas: TTextCanvas);
    destructor Destroy; override;
    function Show: Boolean;
    procedure Add(Ctrl: TTextControl);
    procedure Remove(Ctrl: TTextControl);
    property Canvas: TTextCanvas read FCanvas;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Closed: Boolean read FClosed write FClosed;
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
  public
    constructor Create(AParent: TTextForm);virtual;
    destructor Destroy; override;
    procedure Update(FullRepaint: Boolean = false);
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
  public
    function ProcessChar(c: Char): Boolean; virtual; abstract;
    property Focused: Boolean read FFocused write SetFocused;
    property FocusedForeground: TColor read FFocusedFG write SetFFG;
    property FocusedBackground: TColor read FFocusedBG write SetFBG;
  end;
implementation

{ TTextForm }

function TTextForm.ProcessChar(c: Char): Boolean;
begin
  Result := (c = #27) and (ReadChar(False)=#0);
  if Result then
    Close;
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
  FWidth:=ACanvas.Width;
  FHeight:=ACanvas.Height;
end;

destructor TTextForm.Destroy;
begin
  FControls.Free;
  FUserControls.Free;
  inherited Destroy;
end;

function TTextForm.Show: Boolean;
var c: Char;
  i: Integer;
  ws: TWindowSize;
  f: Boolean;
begin
  Closed:=False;
  repeat 
    f:=False;
    ws:=GetWindowSize;
    if (ws.Width<>Width+1) or (ws.Height<>Height+1) then
    begin
      FCanvas.Resize(ws.Width-1, ws.Height-1);
      FHeight:=ws.Height-1;
      FWidth:=ws.Width-1;
      Resize;
      f:=True;
      FCanvas.Clear;
    end;
    for i :=0 to FControls.Count-1 do
      FControls[i].Update(f);
    FCanvas.Print(f);
    c:=ReadChar();
    if FUserControls.Count>0 then
    if c = #9 then
    begin
      FUserControls[FTabPosition].SetFocused(False);
      FTabPosition:=(FTabPosition+1) mod FUserControls.Count;
      FUserControls[FTabPosition].SetFocused(True);
    end
    else if FUserControls[FTabPosition].ProcessChar(c) or
      ProcessChar(c) then
      c := #0
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
  FChanged:=FFocused;
end;

procedure TUserControl.SetFFG(AValue: TColor);
begin
  if FFocusedFG=AValue then Exit;
  FFocusedFG:=AValue;
  FChanged:=FFocused;
end;

procedure TUserControl.SetFocused(AValue: Boolean);
begin
  if FFocused=AValue then Exit;
  FFocused:=AValue;
  FChanged:=True;
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

{ TTextControl }

procedure TTextControl.SetBG(AValue: TColor);
begin
  if FBackground=AValue then Exit;
  FBackground:=AValue;
  FChanged:=True;
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

