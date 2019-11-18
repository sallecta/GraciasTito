unit uDocDxf;

{$mode objfpc}{$H+}
interface

uses
  Classes, Graphics, LCLProc, fgl,
  uDraw, uDocCore,
  glob
  ;

type
  //ObjType of graphic entity
  TDxfPrimitive = (
    dxfLine, dxfCircle, dxfPolyline, dxfBlock
    );

  TDxf = class;
  TDxf_list = specialize TFPGObjectList<TDxf>;
  { TMyObject }
  TMyObject = class(TDocCoreObject)  //graphic object that we will draw
    procedure Draw; override;  //Draw the graphic object
    constructor Create(argCanvas: TDrawCanvas); override;
  private
    procedure RelocateElements;
  end;

  { TDxf }
  {The object is defined to be compatible with DXF files.}
  TDxf = class(TDocCoreObject)
  private
    pc0, pc1, pcM: TDocCoreControlPoint;
    procedure ControlPoint_0_Move(x_vPoint, y_vPoint, dx, dy: single);
    procedure ControlPoint_1_Move(x_vPoint, y_vPoint, dx, dy: single);
    procedure ControlPoint_M_Move(x_vPoint, y_vPoint, x_dv, y_dv: single);
  public
    DxfPrimitive: TDxfPrimitive;
    dxfId: string;        //identifier of the primitive
    layer: string;
    color: string;
    style: string;
    isComplex: boolean;
    polyFlag: integer; {Flag for polylines. Bitmap, zero by default}
    P0: TDrawPoint2D;
    P1: TDrawPoint2D;
    radius: double;
    vertexs: TDxf_list;   {Vertex list. Only instance for DocCoreObjects
                                complex. It is very heavy to keep a list of
                                TDxf. It should be optimized}
    blkName: string;    //used when it is from Type dxfBlock.
  public
    procedure SetP0(const xv, yv, zv: single);
    procedure SetP1(const xv, yv, zv: single);
    procedure RelocateElements;
  public
    procedure Draw; override;  //Draw the graphic object
    function isSelected(xp, yp: integer): boolean; override;
    constructor Create(argCanvas: TDrawCanvas); override;
    //  destructor Destroy; override;
  end;

implementation

{ TMyObject }
constructor TMyObject.Create(argCanvas: TDrawCanvas);
begin
  inherited;
  ReconstructGeom;
  //It must be called after creating the control points to be able to locate them
  Name := msg.get('objectExample');
end;

procedure TMyObject.RelocateElements;
{Relocate elements, from the object. It is called when the position of the object is changed, with
or without change of dimensions. }
var
  x2: single;
begin
  inherited;
  x2 := x + Width;
end;

procedure TMyObject.Draw();
begin
  //Draw label
  self.Canvas.SetText(clWhite, 11, '', False);
  self.Canvas.Text(x + 2, Y + Height + 20, 0, Name);
  //shows a rectangle
  self.Canvas.SetPen(clWhite, 1, psSolid);
  self.Canvas.FillFixed(clBlack);
  self.Canvas.rectangXYr(x, y + 10, x + Width, y + Height);
  inherited;
end;

{ TDxf }
procedure TDxf.ControlPoint_0_Move(x_vPoint, y_vPoint, dx, dy: single);
begin
  P0.x := x_vPoint;
  P0.y := y_vPoint;
  ReconstructGeom;
end;

procedure TDxf.ControlPoint_1_Move(x_vPoint, y_vPoint, dx, dy: single);
begin
  P1.x := x_vPoint;
  P1.y := y_vPoint;
  ReconstructGeom;
end;

procedure TDxf.ControlPoint_M_Move(x_vPoint, y_vPoint, x_dv, y_dv: single);
begin
  ReconstructGeom;
end;

procedure TDxf.SetP0(const xv, yv, zv: single);
begin
  P0.x := xv;
  P0.y := yv;
  RelocateElements;
end;

procedure TDxf.SetP1(const xv, yv, zv: single);
begin
  P1.x := xv;
  P1.y := yv;
  RelocateElements;
end;

constructor TDxf.Create(argCanvas: TDrawCanvas);
begin
  inherited Create(argCanvas);
  {Note that the control points are static, although it may be better to create them
   only when the object is selected.  }
  pc0 := AddControlPoint(CPO_TOP_LEFT, @ControlPoint_0_Move);
  pc1 := AddControlPoint(CPO_TOP_LEFT, @ControlPoint_1_Move);
  pcM := AddControlPoint(CPO_TOP_LEFT, @ControlPoint_M_Move);
  ReconstructGeom;//Control points must be called after CreateIt to be able to locate them
  Name := msg.get('defaultObjName');
end;

procedure TDxf.RelocateElements;
begin
  //Locate control points
  pc0.PlaceAt(P0.x, P0.y);
  pc1.PlaceAt(P1.x,P1.y);
  pcM.PlaceAt((P0.x + P1.x) / 2, (P0.y + P1.y) / 2);
end;

procedure TDxf.Draw;
var
  ctrlPoint: TDocCoreControlPoint;
  Points: array of TPoint;
  i: integer;
begin
  if Marked and canHighlight then
    self.Canvas.SetPen(TColor($FF8000), 2, psSolid)
  else
    self.Canvas.SetPen(clWhite, 1);
  case DxfPrimitive of
    dxfLine: self.Canvas.Line(P0, P1);
  end;
  //---------------draw selection mark--------------
  if Selected then
    for ctrlPoint in ControlPoints do
      ctrlPoint.Draw//Draw control points
  ;
end;

function TDxf.isSelected(xp, yp: integer): boolean;
  {Custom version}
const
  DSEL = 5;
var
  a, b: single;
  dx, dy: Int16;
  //tolerance in pixels
begin
  if P0.xp = P1.xp then
  begin  //Vertical straight case
    if abs(P0.xp - xp) > DSEL then
      exit(False);  //exceeds horizontal distance
    if P0.yp = P1.yp then
      Result := (abs(P0.yp - yp) < DSEL)//Case of a point
    else
    begin //Common vertical line case
      if P0.yp > P1.yp then
        Result := (yp < P0.yp + DSEL) and (yp > P1.yp - DSEL)//above P0
      else
        Result := (yp < P1.yp + DSEL) and (yp > P0.yp - DSEL)//above P1
      ;
    end;
  end
  else if P0.xp < P1.xp then
  begin  //P0 on the left
    if xp < P0.xp - DSEL then
      exit(False);
    if xp > P1.xp + DSEL then
      exit(False);
    dx := P1.xp - P0.xp;   //Always positive
    dy := P1.yp - P0.yp;   //positive or negative
    if abs(dy) < dx then
      Result := abs((xp - P0.xp) * dy - (yp - P0.yp) * dx) < DSEL * dx
    else
    begin //abs (dy), is greater than dx
      Result := abs((xp - P0.xp) * dy - (yp - P0.yp) * dx) < DSEL * abs(dy);
    end;
  end
  else
  begin                        //P1 on the left
    if xp < P1.xp - DSEL then
      exit(False);
    if xp > P0.xp + DSEL then
      exit(False);
    dx := P0.xp - P1.xp;   //Always positive
    dy := P0.yp - P1.yp;   //positive or negative
    if abs(dy) < dx then
      Result := abs((xp - P1.xp) * dy - (yp - P1.yp) * dx) < DSEL * dx
    else
      Result := abs((xp - P1.xp) * dy - (yp - P1.yp) * dx) < DSEL *
        abs(dy){abs (dy), is greater than dx } ;
  end;
end;

end.
