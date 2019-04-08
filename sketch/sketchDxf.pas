{
Define graphic objects.
All of them must descend from TGraphicObj, so that they can be treated
by the "ogMotEdicion" engine}

unit sketchDxf;

{$mode objfpc}{$H+}
interface

uses
  Classes, Graphics, LCLProc, fgl,
  MotGraf3d, sketchCore;

type
  //ObjType of graphic entity
  TDxfPrimitive = (
    dxfLine, dxfCircle, dxfPolyline, dxfBlock
    );

  TDxf = class;
  TDxf_list = specialize TFPGObjectList<TDxf>;
  { TMyObject }
  TMyObject = class(TGraphicObj)  //graphic object that we will draw
    procedure Draw; override;  //Draw the graphic object
    constructor Create(argVirtScreen: TVirtScreen); override;
  private
    procedure RelocateElements; override;
  end;

  { TDxf }
  {The object is defined to be compatible with DXF files.}
  TDxf = class(TGraphicObj)
  private
    pc0, pc1, pcM: TControlPoint;
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
    polyFlag: integer; {Flag for polylines. Bitmap, zero by default:
    1 = This is a closed polyline (or a polygon mesh closed in the M direction).
    2 = Curve-fit vertices have been added.
    4 = Spline-fit vertices have been added.
    8 = This is a 3D polyline.
    16 = This is a 3D polygon mesh.
    32 = The polygon mesh is closed in the N direction.
    64 = The polyline is a polyface mesh.
    128 = The linetype pattern is generated continuously around the vertices of this polyline.}
    P0: TPoint3D;
    P1: TPoint3D;
    radius: double;
    vertexs: TDxf_list;   {Vertex list. Only instance for objects
                                complex. EYE!!! It is very heavy to keep a list of
                                TDxf. It should be optimized}
    blkName: string;    //used when it is from btnType dxfBlock.
  public
    procedure SetP0(const xv, yv, zv: single);
    procedure SetP1(const xv, yv, zv: single);
    procedure RelocateElements; override;
  public
    procedure Draw; override;  //Draw the graphic object
    function isSelected(xp, yp: integer): boolean; override;
    constructor Create(argVirtScreen: TVirtScreen); override;
    //  destructor Destroy; override;
  end;

implementation

{ TMyObject }
constructor TMyObject.Create(argVirtScreen: TVirtScreen);
begin
  inherited;
  ReconstructGeom;
  //It must be called after creating the control points to be able to locate them
  Name := 'miObjeto';
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
  //  VirtScreen.SetPen(clGray, 1);
  VirtScreen.SetText(clWhite, 11, '', False);
  VirtScreen.Texto(x + 2, Y + Height + 20, 0, Name);
  //shows a rectangle
  VirtScreen.SetPen(clWhite, 1, psSolid);
  VirtScreen.FillFixed(clBlack);
  VirtScreen.rectangXYr(x, y + 10, x + Width, y + Height, 0);
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
  P0.z := zv;
  RelocateElements;
end;

procedure TDxf.SetP1(const xv, yv, zv: single);
begin
  P1.x := xv;
  P1.y := yv;
  P1.z := zv;
  RelocateElements;
end;

constructor TDxf.Create(argVirtScreen: TVirtScreen);
begin
  inherited Create(argVirtScreen);
  {Note that the control points are static, although it may be better to create them
   only when the object is selected.  }
  pc0 := AddControlPoint(CPO_TOP_LEFT, @ControlPoint_0_Move);
  pc1 := AddControlPoint(CPO_TOP_LEFT, @ControlPoint_1_Move);
  pcM := AddControlPoint(CPO_TOP_LEFT, @ControlPoint_M_Move);
  ReconstructGeom;//Control points must be called after CreateIt to be able to locate them
  Name := 'Objeto';
end;

procedure TDxf.RelocateElements;
begin
  //Locate control points
  pc0.PlaceAt(P0);
  pc1.PlaceAt(P1);
  pcM.PlaceAt((P0.x + P1.x) / 2, (P0.y + P1.y) / 2, (P0.z + P1.z) / 2);
end;

procedure TDxf.Draw;
var
  ctrlPoint: TControlPoint;
  Points: array of TPoint;
  i: integer;
begin
  if Marked and canHighlight then
    VirtScreen.SetPen(TColor($FF8000), 2, psSolid)
  else
    VirtScreen.SetPen(clWhite, 1);
  case DxfPrimitive of
    dxfLine: VirtScreen.Line(P0, P1);
    //  dxfCircle: begin
    //      VirtScreen.Circulo(xv + ent.x0, y + ent.y0,
    //                  ent.radius);
    //    end;
{  dxfPolyline: begin
      //Por eficiencia, se dibuja la polilínea directamente del canvas
      SetLength(Points, vertexs.Count);   //dimensiona
      //transforma puntos
      for i:= 0 to vertexs.Count-1 do begin
        Points[i].x := VirtScreen.XPant(vertexs[i].x0);
        Points[i].y := VirtScreen.YPant(vertexs[i].y0);
      end;
      //VirtScreen.Canvas.Polygon(Points);   //dibuja
      VirtScreen.cv.Polyline(Points);
    end;}
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
  {It should not be necessary to update the screen coordinates of P0 and P1, since
   if this line was shown on the screen, it is because its coordinates were updated
   screen:
   VirtScreen.XYpant (P0);
   VirtScreen.XYpant (P1);
  }
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
      exit(False);  //escapes from limit
    if xp > P1.xp + DSEL then
      exit(False);  //escapes from limit
    //Simplify the comparison, seeing only a vertical distance
    //     a := (P1.yp - P0.yp)/(P1.xp - P0.xp);  //pendiente
    //     b := P0.yp - a*P0.xp;  //Define ecuación de la recta y=ax+b
    //     Result := abs(a*xp + b - yp) < DSEL;
    //Alternative form, without divisions
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
      exit(False);  //escapes from limit
    if xp > P0.xp + DSEL then
      exit(False);  //escapes from limit
    //Define equation of the line y = ax + b
    //     a := (P0.yp - P1.yp)/(P0.xp - P1.xp);  //pendiente
    //     b := P1.yp - a*P1.xp;
    //     Result := abs(a*xp + b - yp) < DSEL;
    dx := P0.xp - P1.xp;   //Always positive
    dy := P0.yp - P1.yp;   //positive or negative
    if abs(dy) < dx then
      Result := abs((xp - P1.xp) * dy - (yp - P1.yp) * dx) < DSEL * dx
    else
      Result := abs((xp - P1.xp) * dy - (yp - P1.yp) * dx) < DSEL *
        abs(dy)//abs (dy), is greater than dx
    ;
  end;
end;

end.
