{Define a "Graphic Engine" object, which allows you to draw on a virtual canvas,
of coordinates of type Single, that later will be transformed to coordinates of the
screen (in pixels).
The coordinate system follows the usual direction in geometry:

Y  /|\
    |
    |
    |
    |
    +--------------------> X

With the exception that there is a Z coordinate that rises perpendicular to the XY plane,
in the line of View.
By changing the angles ALfa and Fi, it is possible to obtain a perspective view or
projection similar to the isometric.
}
unit MotGraf3d;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FPCanvas, Graphics, ExtCtrls, Controls;
type
  //Represents a three-dimensional point
  TPoint3D = record
    x,y,z : single;
    xp,yp : Int16;
  end;

  { TVirtScreen }
  TVirtScreen = class
  private
    fAlfa: Single;
    fFi: Single;
    fZoom    : Single;
    sena, cosa: Single;   //sin (Alpha) and cos (Alpha)
    seni, cosi: Single;   //sin (Fi) and cos (Fi)
    gControl: TGraphicControl;   //Graphic control, where you are going to draw
    cv      : Tcanvas;           //reference to the canvas
    function GetPenColor: TColor;
    procedure SetPenColor(AValue: TColor);
    procedure Set_Alfa(AValue: Single);
    procedure Set_Fi(AValue: Single);
    procedure SetZoom(AValue: Single);
  public  //Parameters of the camera (perspective)
    x_cam   : Single;  //coordinates of the camera
    y_cam   : Single;
    {Scroll to locate the virtual center of the screen (0,0)
     It is indicated in pixels. If for example, it is fixed:
     x_offs = 10 and y_offs = 10
     It will make that when you draw something virtually in (0,0), it will appear displaced
     10 pixels to the right of the left edge and 10 pixels to the top of the bottom edge}
    x_offs      : integer;
    y_offs      : Integer;
    OnChangeView: procedure of object;  //Si se produce cambio de la perspectiva
    property Alfa: Single read fAlfa write Set_Alfa;  //Ángulo ALFA
    Property Fi: Single read fFi write Set_Fi;        //Ángulo FI
    Property Zoom: Single read fZoom write SetZoom;
  public    //Funciones de transformación
    function XPant(xv, yv, zv: Single): Integer; inline;
    function YPant(xv, yv, zv: Single): Integer; inline;
    procedure XYpant(xv, yv, zv: Single; var xp, yp: Integer);
    procedure XYpant(var P: TPoint3D);
    function Xvirt(xp, yp: Integer): Single; inline;
    function Yvirt(xp, yp: Integer): Single; inline;
    procedure XYvirt(xp, yp: Integer; zv: Single; var xv, yv: Single);
    procedure Displace(dx, dy: Integer);
    procedure ObtenerDesplazXY(xp, yp: Integer; Xprev, Yprev: Integer; var dx,dy: Single);
  public  //Configuración
    property PenColor: TColor read GetPenColor write SetPenColor;
    procedure SetPen(color: Tcolor; ancho: Integer; estilo: TFPPenStyle = psSolid);
//    procedure FijaLapiz(ancho: Integer; color: Tcolor; estilo: TFPPenStyle);
    procedure FillFixed(ColorR:TColor);
    procedure FijaColor(colLin,colRel:TColor; ancho: Integer = 1); //Fija colorde línea y ColorFill
  public //Funciones de dibujo
    backColor: TColor;
    procedure Clear;
    procedure Line(const x1, y1, z1, x2, y2, z2: Double);
    procedure Line(var P1, P2: TPoint3D);
    procedure rectangXY(x1, y1: Single; x2, y2: Single; z: Single);
    procedure rectangXYr(x1, y1: Single; x2, y2: Single; z: Single);
    procedure poligono3(x1, y1, z1: Single; x2, y2, z2: Single; x3, y3,
      z3: Single; x4: Single=-10000; y4: Single=-10000; z4: Single=-10000;
      x5: Single=-10000; y5: Single=-10000; z5: Single=-10000; x6: Single=-
      10000; y6: Single=-10000; z6: Single=-10000);
    procedure polilinea3(x1, y1, z1: Single; x2, y2, z2: Single; x3, y3,
      z3: Single; x4: Single=-10000; y4: Single=-10000; z4: Single=-10000;
      x5: Single=-10000; y5: Single=-10000; z5: Single=-10000; x6: Single=-
      10000; y6: Single=-10000; z6: Single=-10000);
    procedure rectang0(x1, y1, x2, y2: Integer);
    procedure Barra0(x1, y1, x2, y2: Integer; colFon: TColor);
    //Funciones de texto
    procedure SetFont(Letra: string);
    procedure SetText(color: TColor; tam: single);
    procedure SetText(negrita: Boolean=False; cursiva: Boolean=False;
      subrayado: Boolean=False);
    procedure SetText(color: TColor; tam: single; Letra: String;
      negrita: Boolean=False; cursiva: Boolean=False; subrayado: Boolean=False);
    procedure Texto(x1, y1, z1: Single; txt: String);
    procedure TextoR(x1, y1, ancho, alto: Single; txt: String);
    procedure TextRect(x1, y1, x2, y2: Single; x0, y0: Single;
      const Text: string; const Style: TTextStyle);
    function TextWidth(const txt: string): single;
    procedure SetWindow(ScaleWidth, ScaleHeight: Real; xMin, xMax, yMin,
      yMax: Real);
  public  //Inicialización
    constructor Create(gContrl0: TGraphicControl);
    destructor Destroy; override;
  end;

implementation
const
  ZOOM_MIN = 0.05;
  ZOOM_MAX = 50;
procedure TVirtScreen.Clear;
begin
  gControl.Canvas.Brush.Color := backColor;
  gControl.Canvas.FillRect(0,0,gControl.Width,gControl.Height);
end;
//Funciones de transformación
//Las siguientes funciones son por así decirlo, "estandar".
//Cuando se creen otras clases de dispositivo interfase gráfica deberían tener también estas
//funciones que son siempre necesarias.
procedure TVirtScreen.Set_Alfa(AValue: Single);
begin
  if fAlfa=AValue then Exit;
  fAlfa:=AValue;
  //Realiza el cálculo una sola vez, por velocidad
  sena := sin(fAlfa);
  cosa := cos(fAlfa);
  if OnChangeView<>nil then OnChangeView;
end;
procedure TVirtScreen.Set_Fi(AValue: Single);
begin
  if fFi=AValue then Exit;
  fFi:=AValue;
  //Realiza el cálculo una sola vez, por velocidad
  seni := sin(fFi);
  cosi := cos(fFi);
  if OnChangeView<>nil then OnChangeView;
end;
procedure TVirtScreen.SetZoom(AValue: Single);
begin
  if fZoom=AValue then Exit;
  if AValue<ZOOM_MIN then exit;
  if AValue>ZOOM_MAX then exit;
  fZoom:=AValue;
  if OnChangeView<>nil then OnChangeView;
end;
function TVirtScreen.XPant(xv, yv, zv: Single): Integer;   //INLINE Para acelerar las llamadas
//Función de la geometría del motor. Da la transformación lineal de la coordenada x.
//Obtiene el punto X en la pantalla donde realmente aparece un punto X,Y,Z
var
  x2c: ValReal;
begin
//  Result := Round(
//              (xv) * zoom + x_offs
//            );
  x2c := (xv - x_cam) * cosa - (yv - y_cam) * sena;
  Result:= Round(x_offs + x2c * fZoom);
end;

function TVirtScreen.YPant(xv, yv, zv: Single): Integer;  //INLINE Para acelerar las llamadas
//Función de la geometría del motor. Da la transformación lineal de la coordenada y.
//Obtiene el punto Y en la pantalla donde realmente aparece un punto X,Y,Z
var
  y2c: ValReal;
begin
//  Result := Round(gControl.Height-(
//              (yv) * zoom + y_offs
//            ));
  y2c := ((yv - y_cam) * cosa + (xv - x_cam) * sena) * cosi + zv * seni;
  Ypant := Round(gControl.Height - (y_offs + y2c * fZoom));
end;
procedure TVirtScreen.XYpant(xv, yv, zv: Single; var xp, yp: Integer);
begin
  xp := XPant(xv, yv, zv);
  yp := YPant(xv, yv, zv);
end;
procedure TVirtScreen.XYpant(var P: TPoint3D);
{Actualiza las coordenadas xp, yp  de un registro TPoint3D}
begin
  P.xp := XPant(P.x, P.y, P.z);
  P.yp := YPant(P.x, P.y, P.z);
end;
function TVirtScreen.Xvirt(xp, yp: Integer): Single;
//Obtiene la coordenada X virtual (del punto X,Y,Z ) a partir de unas coordenadas de
//pantalla.
var
  x2c, y2c: Single;
begin
  x2c := (xp - x_offs) / fZoom;
  y2c := (gControl.Height - yp - y_offs) / fZoom;
  //caso z= 0, con inclinación. Equivalente a seleccionar en el plano XY
  Xvirt := (x2c * cosa * cosi + sena * y2c) / cosi + x_cam;
end;
function TVirtScreen.Yvirt(xp, yp: Integer): Single;
//Obtiene la coordenada Y virtual (del punto X,Y,Z ) a partir de unas coordenadas de
//pantalla.
var
  x2c, y2c: Single;
begin
  x2c := (xp - x_offs) / fZoom;
  y2c := (gControl.Height - yp - y_offs) / fZoom;
  //caso z= 0, con inclinación. Equivalente a seleccionar en el plano XY
  Yvirt := (cosa * y2c - x2c * sena * cosi) / cosi + y_cam;
end;
procedure TVirtScreen.XYvirt(xp, yp: Integer; zv: Single; var xv, yv: Single);
//Devuelve las coordenadas virtuales xv,yv a partir de unas coordenadas de pantalla
//(o del ratón). Debe indicarse el value de Z. Equivale a intersecar un plano
//paralelo al plano XY con la línea de mira del ratón en pantalla.
var
  x2c, y2c : Single;
begin
  x2c := (xp - x_offs) / fZoom;
  y2c := (gControl.Height - yp - y_offs) / fZoom;
  //Para ser legales, debería haber protección para cos(fi) = 0
  if zv = 0 then begin  //fórmula simplificada
      xv := (x2c * cosa * cosi + sena * y2c) / cosi + x_cam;
      yv := (cosa * y2c - x2c * sena * cosi) / cosi + y_cam;
  end else begin //para cualquier plano paralelo a XY
      xv := (x2c * cosa * cosi + sena * (y2c - zv * seni)) / cosi + x_cam;
      yv := (cosa * (y2c - zv * seni) - x2c * sena * cosi) / cosi + y_cam;
  end;
  //Si los ángulos de View alfa y fi son cero (caso normal), bastaría con
  //xv = x2c + x_cam
  //yv = y2c + y_cam
end;
procedure TVirtScreen.Displace(dx, dy: Integer);
//Desplaza el escenario (el punto de rotación siempre está en el centro de la pantalla)
begin
   //desplazamineto en y
   y_cam := y_cam - dy * cosa;
   x_cam := x_cam - dy * sena;
   //desplazamiento en x
   x_cam := x_cam - dx * cosa;
   y_cam := y_cam + dx * sena;
end;
procedure TVirtScreen.ObtenerDesplazXY(xp, yp: Integer; Xprev, Yprev: Integer;
  var dx, dy: Single);
{Obtiene los desplazamientos dx, dy virtuales, para los objects gráficos en base a
los movimientos del ratón.
Esta es otra de las funciones importantes, que se usa para el control de la
pantalla, con el movimiento del ratón.}
var
  dx0, dy0: Single;
begin
   //Desplazamiento en plano XY  (z=0)
   dx0 := (xp - Xprev) / fZoom;   //notar que no se toman en cuenta las constantes
   dy0 := -(yp - Yprev) / fZoom;
   dx := (dx0 * cosa * cosi + sena * dy0) / cosi;
   dy := (cosa * dy0 - dx0 * sena * cosi) / cosi;
end;
//Configuración
procedure TVirtScreen.SetPenColor(AValue: TColor);
begin
  cv.Pen.Color:=AValue;
end;
function TVirtScreen.GetPenColor: TColor;
begin
  Result := cv.Pen.Color;
end;
procedure TVirtScreen.SetPen(color: Tcolor; ancho: Integer; estilo: TFPPenStyle);
//Establece el lápiz actual de dibujo
begin
  cv.pen.Color := color;
  cv.pen.Width := ancho;
  cv.Pen.Style := estilo;
end;
//procedure TVirtScreen.FijaLapiz(ancho: Integer; color: Tcolor; estilo: TFPPenStyle
//  );
////Establece el lápiz actual de dibujo
//begin
//   cv.Pen.Style := estilo;
//   cv.pen.Width := ancho;
//   cv.pen.Color := color;
//end;
procedure TVirtScreen.FillFixed(ColorR: TColor);
//Establece el ColorFill actual
begin
   cv.Brush.Style := bsSolid;  //estilo sólido
   cv.Brush.Color:=ColorR;
end;
procedure TVirtScreen.FijaColor(colLin, colRel: TColor; ancho: Integer);
//Fija un color de línea y un color de ColorFill. La línea se fija a estilo sólido
//y el ColorFill también
begin
    cv.Pen.Style := psSolid;
    cv.pen.Width := ancho;
    cv.pen.Color := colLin;

    cv.Brush.Style:=bsSolid;
    cv.Brush.Color:=colRel;
end;
//Funciones de dibujo
procedure TVirtScreen.Line(const x1, y1, z1, x2, y2, z2: Double);
begin
  cv.Line(XPant(x1, y1, z1), YPant(x1, y1, z1),
          XPant(x2, y2, z2), YPant(x2, y2, z2));
end;
procedure TVirtScreen.Line(var P1, P2: TPoint3D);
begin
 XYpant(P1);   //actualiza coordenadas de pantalla
 XYpant(P2);   //actualiza coordenadas de pantalla
 cv.Line(P1.xp, P1.yp, P2.xp, P2.yp);
end;
procedure TVirtScreen.rectangXY(x1, y1: Single; x2, y2: Single; z: Single);
//Dibuja un rectángulo, paralelo al plano XY
begin
  //Se pasa un punto más a la polilínea.
 polilinea3(x1, y1, z, x2, y1, z, x2, y2, z, x1, y2, z, x1, y1, z);
End;
procedure TVirtScreen.rectangXYr(x1, y1: Single; x2, y2: Single; z: Single);
//Dibuja un rectángulo ColorFill, paralelo al plano XY
begin
 poligono3(x1, y1, z, x2, y1, z, x2, y2, z, x1, y2, z);
End;
procedure TVirtScreen.poligono3(x1,y1,z1: Single;
                  x2,y2,z2: Single;
                  x3,y3,z3: Single;
                  x4: Single = -10000; y4: Single = -10000; z4: Single = -10000;
                  x5: Single = -10000; y5: Single = -10000; z5: Single = -10000;
                  x6: Single = -10000; y6: Single = -10000; z6: Single = -10000);
//Dibuja un polígono ColorFill en 3D..
var
  Ptos3: array[1..7] of TPoint3D;     //puntos 3d
  Points: array[1..7] of TPoint;    //arreglo de puntos a Draw
  nptos : integer;
  x1c, y1c : integer;
  i : integer;
begin
 Ptos3[1].x := x1; Ptos3[1].y := y1; Ptos3[1].z := z1;
 Ptos3[2].x := x2; Ptos3[2].y := y2; Ptos3[2].z := z2;
 Ptos3[3].x := x3; Ptos3[3].y := y3; Ptos3[3].z := z3;
 nptos := 3;
 If x4 <> -10000 Then begin Ptos3[4].x := x4; Ptos3[4].y := y4; Ptos3[4].z := z4; nptos := 4; end;
 If x5 <> -10000 Then begin Ptos3[5].x := x5; Ptos3[5].y := y5; Ptos3[5].z := z5; nptos := 5; end;
 If x6 <> -10000 Then begin Ptos3[6].x := x6; Ptos3[6].y := y6; Ptos3[6].z := z6; nptos := 6; end;
 //transformación 3d
 For i := 1 To nptos  do begin
     x1c := XPant(Ptos3[i].x, Ptos3[i].y, Ptos3[i].z);
     y1c := YPant(Ptos3[i].x, Ptos3[i].y, Ptos3[i].z);
     Points[i].x := x1c;
     Points[i].y := y1c;
 end;
 cv.Polygon(@Points[1], nptos);   //dibuja borde
end;

procedure TVirtScreen.polilinea3(x1, y1, z1: Single; x2, y2, z2: Single; x3, y3,
  z3: Single; x4: Single; y4: Single; z4: Single; x5: Single; y5: Single;
  z5: Single; x6: Single; y6: Single; z6: Single);
//Dibuja un polígono sin rellenar en 3D..
var
  Ptos3: array[1..7] of TPoint3D;     //puntos 3d
  Points: array[1..7] of TPoint;    //arreglo de puntos a Draw
  nptos : integer;
  x1c, y1c : integer;
  i : integer;
begin
 Ptos3[1].x := x1; Ptos3[1].y := y1; Ptos3[1].z := z1;
 Ptos3[2].x := x2; Ptos3[2].y := y2; Ptos3[2].z := z2;
 Ptos3[3].x := x3; Ptos3[3].y := y3; Ptos3[3].z := z3;
 nptos := 3;
 If x4 <> -10000 Then begin Ptos3[4].x := x4; Ptos3[4].y := y4; Ptos3[4].z := z4; nptos := 4; end;
 If x5 <> -10000 Then begin Ptos3[5].x := x5; Ptos3[5].y := y5; Ptos3[5].z := z5; nptos := 5; end;
 If x6 <> -10000 Then begin Ptos3[6].x := x6; Ptos3[6].y := y6; Ptos3[6].z := z6; nptos := 6; end;
 //transformación 3d
 For i := 1 To nptos  do begin
     x1c := XPant(Ptos3[i].x, Ptos3[i].y, Ptos3[i].z);
     y1c := YPant(Ptos3[i].x, Ptos3[i].y, Ptos3[i].z);
     Points[i].x := x1c;
     Points[i].y := y1c;
 end;
 cv.Polyline(@Points[1], nptos);   //dibuja borde
end;
procedure TVirtScreen.rectang0(x1, y1, x2, y2: Integer);
//Dibuja un rectángulo sin "transformación"
begin
    cv.Frame(x1, y1, x2, y2);
End;
procedure TVirtScreen.Barra0(x1, y1, x2, y2: Integer; colFon: TColor);
//Rellena un área rectangular, no rellena el borde derecho e inferior.
//Es más rápido que rellenar con Rectangle()
begin
    cv.Brush.Color := colFon;
    cv.FillRect(x1,y1,x2,y2); //fondo
end;
//funciones para texto
procedure TVirtScreen.SetFont(Letra: string);
//Permite definir el btnType de letra actual
begin
  if Letra = '' then cv.Font.Name:= 'MS Sans Serif';
  //'Times New Roman'
end;
procedure TVirtScreen.SetText(color: TColor; tam: single);
//método sencillo para cambiar propiedades del texto
begin
   cv.Font.Color := color;
   cv.Font.Size := round(tam * fZoom);
end;
procedure TVirtScreen.SetText(negrita:Boolean = False; cursiva: Boolean = False;
            subrayado: Boolean = False);
//Establece las características completas del texto
begin
   cv.Font.Bold := negrita;
   cv.Font.Italic := cursiva;
   cv.Font.Underline := subrayado;
End;
procedure TVirtScreen.SetText(color: TColor; tam: single; //; nDegrees As Single, _
            Letra: String;
            negrita:Boolean = False;
            cursiva: Boolean = False;
            subrayado: Boolean = False);
//Establece las características completas del texto
begin
   cv.Font.Color := color;
   cv.Font.Size := round(tam * fZoom);
   if Letra <> '' then cv.Font.Name:=letra;
   cv.Font.Bold := negrita;
   cv.Font.Italic := cursiva;
   cv.Font.Underline := subrayado;
End;
procedure TVirtScreen.Texto(x1, y1, z1: Single; txt: String);
//Escribe un texto
begin
   cv.Brush.Style := bsClear;  //Fondo transparente
//   tmp := cv.Font.Size;  //guarda tamaño actual
//   cv.Font.Size := round(cv.Font.Size * Zoom);
   cv.TextOut(XPant(x1,y1,z1), YPant(x1,y1,z1), txt);
//   cv.Font.Size := tmp;  //restaura
//   cv.Brush.Style := bsSolid;  //devuelve estilo de fondo
End;
procedure TVirtScreen.TextRect(x1,y1,x2,y2: Single; x0, y0: Single; const Text: string;
                       const Style: TTextStyle);
//Escribe un texto
var
  Arect: TRect;
begin
   cv.Brush.Style := bsClear;  //Fondo transparente
//   tmp := cv.Font.Size;  //guarda tamaño actual
//   cv.Font.Size := round(cv.Font.Size * Zoom);
   ARect.Left   := XPant(x1,y1,0);
   ARect.Top    := YPant(x1,y1,0);
   ARect.Right  := XPant(x2,y2,0);
   ARect.Bottom := YPant(x2,y2,0);
   cv.TextRect(Arect, XPant(x0,y0,0), YPant(x0,y0,0), Text, Style);
//   cv.Font.Size := tmp;  //restaura
   cv.Brush.Style := bsSolid;  //devuelve estilo de fondo
End;
procedure TVirtScreen.TextoR(x1, y1, ancho, alto: Single; txt: String);
//Escribe un texto
var r:TRect;
    //s:TTextStyle;
begin
//    cv.Font.Color:=clred;
   cv.Brush.Style := bsClear;  //Fondo transparente
   cv.Font.Size := round(11 * fZoom);
   r.Left := XPant(x1,y1,0);
   r.Top := YPant(x1,y1,0);
   r.Right := XPant(x1+ancho,y1,0);     { TODO : Show como Draw texto no limitado }
   r.Bottom:= YPant(x1,y1+alto,0);
//   s.Alignment:=taRightJustify;  //alineado a la derecha
// cv.TextRect(r,r.Left,r.Top,txt,s);//No permite cambia el tamaño de letra!!!!
   cv.TextRect(r,r.Left,r.Top,txt);
   cv.Brush.Style := bsSolid;  //devuelve estilo de fondo
End;
function TVirtScreen.TextWidth(const txt: string): single;
begin
  Result := cv.TextWidth(txt) * fZoom;
end;
procedure TVirtScreen.SetWindow(ScaleWidth, ScaleHeight: Real;
               xMin, xMax, yMin, yMax: Real);
//Fija las coordenadas de pantalla de manera que se ajusten a las nuevas que se dan
//Recibe coordenadas virtuales
var zoomX: Real;
    zoomY: Real;
    dxcen: Real; //Desplazamiento en x para centrar
    dycen: Real; //Desplazamiento en y para centrar
begin
   If xMax <= xMin Then Exit;
   If yMax <= yMin Then Exit;
   //calcula el zoom por efecto de dX
   zoomX := ScaleWidth / (xMax - xMin);
   //calcula el zoom por efecto de dY
   zoomY := ScaleHeight / (yMax - yMin);
   //toma el zoom menor, en caso de relación de aspecto diferente de 1
   If zoomY > zoomX Then   //toma el zoom de x
      begin
        fZoom := zoomX;
        dxcen := 0;
        dycen := (ScaleHeight / fZoom - (yMax - yMin)) / 2;   //para centrar en vertical
      end
   Else  //zoomX > zoomy    ,toma el zoom de y
      begin
        fZoom := zoomY;
        dycen := 0;
        dxcen := (ScaleWidth / fZoom - (xMax - xMin)) / 2;   //para centrar en horizontal
      end;
   //fija las coordenadas de cámara
   x_cam := xMin + x_offs / fZoom - dxcen;
   y_cam := yMin + y_offs / fZoom - dycen;
End;

constructor TVirtScreen.Create(gContrl0: TGraphicControl);
begin
  gControl := gContrl0;
  cv := gControl.Canvas;
  //Punto de rotación, a 10 pixeles de la esquina
  x_offs := 10;
  y_offs := 10;
  fZoom := 1;
  Alfa:=1; Alfa:=0;   //para forzar a actualizar los ángulos
  Fi:=1; FI:=0;   //para forzar a actualizar los ángulos

  backColor := clWhite;
end;
destructor TVirtScreen.Destroy;
begin
  inherited Destroy;
end;

end.

