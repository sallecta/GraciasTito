{
VisGraf3D unit
=====================
By Tito Hinostroza 01/04/2017

Description
============
Defines the class TView3D that implements a viewer-editor of graphic objects.
To work you must associate with a PaintBox control (where the objects will appreciate)
graphics) and a list of objects of type TEditorObjList, defined in the unit
ogDefObjGraf.
This unit is based on the ogEditGraf library, where it would be equivalent to
the ogMotEdicion unit, with the difference that here, the main class, does not include
to the container of objects but only a referecncia, and that the focus here is on handling
of states before of events.
The idea is that this class provides a higher level layer on the graphic engine
to show and edit objects with the mouse and by commands.
In summary, the TView3D class must meet the following requirements:

1. Be associated with a list of objects "TEditorObjList". Does not include the container,
   but it is only a viewer-editor.
2. Must intercept mouse events, for editing. This is one of
   the main functions of this class.
3. This class must be in charge of executing the commands. This is another one of
   the main functions of this class.
4. Objects must only be modified through this class. They should not be done
   directly from outside. This will allow taking control of the actions for
   implement the Undo and Redo.
5. The changes in the objects must be informed, through the OnModify event.
6. This class must restrict access to the graphics engine. The idea is that it can adapt
   without problems to other graphics engines.

There are only two ways to interact from outside, with the viewer:

1. Through commands and data. Through the methods ExecuteCommand () and SendData ().
2. With Mouse events on the PaintBox.

There is not, nor should there be another form of interaction. The keys must be converted first
in commands or data, before sending them to this viewer.

The viewer communicates with the outside, through the events.

}
unit sketchEditor;
{$mode objfpc}{$H+}
INTERFACE
uses
  Classes, Controls, ExtCtrls, Graphics, LCLProc, LCLType, fgl,
  MotGraf3d, sketchCore, sketchDxf;
const
  CUR_DEFEC = crDefault;          //cursor por defecto

  ZOOM_MAX_CONSULT = 5  ;  //Define el zoom máximo que se permite en un diagrama
  ZOOM_MIN_CONSULT = 0.1;  //Define el zoom mínimo que se permite en un diagrama

  FACTOR_AMPLIA_ZOOM = 1.15;  //Factor de ampliación del zoom
  DESPLAZ_MENOR = 10;
type
  //Tipo de evento producido en la View
  TVisEventTyp = (
    vmeMouseDown,   //Botón del mouse pulsado
    vmeMouseMove,   //Botón desplazado
    vmeMouseUp,     //Botón del mouse soltado
    vmeEjecComm    //Inicio de comando
  );
  //Tipo del manejador de eventos de la View. Se espera solo eventos del mouse o de
  //comandos.
  TVisEventHandler = procedure(EventTyp: TVisEventTyp; Button: TMouseButton;
                            Shift: TShiftState; xp, yp: Integer; txt: string) of object;

  //Estados del puntero
  TViewState = (
      //Estados del editor visual
       EP_NORMAL      //No se está realizando ninguna operación
      ,EP_SELECMULT   //Esta en modo de selección múltiple
      ,EP_MOV_OBJS    //Indica que se esta moviendo una o mas objects
      ,EP_DESP_PANT   //desplazamiento de pantalla
      ,EP_DESP_ANG    //Indica desplazamiento de ángulos de View
      ,EP_DIMEN_OBJ   //Indica que se está dimensionando un objeto
      ,EP_RAT_ZOOM    //Indica que se está en un proceso de Zoom
      //Estados adciionales para los comandos
      ,EP_COMM_LINE
      ,EP_COMM_RECTAN
      );

  TOnClickDer = procedure(x,y:integer) of object;
  TEvChangeState = procedure(ViewState: TViewState) of object;
  TEvSendMessage = procedure(msg: string) of object;

  { TView3D }
  TView3D = class
  private
    FState: TViewState;
    procedure GraphicObjectAdd(argGraphicObject: TGraphicObj; AutoPos: boolean=true);
    procedure GraphicObjectDelete(obj: TGraphicObj);  //elimina un objeto grafico
    procedure DeleteSelected;
    procedure proc_COMM_RECTAN(EventTyp: TVisEventTyp; Button: TMouseButton;
      Shift: TShiftState; xp, yp: Integer; txt: string);
    procedure SetState(AValue: TViewState);
    procedure v2d_ChangeView;
  protected
    PBox         : TPaintBox;   //Control de Salida
    CapturoEvento: TGraphicObj;    //referencia a objeto que capturo el movimiento
    ultMarcado   : TGraphicObj;    //nombre del objeto marcado
    ParaMover    : Boolean;     //bandera de control para el inicio del movimiento
    procedure PBox_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
                        xp, yp: Integer); virtual;
    procedure PBox_MouseUp(Sender: TObject; Button: TMouseButton;Shift: TShiftState; xp, yp: Integer);
    procedure PBox_MouseMove(Sender: TObject; Shift: TShiftState; X,  Y: Integer); virtual;
    procedure PBox_Paint(Sender: TObject);
    procedure PBox_MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PBox_DblClick(Sender: TObject);
    procedure PBox_Resize(Sender: TObject);
  public  //Esta ruitna pertenece al grupo de eventos PBox_???, pero debe ser pública.
    procedure ExecuteCommand(command: string);
  public  //Manejo de eventos
    OnClickDer  : TOnClickDer;
    OnMouseUp   : TMouseEvent;          //cuando se suelta el botón
    OnMouseDown : TMouseEvent;
    OnMouseMove : TMouseMoveEvent;
    OnDblClick  : TNotifyEvent;
    OnObjectsMoved: procedure of object;
    OnChangeView: procedure of object;
    OnModify     : procedure of object;  //Este visor indica los cambios con este evento
    OnChangeState: TEvChangeState;      //Cambia el state del Visor
    OnSendMessage: TEvSendMessage;      //Envía un mensaje. Usado para respuesta a comandos
  public
    xvPtr       : Single;   //coordenadas cirtuales del puntero
    yvPtr       : Single;   //coordenadas cirtuales del puntero
    zvPtr       : Single;   //coordenadas cirtuales del puntero
    objects     : TEditorObjList; //referencia a la lisat de objects
    seleccion   : TEditorObjList;
    v2d         : TMotGraf;    //salida gráfica
    incWheel    : Single;      //Incremento de ámgulo con la rueda del mouse
    ShowAxes : boolean;     //Para mostrar los ejec coordenados.
    AxesDistance : integer;     //Longitud de ejes coordenados
    ShowRotPoint: boolean;     //Para mostrar el punto de giro.
    ShowGrid  : boolean;     //Para mostrar la cuadrícula.
    function Seleccionado: TGraphicObj;
    function ObjPorNombre(nom: string): TGraphicObj;
    procedure Refresh;
    procedure SeleccionarTodos;
    procedure SelectNone();
  protected
    x1Sel    : integer;
    y1Sel    : integer;
    x2Sel    : integer;
    y2Sel    : integer;
    x1Sel_a  : integer;
    y1Sel_a  : integer;
    x2Sel_a  : integer;
    y2Sel_a  : integer;
    //coordenadas del raton
    x_pulso: integer;
    y_pulso: integer;
    x_cam_a: Single;  //coordenadas anteriores de x_cam
    y_cam_a: Single;
    procedure AmpliarClick(factor: real=FACTOR_AMPLIA_ZOOM; xr: integer=0;
      yr: integer=0);
    function AnteriorVisible(c: TGraphicObj): TGraphicObj;
    procedure DibujRecSeleccion;

    function enRecSeleccion(X, Y: Single): Boolean;
    procedure InicRecSeleccion(X, Y: Integer);
    procedure moverAbajo(desp: Double=DESPLAZ_MENOR);
    procedure moverArriba(desp: Double=DESPLAZ_MENOR);
    procedure moverDerecha(desp: Double=DESPLAZ_MENOR);
    procedure moverIzquierda(desp: Double=DESPLAZ_MENOR);
    procedure Desplazar(dx, dy: integer);
    function NumeroVisibles: Integer;
    function PrimerVisible: TGraphicObj;
    function RecSeleccionNulo: Boolean;
    procedure ReducirClick(factor: Real=FACTOR_AMPLIA_ZOOM; x_zoom: Real=0;
      y_zoom: Real=0);
    function SeleccionaAlguno(xp, yp: Integer): TGraphicObj;
    procedure SeleccionarAnterior;
    procedure SeleccionarSiguiente;
    function SiguienteVisible(c: TGraphicObj): TGraphicObj;
    function UltimoVisible: TGraphicObj;
    function VerificarMovimientoRaton(X, Y: Integer): TGraphicObj;
    procedure VerificarParaMover(xp, yp: Integer);
  public  //Se hace público porque se necesita acceder desde fuera
    procedure GraphicObject_Select(obj: TGraphicObj);     //Respuesta a Evento
    procedure GraphicObject_Unselec(obj: TGraphicObj);    //Respuesta a Evento
    procedure GraphicObject_SetPointer(Punt: integer);  //Respuesta a Evento
  private  //Rutinas de procesamiento de estados
    {Contenedor que asocia el state a su procedimiento manejador. Se usar para acceder
     rápidamente a la rutina manejadora, ya que algunos eventos (como PBox_MouseMove), se
     generan de forma repetida.}
    EventOfState: array[low(TViewState) .. high(TViewState)] of TVisEventHandler;
    property State: TViewState read FState write SetState;  //state del puntero
    public function StateAsStr: string; private  //Cadena de descripción de state
    procedure RegisterState(State0: TViewState; EventHandler: TVisEventHandler);
    procedure ClearEventState;
    procedure SendData(Data: string);
    procedure CallEventState(State0: TViewState; EventTyp: TVisEventTyp;
      Button: TMouseButton; Shift: TShiftState; xp, yp: Integer; txt: string);
  private  //Manejadores de eventos de state
    procedure proc_COMM_LINE(EventTyp: TVisEventTyp; Button: TMouseButton;
                            Shift: TShiftState; xp, yp: Integer; txt: string);
    procedure proc_NORMAL(EventTyp: TVisEventTyp; Button: TMouseButton;
                            Shift: TShiftState; xp, yp: Integer; txt: string);
    procedure proc_SELECMULT(EventTyp: TVisEventTyp; Button: TMouseButton;
                            Shift: TShiftState; xp, yp: Integer; txt: string);
    procedure proc_MOV_OBJS(EventTyp: TVisEventTyp; Button: TMouseButton;
                            Shift: TShiftState; xp, yp: Integer; txt: string);
    procedure proc_DESP_PANT(EventTyp: TVisEventTyp; Button: TMouseButton;
                            Shift: TShiftState; xp, yp: Integer; txt: string);
    procedure proc_DESP_ANG(EventTyp: TVisEventTyp; Button: TMouseButton;
                            Shift: TShiftState; xp, yp: Integer; txt: string);
    procedure proc_DIMEN_OBJ(EventTyp: TVisEventTyp; Button: TMouseButton;
                            Shift: TShiftState; xp, yp: Integer; txt: string);
    procedure proc_RAT_ZOOM(EventTyp: TVisEventTyp; Button: TMouseButton;
                            Shift: TShiftState; xp, yp: Integer; txt: string);
  public //Inicialización
    procedure RestoreState(msg: string='');
    constructor Create(PB0: TPaintBox; objectList: TEditorObjList);
    destructor Destroy; override;
  end;

implementation
uses
  guiFormConfig;

procedure TView3D.SetState(AValue: TViewState);
begin
  if FState=AValue then Exit;
  FState:=AValue;
  if OnChangeState<>nil then OnChangeState(FState);
end;
procedure TView3D.v2d_ChangeView;
begin
  if OnChangeView<>nil then OnChangeView;
end;
procedure TView3D.PBox_MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; xp, yp: Integer);
begin
    if OnMouseDown<>nil then OnMouseDown(Sender, Button, Shift, Xp, Yp);
    x_pulso := xp;
    y_pulso := yp;
    //Prepara inicio de desplazamiento de la pantalla. Se debe hacer porque podría
    //iniciarse el proceso de desplazamiento.
    x_cam_a := v2d.x_cam;
    y_cam_a := v2d.y_cam;

    CallEventState(State, vmeMouseDown, Button, Shift, xp, yp, ''); //Procesa de acuerdo al state
end;
procedure TView3D.PBox_MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; xp, yp: Integer);
begin
   //Verifica si la selección es NULA
   If (State = EP_SELECMULT) And RecSeleccionNulo Then State := EP_NORMAL;
   CallEventState(State, vmeMouseUp, Button, Shift, xp, yp, ''); //Procesa de acuerdo al state
   if Button = mbRight then
     if OnClickDer<> nil then OnClickDer(xp,yp);  //evento
   if OnMouseUp<>nil then OnMouseUp(Sender, Button, Shift, xp, yp);
end;
procedure TView3D.PBox_MouseMove(Sender: TObject; Shift: TShiftState;
  X,  Y: Integer);
begin
  zvPtr := 0;   //fijamos el plano de trabajo en z=0
  v2d.XYvirt(X,Y,zvPtr, xvPtr, yvPtr);  {actualiza puntero virtual.}
  if OnMouseMove<>nil then OnMouseMove(Sender, Shift, X, Y);
  if ParaMover = True Then VerificarParaMover(X, Y);
  CallEventState(State, vmeMouseMove, mbExtra1, Shift, x, y, ''); //Procesa de acuerdo al state
end;
procedure TView3D.PBox_Paint(Sender: TObject);
var
  o:TGraphicObj;
  x, y, xCuad1, xCuad2, yCuad1, yCuad2: Single;
  nCuad, ix, distCub, paso: Integer;
begin
    v2d.Clear;
    If State = EP_SELECMULT Then DibujRecSeleccion;
    if ShowGrid then begin
      //Muestra cuadrícula
      v2d.SetPen(TColor($404040),1);
      if v2d.Zoom > 7 then begin
        distCub := 100;  //distancia cubierta (valor virtual)
        paso := 10;      //ancho del paso (valor virtual)
      end else if v2d.Zoom > 3 then begin
        distCub := 200;  //distancia cubierta (valor virtual)
        paso := 20;      //ancho del paso (valor virtual)
      end else if v2d.Zoom > 1 then begin
        distCub := 600;  //distancia cubierta (valor virtual)
        paso := 50;      //ancho del paso (valor virtual)
      end else begin
        distCub := 1200;  //distancia cubierta (valor virtual)
        paso := 100;      //ancho del paso (valor virtual)
      end;
      nCuad := distCub div paso;

//      xCuad1 := 0;
//      xCuad2 := 1000;
      xCuad1 := int((v2d.x_cam - distCub/2)/paso)*paso;
      xCuad2 := xCuad1 + distCub;

//      yCuad1 := 0;
//      yCuad2 := 1000;
      yCuad1 := int((v2d.y_cam - distCub/2)/paso)*paso;
      yCuad2 := yCuad1 + distCub;

      x := xCuad1;
      for ix := 0 to nCuad do begin
        v2d.Line(x,yCuad1,0, x, yCuad2, 0);
        x := x + paso;
      end;
      y := yCuad1;
      for ix := 0 to nCuad do begin
        v2d.Line(xCuad1, y, 0, xCuad2, y, 0);
        y := y + paso;
      end;
    end;
    //Dibuja objects
    for o In objects do begin
      o.Dibujar;
    end;
    //Dibuja eje
    if ShowAxes then begin
      v2d.SetPen(clRed, 1);
      v2d.Line(0,0,0,100,0,0);
      v2d.Line(0,0,0,0,100,0);
      v2d.Line(0,0,0,0,0,100);
      v2d.Texto(100,10,0,'x');
      v2d.Texto(0,100,0,'y');
    end;
    if ShowRotPoint then begin
      x := v2d.x_cam;
      y := v2d.y_cam;
      v2d.SetPen(clGreen, 1);
      v2d.Line(x-30,y,0,  x+30,y,0);
      v2d.Line(x, y-30,0, x, y+30,0);
    end;
    //Dibuja puntero del mouse  (No es apropiado porque necesita refescar siempre.)
//    v2d.SetPen(clWhite, 1);
//    v2d.Line(xvPtr-30, yvPtr, zvPtr,
//             xvPtr+30, yvPtr, zvPtr);
//    v2d.Line(xvPtr, yvPtr-30, zvPtr,
//             xvPtr, yvPtr+30, zvPtr);
//    v2d.Line(xvPtr, yvPtr, zvPtr-30,
//             xvPtr, yvPtr, zvPtr+30);
end;
procedure TView3D.PBox_MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  d: Single;
begin
  if Shift = [ssCtrl] then begin
    if WheelDelta>0 then d := incWheel else d := -incWheel;
    v2d.Alfa := v2d.Alfa + d;
  end;
  if Shift = [ssShift] then begin
    if WheelDelta>0 then d := incWheel else d := -incWheel;
    v2d.Fi := v2d.Fi + d;
  end;
  if Shift = [] then begin
    if WheelDelta>0 then v2d.Zoom:=v2d.Zoom*1.2
    else v2d.Zoom:=v2d.Zoom/1.2;
  end;
  Refresh;
end;
procedure TView3D.PBox_DblClick(Sender: TObject);
begin
  if OnDblClick<>nil then OnDblClick(Sender);
end;
procedure TView3D.PBox_Resize(Sender: TObject);
{Se aprovecha para fijar el punto de rotación al centro del control.}
begin
  v2d.x_offs := PBox.Width div 2;
  v2d.y_offs := PBox.Height div 2;
end;
procedure TView3D.ExecuteCommand(command: string);
{Solicita ejecutar, un comando al visor. Esta debe ser el úncio medio, además de los
eventos del ratón, por el cual se comunica acciones al visor. Visto de este modo,
ExecuteCommand(), es similar a las rutinas manejadores de eventos: PBOX_???(), con la
excepción de que no se ejecuta, como respuesta a un evento del mouse, sino que debe ser
llamado externamente. }
begin
  //Pasa el evento de comando, a la rutina correspondiente al state actual.
  //El único state que debería tratar este comando, sería el state NORMAL.
  CallEventState(State, vmeEjecComm, mbExtra1, [], 0, 0, command); //Procesa de acuerdo al state
end;

procedure TView3D.Refresh();  //   Optional s: TGraphicObj = Nothing
begin
  PBox.Invalidate;
end;
function TView3D.SeleccionaAlguno(xp, yp: Integer): TGraphicObj;
//Rutina principal para determinar la selección de objects. Si (xp,yp)
//selecciona a algún objeto, devuelve la referencia, sino devuelve "NIL"
var
  i: Integer;
  s: TGraphicObj;
begin
  //Verifica primero entre los que están seleccionados
  Result := NIL; //valor por defecto
  //Explora objects priorizando los que están encima
  For i := seleccion.Count-1 downTo 0 do begin
    s := seleccion[i];
    If not s.SelLocked and s.LoSelecciona(xp, yp) Then begin
        Result:= s;
        Exit;
    End;
  end;
  //Explora objects priorizando los que están encima
  For i := objects.Count-1 downTo 0 do begin
    s := objects[i];
    If not s.SelLocked and s.LoSelecciona(xp, yp) Then begin
        Result := s;
        Exit;
    End;
  end;
End;
procedure TView3D.VerificarParaMover(xp, yp: Integer);
{Si se empieza el movimiento, selecciona primero algun elemento que
pudiera estar debajo del puntero y actualiza "EstPuntero".
Solo se debe ejecutar una vez al inicio del movimiento, para ello se
usa la bandera ParaMover, que debe ponerse a FALSE aquí.}
var s: TGraphicObj;
begin
    for s In seleccion  do begin  //da prioridad a los elementos seleccionados
      if s.PosLocked then continue;
      s.StartMove(xp, yp);      //llama al evento inic_mover para cada objeto
      if s.Proceso Then begin  //este objeto proceso el evento
          CapturoEvento := s;
          if s.Resizing then State := EP_DIMEN_OBJ else State := EP_NORMAL;
          ParaMover := False;    //para que ya no se llame otra vez
          Exit;
      end;
    end;
    for s In objects do begin
      if s.PosLocked then continue;
      s.StartMove(xp, yp);    //llama al evento inic_mover para cada objeto
      if s.Proceso Then begin   //este objeto proceso el evento
          CapturoEvento := s;
          if s.Resizing then State := EP_DIMEN_OBJ else State := EP_NORMAL;
          State := EP_NORMAL;
          ParaMover := False;   //para que ya no se llame otra vez
          exit;
      end;
    end;
    //Ningún objeto ha capturado, el evento, asumimos que se debe realizar
    //el desplazamiento simple de los objects seleccionados
//Debug.Print "   VerifParaMover: EP_MOV_OBJS"
    State := EP_MOV_OBJS;
    CapturoEvento := nil;      //ningún objeto capturo el evento
    ParaMover := False;        //para que ya no se llame otra vez
End;
function TView3D.VerificarMovimientoRaton(X, Y: Integer): TGraphicObj;
//Anima la marcación de los objects cuando el ratón pasa encima de ellos
//Devuelve referencia al objeto por el que pasa el cirsor
var s: TGraphicObj;
begin

    s := SeleccionaAlguno(X, Y);    //verifica si selecciona a un objeto
    Result := s;  //devuelve referencia
//    If Not s = NIL Then
//        If s.Id = ID_CONECTOR Then  ;  //Or s.Seleccionado
//            Set s = Nothing  ;  //no válido para conectores
//        End If
//    End If
    //Se refresca la pantalla optimizando
    If s = NIL Then begin  //No hay ninguno por marcar
      If ultMarcado <> NIL Then begin
            //Si ya había uno marcado, se actualiza el dibujo y la bandera
            ultMarcado.Marcado := False;  //se desmarca
            ultMarcado := NIL;
            Refresh;
        End;
      PBox.Cursor := CUR_DEFEC;   //restaura cursor
    end
    Else begin   //Hay uno por marcar
      If ultMarcado = NIL Then begin
         //No había ninguno marcado
         ultMarcado := s;      //guarda
         s.Marcado := True;    //lo marca
         Refresh;            //y se dibuja
      end Else begin  //ya había uno marcado
           If ultMarcado = s Then  //es el mismo
               //no se hace nada
           Else begin    //había otro marcado
               ultMarcado.Marcado := False;  //se desmarca
               ultMarcado := s ;   //actualiza
               s.Marcado := True;
               Refresh;          //y se dibuja
           End;
        End;
    End;

End;
//***********Funciones para administrar los elementos visibles y seleccion por teclado**********
function TView3D.NumeroVisibles: Integer;
//devuelve el número de objects visibles
var
  v: TGraphicObj;
  tmp: Integer;
begin
  tmp := 0;
  For v in objects do begin
    if v.visible then Inc(tmp);
  end;
  Result := tmp;
end;
function TView3D.PrimerVisible: TGraphicObj;
 //devuelve el primer objeto visible
var
  i: integer;
begin
  for i:=0 to objects.Count-1 do begin
    if objects[i].visible then begin
      Result := objects[i];
      exit;
    end;
  end;
End;
function TView3D.UltimoVisible: TGraphicObj;
 //devuelve el último objeto visible
var
  i: Integer;
begin
  for i:=objects.Count-1 downto 0 do begin
    if objects[i].visible then begin
      Result := objects[i];
      exit;
    end;
  end;
end;
function TView3D.SiguienteVisible(c: TGraphicObj): TGraphicObj;
//devuelve el siguiente objeto visible en el orden de creación
var
  i: Integer;
begin
    //busca su orden dentro de los objects
    For i := 0 To objects.Count-1 do begin
      if objects[i] = c Then break;
    end;
    //calcula el siguiente elemento
    repeat
      Inc(i);
      If i >= objects.Count Then begin  //se ha llegado al final del conjunto
        Result := PrimerVisible;
        Exit;
      end;
    until objects[i].visible;
    //selecciona el siguiente visible
    Result := objects[i];
end;
function TView3D.AnteriorVisible(c: TGraphicObj): TGraphicObj;
//devuelve el anterior objeto visible en el orden de creación
var
  i: Integer;
begin
    //busca su orden dentro de los objects
    For i := 0 To objects.Count-1 do begin
      If objects[i] = c Then break;
    end;
    //calcula el elemento anterior
    repeat
      Dec(i);
      If i < 0 Then begin  //se ha llegado al inicio
        Result := UltimoVisible;
        Exit;
      End;
    until objects[i].visible;
    //selecciona el siguiente visible
    Result := objects[i];
End;
procedure TView3D.SeleccionarSiguiente;
//Selecciona el siguiente elemento visible en el orden de creación.
//Si no hay ninguno seleccionado, selecciona el primero
var
  s: TGraphicObj;
begin
    if NumeroVisibles() = 0 Then exit;
    if seleccion.Count = 1 Then begin  //hay uno seleccionado
        s := seleccion[0];   //toma el seleccionado
        s := SiguienteVisible(s);
        SelectNone;
        s.Selec;
    end else begin     //hay cero o más de uno seleccionado
        s := PrimerVisible;  //selecciona el primero
        SelectNone;
        s.Selec;
    end;
    Refresh;
end;
procedure TView3D.SeleccionarAnterior;
//Selecciona el anterior elemento visible en el orden de creación.
//Si no hay ninguno seleccionado, selecciona el ultimo
var
  s: TGraphicObj;
begin
    if NumeroVisibles() = 0 Then exit;
    if seleccion.Count = 1 then begin     //hay uno seleccionado
        s := seleccion[0];    //toma el seleccionado
        s := AnteriorVisible(s);
        SelectNone;
        s.Selec;
    end else begin               //hay cero o más de uno seleccionado
        s := UltimoVisible;   //selecciona el ultimo
        SelectNone;
        s.Selec;
    end;
    Refresh;
end;
//******************* Funciones de visualización **********************
procedure TView3D.AmpliarClick(factor: real = FACTOR_AMPLIA_ZOOM;
                        xr: integer = 0; yr: integer = 0);
var anc_p: Real ;  //ancho de pantalla
    alt_p: Real ;  //alto de pantalla
    x_zoom, y_zoom: Single;
begin
    If v2d.zoom < ZOOM_MAX_CONSULT Then
        v2d.zoom := v2d.zoom * factor;
    If (xr <> 0) Or (yr <> 0) Then begin  //se ha especificado una coordenada central
        anc_p := PBox.width / v2d.zoom;
        alt_p := PBox.Height / v2d.zoom;
        v2d.XYvirt(xr, yr, 0, x_zoom, y_zoom);     //convierte
        v2d.FijarVentana(PBox.Width, PBox.Height,
                x_zoom - anc_p / 2, x_zoom + anc_p / 2, y_zoom - alt_p / 2, y_zoom + alt_p / 2);
    End;
    Refresh;
End;
procedure TView3D.ReducirClick(factor: Real = FACTOR_AMPLIA_ZOOM;
                        x_zoom: Real = 0; y_zoom: Real = 0);
begin
    If v2d.zoom > ZOOM_MIN_CONSULT Then
        v2d.zoom := v2d.zoom / factor;
    Refresh;
End;
/////////////////////////  Funciones de selección /////////////////////////////
procedure TView3D.SeleccionarTodos;
var s: TGraphicObj;
begin
    For s In objects do s.Selec; //selecciona todos
End;
procedure TView3D.SelectNone();
var s: TGraphicObj;
begin
  For s In objects do //no se explora "seleccion" porque se modifica con "s.Deselect"
    if s.Selected then s.Deselect;
//  seleccion.Clear; //No se puede limpiar simplemente la lista. Se debe llamar a s.Deselect
End;
function  TView3D.Seleccionado: TGraphicObj;
//Devuelve el objeto seleccionado. Si no hay ninguno seleccionado, devuelve NIL.
begin
  Result := nil;   //valor por defecto
  if seleccion.Count = 0 then exit;  //no hay
  //hay al menos uno
  Result := seleccion[seleccion.Count-1];  //devuelve el único o último
End;
function  TView3D.ObjPorNombre(nom: string): TGraphicObj;
//Devuelve la referecnia a un objeto, dado el nombre. Si no encuentra, devuelve NIL.
var s: TGraphicObj;
begin
  Result := nil;   //valor por defecto
  if nom = '' then exit;
  For s In objects do
    if s.nombre = nom then begin
       Result := s;
       break;
    end;
End;

procedure TView3D.moverAbajo(desp: Double = DESPLAZ_MENOR) ;  //abajo
//Genera un desplazamiento en la pantalla haciendolo independiente del
//factor de ampliación actual
var
    z: Single ;  //zoom
begin
    z := v2d.zoom;
    Desplazar(0, round(desp / z));
    Refresh;
end;
procedure TView3D.moverArriba(desp: Double = DESPLAZ_MENOR) ;  //arriba
//Genera un desplazamiento en la pantalla haciendolo independiente del
//factor de ampliación actual
var
    z: Single ;  //zoom
begin
    z := v2d.zoom;
    Desplazar(0, round(-desp / z));
    Refresh;
end;
procedure TView3D.moverDerecha(desp: Double = DESPLAZ_MENOR) ;  //derecha
//Genera un desplazamiento en la pantalla haciendolo independiente del
//factor de ampliación actual
var
    z: Single ;  //zoom
begin
    z := v2d.zoom;
    Desplazar(round(desp / z), 0);
    Refresh;
end;
procedure TView3D.moverIzquierda(desp: Double = DESPLAZ_MENOR) ;  //izquierda
//Genera un desplazamiento en la pantalla haciendolo independiente del
//factor de ampliación actual
var
    z: Single ;  //zoom
begin
    z := v2d.zoom;
    Desplazar(round(-desp / z), 0);
    Refresh;
end;
procedure TView3D.Desplazar(dx, dy: integer);
begin
//Procedimiento "estandar" para hacer un desplazamiento de la pantalla
//Varía los parámetros de la perspectiva "x_cam" e "y_cam"
    v2d.Desplazar(dx, dy);
end;
//Modificación de objects
procedure TView3D.GraphicObjectAdd(argGraphicObject: TGraphicObj; AutoPos: boolean = true);
//Agrega un objeto grafico al editor. El objeto gráfico debe haberse creado previamente,
//y ser de tipo TGraphicObj o un descendiente. "AutoPos", permite posicionar automáticamente
//al objeto en pantalla, de modo que se evite ponerlo siempre en la misma posición.
var
  x: single;
  y: single;
begin
  if OnModify<>nil then OnModify;
  //Posiciona tratando de que siempre aparezca en pantalla
  if AutoPos Then begin  //Se calcula posición
    x := v2d.Xvirt(100, 100) + 30 * objects.Count Mod 400;
    y := v2d.Yvirt(100, 100) + 30 * objects.Count Mod 400;
    argGraphicObject.PlaceAt(x,y);
  end;
  //configura eventos para ser controlado por este editor
  argGraphicObject.OnSelect   := @GraphicObject_Select;     //referencia a procedimiento de selección
  argGraphicObject.OnDeselect := @GraphicObject_Unselec;    //referencia a procedimiento de "de-selección"
  argGraphicObject.OnCamPoint := @GraphicObject_SetPointer; //procedimiento para cambiar el puntero
//  Refresh(s)   ;             //Refresca objeto
  objects.Add(argGraphicObject);               //agrega elemento
end;
procedure TView3D.GraphicObjectDelete(obj: TGraphicObj);  //elimina un objeto grafico
begin
  obj.Deselect;  //por si acaso
  objects.Remove(obj);
  obj := nil;
  if OnModify<>nil then OnModify;
End;
procedure TView3D.DeleteSelected;
//Elimina la selección.
var
  v: TGraphicObj;
begin
  For v In seleccion  do  //explora todos
    GraphicObjectDelete(v);
  if OnModify<>nil then OnModify;
  Refresh;
end;

/////////////////////////   Funciones del Rectángulo de Selección /////////////////////////
procedure TView3D.DibujRecSeleccion();
//Dibuja por métodos gráficos el rectángulo de selección en pantalla
begin
    v2d.SetPen(clGreen, 1, psDot);
    v2d.rectang0(x1Sel, y1Sel, x2Sel, y2Sel);

    x1Sel_a := x1Sel; y1Sel_a := y1Sel;
    x2Sel_a := x2Sel; y2Sel_a := y2Sel;
End;
procedure TView3D.InicRecSeleccion(X, Y: Integer);
//Inicia el rectángulo de selección, con las coordenadas
begin
    x1Sel:= X; y1Sel := Y;
    x2Sel := X; y2Sel := Y;
    x1Sel_a := x1Sel;
    y1Sel_a := y1Sel;
    x2Sel_a := x2Sel;
    y2Sel_a := y2Sel;
End;
function TView3D.RecSeleccionNulo: Boolean;
 //Indica si el rectángulo de selección es de tamaño NULO o despreciable
begin
    If (x1Sel = x2Sel) And (y1Sel = y2Sel) Then
        RecSeleccionNulo := True
    Else
        RecSeleccionNulo := False;
End;
function TView3D.enRecSeleccion(X, Y: Single): Boolean;
//Devuelve verdad si (x,y) esta dentro del rectangulo de seleccion.
var xMin, xMax: Integer;   //coordenadas mínimas y máximas del recuadro
    yMin, yMax: Integer;
    xx1, yy1: Single;
    xx2, yy2: Single;
begin
    //guarda coordenadas mínimas y máximas
    If x1Sel < x2Sel Then begin
        xMin := x1Sel;
        xMax := x2Sel;
    end Else begin
        xMin := x2Sel;
        xMax := x1Sel;
    End;
    If y1Sel < y2Sel Then begin
        yMin := y1Sel;
        yMax := y2Sel;
    end Else begin
        yMin := y2Sel;
        yMax := y1Sel;
    End;

    v2d.XYvirt(xMin, yMin, 0, xx1, yy1);
    v2d.XYvirt(xMax, yMax, 0, xx2, yy2);

    //verifica si está en región
    If (X >= xx1) And (X <= xx2) And (Y >= yy1) And (Y <= yy2) Then
        enRecSeleccion := True
    Else
        enRecSeleccion := False;
End;
////////////////// Eventos para atender requerimientos de objects "TGraphicObj" ///////////////////////
procedure TView3D.GraphicObject_Select(obj: TGraphicObj);
//Agrega un objeto gráfico a la lista "selección". Este método no debe ser llamado directamente.
//Si se quiere seleccionar un objeto se debe usar la forma objeto.Selec.
begin
//    If obj.Seleccionado Then Exit;  //Ya está seleccionado. No debe ser necesario
  seleccion.Add(obj);      { TODO : Verificar si se puede manejar bien el programa sin usar la propiedad "NombreObj"}
End;
procedure TView3D.GraphicObject_Unselec(obj: TGraphicObj);
//Quita un objeto gráfico de la lista "selección". Este método no debe ser llamado directamente.
//Si se quiere quitar la seleccion a un objeto se debe usar la forma objeto.Deselect.
begin
//    If not obj.Seleccionado Then Exit;
  seleccion.Remove(obj);
End;
procedure TView3D.GraphicObject_SetPointer(Punt: integer);
//Procedimiento que cambia el puntero del mouse. Es usado para proporcionar la los objects "TGraphicObj"
//la posibilidad de cambiar el puntero.
begin
  PBox.Cursor := Punt;        //define cursor
end;
//Rutinas de procesamiento de estados
function TView3D.StateAsStr: string;
{Debe el esatdo como una cadena descriptiva. Es necesario actualizar la desciprción
para cada state nuevoq ue se vaya agregando.}
begin
  case State of
  EP_NORMAL      : Result := 'Normal';
  EP_SELECMULT   : Result := 'Selecc. Múltiple';
  EP_MOV_OBJS    : Result := 'Moviendo objects';
  EP_DESP_PANT   : Result := 'Desplaz. Pantalla';
  EP_DESP_ANG    : Result := 'Rotando Pantalla';
  EP_DIMEN_OBJ   : Result := 'Dimension.objects';
  EP_RAT_ZOOM    : Result := 'Zoom con ratón';
  EP_COMM_LINE   : Result := 'Modo línea';
  EP_COMM_RECTAN : Result := 'Modo Rectángulo';
  else
    Result := '<< Desconocido >>';
  end;
end;
procedure TView3D.RegisterState(State0: TViewState;
  EventHandler: TVisEventHandler);
{Registra un nuevo state del Ratón}
begin
  EventOfState[State0] := EventHandler;
end;
procedure TView3D.ClearEventState;
var
  sta: TViewState;
begin
  for sta := low(TViewState) to high(TViewState) do begin
    EventOfState[sta] := nil;
  end;
end;
procedure TView3D.SendData(Data: string);
{Solicita enviar datos al comadno actual (que debe ser el state actual).}
begin
  CallEventState(State, vmeEjecComm, mbExtra1, [], 0, 0, Data);  //para Initiate
end;
procedure TView3D.CallEventState(State0: TViewState;
  EventTyp: TVisEventTyp; Button: TMouseButton;
  Shift: TShiftState; xp, yp: Integer; txt: string);
{Llama al evento apropiado para el state indicado}
var
  eveHandler: TVisEventHandler;
begin
  eveHandler := EventOfState[State0];
  if eveHandler=nil then exit;  //protección
  eveHandler(EventTyp, Button, Shift, xp, yp, txt);
end;
//Manejadores de eventos de state
function GetNumber(var txt: string): Single;
{Extrae un número de una cadena de texto. Si hay error, devuelev "MaxInt"}
var
  decimalMark: Boolean;
  i: Integer;
  numTxt: String;
begin
  if txt = '' then exit(MaxInt);
  if not (txt[1] in ['0'..'9']) then exit(MaxInt);
  i := 2;
  decimalMark := false;
  while (i<=length(txt)) and (txt[i] in ['0'..'9','.']) do begin
    if txt[i]='.' then begin
      if decimalMark then break;  //ya hay un punto decimal
      decimalMark := true;        //indica que encontró el punto decimal
    end;
    Inc(i)
  end;
  //Terminó de explorar la cadena
  numTxt := copy(txt, 1, i-1);
  Result := StrToDouble(numTxt);   //no debería fallar si se ha extraído bien el número
  delete(txt, 1, i-1);
end;
function GetSeparator(var txt: string): boolean;
{Extrae un separador (espacio o coma) de una cadena de texto, ignorando los espacios
múltiples. Si no encuentra un  separador, devuelve FALSE}
var
  i: Integer;
  HaveSeparator: Boolean;
begin
  if txt='' then exit(false);
  i := 1;
  HaveSeparator := false;
  while (i<=length(txt)) and (txt[i] in [#32, #9]) do begin
    HaveSeparator := true;
    inc(i);  //extrae espacios
  end;
  if txt[i] = ',' then begin
    HaveSeparator := true;
    inc(i);
  end;
  while (i<=length(txt)) and  (txt[i] in [#32, #9]) do inc(i);  //extrae espacios adicionales
  delete(txt, 1, i-1);  //elimina texto procesado
  Result := HaveSeparator;   //devuelve resultado
end;
function GetCoords(var txt: string; out x , y: Single): boolean;
{Devuelve las coordenadas leídas de una cadena de texto. Si hay error
devuelve FALSE.}
begin
  x := GetNumber(txt);
  if x=MaxInt then exit(false);
  if not GetSeparator(txt) then exit(false);
  y := GetNumber(txt);
  if y=MaxInt then exit(false);
  exit(true);
end;
procedure TView3D.proc_NORMAL(EventTyp: TVisEventTyp; Button: TMouseButton;
  Shift: TShiftState; xp, yp: Integer; txt: string);
{Procesa eventos, en el esatdo NORMAL. Este es el state estable o Proj defecto.
Desde aquí se pasan a todos los demás estados.}
var
  o: TGraphicObj;
  s: TGraphicObj;
  ogs: TGraphicObj;  //objeto seleccionado
begin
  if EventTyp = vmeMouseDown then begin  ////////// Botón Pulsado
     ogs := SeleccionaAlguno(xp, yp);  //verifica si selecciona a un objeto
     if          Shift = [ssRight] then begin     //Botón derecho
         if ogs = nil Then begin  //Ninguno seleccionado
             SelectNone;
             Refresh;
             State := EP_SELECMULT;  //inicia seleccion multiple
             InicRecSeleccion(x_pulso, y_pulso);
         end else begin //Selecciona a uno, pueden haber otros seleccionados
             if ogs.Selected Then  begin  //Se marcó sobre un seleccionado
//                   if Shift = [] Then SelectNone;
                 ogs.MouseDown(Self, Button, Shift, xp, yp);  //Pasa el evento
                 exit;
             end;
             //Se selecciona a uno que no tenía selección
             if Shift = [ssRight] Then  //Sin Control ni Shift
               SelectNone;
             ogs.MouseDown(Self, Button, Shift, xp, yp);  //Pasa el evento
             Refresh;
              //ParaMover = True       ;  //listo para mover
         end;
     end else If Shift = [ssLeft] then begin      //Botón izquierdo
         if ogs = NIL Then  begin  //No selecciona a ninguno
             SelectNone;
             Refresh;
             State := EP_SELECMULT;  //inicia seleccion multiple
             InicRecSeleccion(x_pulso, y_pulso);
         end Else begin     //selecciona a uno, pueden haber otros seleccionados
             If ogs.Selected Then begin //Se marcó sobre un seleccionado
                 //No se quita la selección porque puede que se quiera mover
                 //varios objects seleccionados. Si no se mueve, se quitará la
                 //selección en PBox_MouseUp
                 //If Shift = 0 Then Call SelectNone
                 ogs.MouseDown(Self, Button, Shift, xp, yp);  //Pasa el evento
                 ParaMover := True;  //listo para mover
                 Exit;               //Se sale sin desmarcar
             end;
             //Se selecciona a uno que no tenía selección
             if Shift = [ssLeft] Then  //Sin Control ni Shift
                SelectNone;
             ogs.MouseDown(Self, Button, Shift, xp, yp);  //Pasa el evento
             ParaMover := True;            //listo para mover
         end;
     end else if Shift >= [ssCtrl, ssShift] then begin   //Contiene Shift+Ctrl
         //Inicia state de ZOOM. Puede convertirse en EP_DESP_PANT
         //si luego se genera el evento Move()
         State := EP_RAT_ZOOM;
         Exit;  //Ya no se necesita procesar
     end else if (Shift = [ssMiddle]) or (Shift = [ssCtrl, ssShift, ssRight]) then begin
         //Inicia el modo de desplazamiento
         State := EP_DESP_PANT;
     end else if Shift = [ssMiddle, ssShift] then begin  //Botón central y Shift
         //Inicia el módo de cambio ángulo de visión
         State := EP_DESP_ANG;
     end;
  end else if EventTyp = vmeMouseMove then begin  /////// Movim. Mouse
    //CapturoEvento lo actualiza la rutina "VerificarParaMover"
    If CapturoEvento <> NIL Then begin
       CapturoEvento.Mover(Xp, Yp, seleccion.Count);
       Refresh;
    end Else begin  //Movimiento simple
        s := VerificarMovimientoRaton(Xp, Yp);
        if s <> NIL then s.MouseMove(self, Shift, Xp, Yp);  //pasa el evento
    end;
  end else if EventTyp = vmeMouseUp then begin /////// Botón soltado
      o := SeleccionaAlguno(xp, yp);  //verifica si selecciona a un objeto
      if Button = mbRight then begin //----- solto derecho -------------------
(*            If o = NIL Then  //Ninguno seleccionado
              RaiseEvent ClickDerDiag    //Genera evento
          Else    ;  //Hay uno que lo selecciona, o más???
              If Not o.Seleccionado Then Call o.SoltoRaton(Button, Shift, xr, yr)    ;  //Pasa el evento
              RaiseEvent ClickDerSel     //Genera evento
          End If*)
      end else If Button = mbLeft Then begin //----- solto izquierdo -----------
          If o = NIL Then    //No selecciona a ninguno
//                SelectNone
          else begin         //Selecciona a alguno
              If Shift = [] Then SelectNone;
              o.Selec;   //selecciona
              o.MouseUp(self, Button, Shift, xp, yp, false);
              Refresh;
              //Verifica si el objeto está pidiendo que lo eliminen
//Este código se comentó porque no se le encontró ninguna utilidad
//                if o.Erased then begin
//                  GraphicObjectDelete(o);
//                  Refresh;
//                end;
          End;
          CapturoEvento := NIL;      //inicia bandera de captura de evento
          ParaMover := False;        //por si aca
      end;
  end else if EventTyp = vmeEjecComm then begin /////// Ejecutar comando
      if txt = 'LINE' then begin
        State := EP_COMM_LINE;   //inicia el state
        CallEventState(State, vmeEjecComm, mbExtra1, [], 0, 0, '');  //para Initiate
      end else if txt = 'RECTANGLE' then begin
        State := EP_COMM_RECTAN;   //inicia el state
        CallEventState(State, vmeEjecComm, mbExtra1, [], 0, 0, '');  //para Initiate
      end else if UpCase(txt) = 'CANCEL' then begin
        //Cancela todos los comandos activos
        RestoreState;
      end else begin
        if OnSendMessage<>nil then OnSendMessage('Comando desconocido: "' + txt + '"');
      end;
  end;
end;
procedure TView3D.proc_SELECMULT(EventTyp: TVisEventTyp;
  Button: TMouseButton; Shift: TShiftState; xp, yp: Integer; txt: string);
var
  o: TGraphicObj;
  s: TGraphicObj;
begin
  if EventTyp = vmeMouseDown then begin
  end else if EventTyp = vmeMouseMove then begin
    x2Sel := xp;
    y2Sel := xp;
    //verifica los que se encuentran seleccionados
    if objects.Count < 100 Then begin//sólo anima para pocos objects
        for s In objects do begin
          if s.SelLocked then continue;
          if enRecSeleccion(s.XCent, s.YCent) And Not s.Selected Then begin
            s.Selec;
          End;
          if Not enRecSeleccion(s.XCent, s.YCent) And s.Selected Then begin
            s.Deselect;
          end;
        end;
    End;
    Refresh
  end else if EventTyp = vmeMouseUp then begin
    if objects.Count > 100 Then begin  //Necesita actualizar porque la selección múltiple es diferente
      for o in objects do
        if enRecSeleccion(o.XCent, o.YCent) And Not o.Selected Then o.Selec;
    end;
    State := EP_NORMAL;
  end;
end;
procedure TView3D.proc_MOV_OBJS(EventTyp: TVisEventTyp;
  Button: TMouseButton; Shift: TShiftState; xp, yp: Integer; txt: string);
var
  s: TGraphicObj;
  o: TGraphicObj;
begin
  if EventTyp = vmeMouseDown then begin
  end else if EventTyp = vmeMouseMove then begin
    if OnModify<>nil then OnModify;  //cambio
    for s in seleccion do
        s.Mover(xp,yp, seleccion.Count);
    Refresh;
  end else if EventTyp = vmeMouseUp then begin
//Debug.Print "Esatado EP_MOV_OBJS"
    for o In seleccion do  //Pasa el evento a la selección
        o.MouseUp(self, Button, Shift, xp, yp, State = EP_MOV_OBJS);
    State := EP_NORMAL;  //fin de movimiento
    Refresh;
    //Genera eventos. Los objects movidos se pueden determinar a partir de la selección.
    if OnObjectsMoved<>nil then OnObjectsMoved;
  end;
end;
procedure TView3D.proc_DESP_PANT(EventTyp: TVisEventTyp;
  Button: TMouseButton; Shift: TShiftState; xp, yp: Integer; txt: string);
var
  dx, dy: Single;
begin
  if EventTyp = vmeMouseDown then begin
  end else if EventTyp = vmeMouseMove then begin
    v2d.ObtenerDesplazXY( xp, yp, x_pulso, y_pulso, dx, dy);
    v2d.x_cam -= dx;
    v2d.y_cam -= dy;
    x_pulso := xp; y_pulso := yp;  {Tal vez deba usar otras variables aparte de x_pulso, e
                                  y_pulso,  para no interferir}
    Refresh;
  end else if EventTyp = vmeMouseUp then begin
    //Si estaba desplazándose, vuelve al state normal
    State := EP_NORMAL;
  end;
end;
procedure TView3D.proc_DESP_ANG(EventTyp: TVisEventTyp;
  Button: TMouseButton; Shift: TShiftState; xp, yp: Integer; txt: string);
var
  dx, dy: Single;
begin
  if EventTyp = vmeMouseDown then begin
  end else if EventTyp = vmeMouseMove then begin
    v2d.ObtenerDesplazXY( xp, yp, x_pulso, y_pulso, dx, dy);
    v2d.Alfa := v2d.Alfa + dx/100;
    v2d.Fi   := v2d.Fi + dy/100;
    x_pulso := xp; y_pulso := yp;  {Tal vez deba usar otras variables aparte de x_pulso, e
                                   y_pulso,  para no interferir}
    Refresh;
  end else if EventTyp = vmeMouseUp then begin
    State := EP_NORMAL;
  end;
end;
procedure TView3D.proc_DIMEN_OBJ(EventTyp: TVisEventTyp;
  Button: TMouseButton; Shift: TShiftState; xp, yp: Integer; txt: string);
begin
  if EventTyp = vmeMouseDown then begin
  end else if EventTyp = vmeMouseMove then begin
      //se está dimensionando un objeto
      CapturoEvento.Mover(Xp, Yp, seleccion.Count);
      Refresh;
  end else if EventTyp = vmeMouseUp then begin
    //pasa evento a objeto que se estaba dimensionando
    CapturoEvento.MouseUp(self, Button, Shift, xp, yp, false);
    //termina state
    State := EP_NORMAL;
    CapturoEvento := NIL;      //inicia bandera de captura de evento
    ParaMover := False;        //por si aca
  end;
end;
procedure TView3D.proc_RAT_ZOOM(EventTyp: TVisEventTyp;
  Button: TMouseButton; Shift: TShiftState; xp, yp: Integer; txt: string);
begin
  if EventTyp = vmeMouseDown then begin
  end else if EventTyp = vmeMouseMove then begin
  end else if EventTyp = vmeMouseUp then begin
    If Button = mbLeft Then AmpliarClick(1.2, xp, yp) ;  //<Shift> + <Ctrl> + click izquierdo
    If Button = mbRight Then ReducirClick(1.2, xp, yp) ;  //<Shift> + <Ctrl> + click derecho
    State := EP_NORMAL;
  end;
end;
procedure TView3D.proc_COMM_LINE(EventTyp: TVisEventTyp;
  Button: TMouseButton; Shift: TShiftState; xp, yp: Integer; txt: string);
const
  {Usamos constante con tipo porque no hay STATIC en FreePascal, y como este
  procedimiento irá ejecutándose repetídamente, necesitamos conservar el valor de las
  variables, entre sesión y sesión.}
  paso: integer  = 0;
  lin: TObjGrafDXF = nil;
  x0: Single = 0;  //Coordenadas iniciales
  y0: Single = 0;  //Coordenadas iniciales
var
  xLin, yLin: Single;
begin
  if paso = 0 then begin  //Inicio de comando
    OnSendMessage('>> Ingrese punto inicial:');
    paso := 1;
  end else if paso = 1 then begin
    case EventTyp of
    vmeEjecComm: begin  //Inicio de comando
      //Esperamos coordenadas iniciales
      if txt = 'CANCEL' then begin  //válido en cualquier state
        RestoreState('>> Comando:');   //Termina
        paso := 0;   //reinicia
        exit;
      end;
      if not GetCoords(txt, xLin, yLin) then begin
        OnSendMessage('>> ERROR: Ingrese punto inicial:');
        exit;
      end;
      //Agregar recta, con las coord. dadas
      lin := TObjGrafDXF.Create(v2d);
      lin.SetP0(xLin, yLin, 0); //Especifica el primer punto
      lin.SetP1(xvPtr, yvPtr, 0); //Especifica siguiente punto por defecto
      x0 := xLin; y0 := yLin;  //guarda primer punto
    end;
    vmeMouseDown: begin
      //Agregar recta, con las coord. dadas
      lin := TObjGrafDXF.Create(v2d);
      lin.SetP0(xvPtr, yvPtr, 0); //Esperamos coordenadas
      lin.SetP1(xvPtr, yvPtr, 0);
      x0 := xvPtr; y0 := yvPtr;  //guarda primer punto
    end;
    else exit;  //Sale para los otros eventos, sino puede generar errro
    end;
    GraphicObjectAdd(lin);
    Refresh;
    OnSendMessage('>> Ingrese siguiente punto ([C]errar):');
    paso := 2;
  end else if paso = 2 then begin
    case EventTyp of
    vmeEjecComm: begin  //Inicio de comando
      //Esperamos coordenadas finales
      if txt = 'CANCEL' then begin
        //Se debe eliminar la última recta
        GraphicObjectDelete(lin);   //Mejor sería, si se hace con un UNDO
        Refresh;
        RestoreState('>> Comando:');   //Termina
        paso := 0;   //reinicia
        exit;
      end;
      if txt = 'C' then begin
        //Cerrar líneas
        lin.SetP1(x0, y0, 0);
        Refresh;
        paso := 0;   //reinicia
        exit;
      end;
      if not GetCoords(txt, xLin, yLin) then begin
        OnSendMessage('>> Ingrese siguiente punto ([C]errar):');
        exit;
      end;
      lin.SetP1(xLin, yLin, 0);
      Refresh;

      //Inicia otra recta, sin salir del state
      lin := TObjGrafDXF.Create(v2d);
      lin.SetP0(xLin, yLin, 0); //Esperamos coordenadas
      lin.SetP1(xvPtr, yvPtr, 0);
      GraphicObjectAdd(lin);
      Refresh;
      OnSendMessage('>> Ingrese siguiente punto ([C]errar):');
//        //Terminó el comando
//        RestoreState('>> Comando:');
//        paso := 0;
//        lin := nil;
    end;
    vmeMouseMove: begin
      //En esta fase, se debe hacer la animación por si se usa el Mouse para PlaceAt
        //el siguiente punto.
      lin.SetP1(xvPtr, yvPtr, 0);
      Refresh;
    end;
    vmeMouseDown: begin
      //Esperamos coordenadas finales
      lin.SetP1(xvPtr, yvPtr, 0);
      Refresh;

      //Inicia otra recta, sin salir del state
      lin := TObjGrafDXF.Create(v2d);
      lin.SetP0(xvPtr, yvPtr, 0); //Esperamos coordenadas
      lin.SetP1(xvPtr, yvPtr, 0);
      GraphicObjectAdd(lin);
      Refresh;
      OnSendMessage('>> Ingrese siguiente punto:');

//        //Terminó el comando
//        RestoreState('>> Comando:');
//        paso := 0;
//        lin := nil;
    end;
    end;
  end;
end;
procedure TView3D.proc_COMM_RECTAN(EventTyp: TVisEventTyp;
  Button: TMouseButton; Shift: TShiftState; xp, yp: Integer; txt: string);
const
  {Usamos constante con tipo porque no hay STATIC en FreePascal, y como este
  procedimiento irá ejecutándose repetídamente, necesitamos conservar el valor de las
  variables, entre sesión y sesión.}
  paso: integer  = 0;
  lin: TObjGrafDXF = nil;
var
  xLin, yLin: Single;
begin
  case EventTyp of
  vmeEjecComm: begin  //Inicio de comando
      if paso = 0 then begin  //Inicio de comando
        OnSendMessage('>> Ingrese punto inicial:');
        paso := 1;
      end else if paso = 1 then begin
        if txt = 'CANCEL' then begin  //válido en cualquier state
          RestoreState('>> Comando:');   //Termina
          paso := 0;   //reinicia
          exit;
        end;
        //Esperamos coordenadas iniciales
        if not GetCoords(txt, xLin, yLin) then begin
          OnSendMessage('>> ERROR: Ingrese punto inicial:');
          exit;
        end;
        //Agregar recta, con las coord. dadas
        lin := TObjGrafDXF.Create(v2d);
        lin.SetP0(xLin, yLin, 0); //Especifica el primer punto
        lin.SetP1(xvPtr, yvPtr, 0); //Especifica el primer punto
        GraphicObjectAdd(lin);
        Refresh;
        OnSendMessage('>> Ingrese siguiente punto:');
        paso := 2;
      end else if paso = 2 then begin
        if txt = 'CANCEL' then begin
          //Se debe eliminar la última recta
          GraphicObjectDelete(lin);   //Mejor sería, si se hace con un UNDO
          Refresh;
          RestoreState('>> Comando:');   //Termina
          paso := 0;   //reinicia
          exit;
        end;
        //Esperamos coordenadas finales
        if not GetCoords(txt, xLin, yLin) then begin
          OnSendMessage('>> ERROR: Ingrese punto inicial:');
          exit;
        end;
        lin.SetP1(xLin, yLin, 0);
        Refresh;

        //Terminó el comando
        RestoreState('>> Comando:');
        paso := 0;
        lin := nil;
      end;
    end;
  vmeMouseMove: begin
    if paso = 2 then begin
        //En esta fase, se debe hacer la animación por si se usa el Mouse para PlaceAt
        //el siguiente punto.
        lin.SetP1(xvPtr, yvPtr, 0);
        Refresh;
    end;
  end;
  vmeMouseDown: begin
      if paso = 1 then begin
        //Agregar recta, con las coord. dadas
        lin := TObjGrafDXF.Create(v2d);
        lin.SetP0(xvPtr, yvPtr, 0); //Esperamos coordenadas
        lin.SetP1(xvPtr, yvPtr, 0);
        GraphicObjectAdd(lin);
        Refresh;
        OnSendMessage('>> Ingrese siguiente punto:');
        paso := 2;
      end else if paso = 2 then begin
        //Esperamos coordenadas finales
        lin.SetP1(xvPtr, yvPtr, 0);
        Refresh;

        //Inicia otra recta, sin salir del state
        lin := TObjGrafDXF.Create(v2d);
        lin.SetP0(xvPtr, yvPtr, 0); //Esperamos coordenadas
        lin.SetP1(xvPtr, yvPtr, 0);
        GraphicObjectAdd(lin);
        Refresh;
        OnSendMessage('>> Ingrese siguiente punto:');

//        //Terminó el comando
//        RestoreState('>> Comando:');
//        paso := 0;
//        lin := nil;
      end;
    end;
  end;
end;
//Inicialización
procedure TView3D.RestoreState(msg: string='');
{Resatura el state del Visor, poniéndolo en state EP_NORMAL.
Si se indica "msg", se genera el evento OnSendMessage().}
begin
  State := EP_NORMAL;
  ParaMover := false;
  CapturoEvento := nil;
  ultMarcado := nil;
  PBox.Cursor := CUR_DEFEC;        //define cursor
  if msg<>'' then OnSendMessage(msg);
end;
constructor TView3D.Create(PB0: TPaintBox; objectList: TEditorObjList);
{Metodo de inicialización de la clase Visor. Debe indicarse el PaintBox de
salida donde se controlarán los objects gráficos.
y también debe recibir la lista de objects a administrar.}
var
  argGraphicObject: TMyObject;
begin
  PBox := PB0;  //asigna control de salida
  objects := objectList;
  //Intercepta eventos
  PBox.OnMouseUp   := @PBox_MouseUp;
  PBox.OnMouseDown := @PBox_MouseDown;
  PBox.OnMouseMove := @PBox_MouseMove;
  PBox.OnMouseWheel:= @PBox_MouseWheel;
  PBox.OnDblClick  := @PBox_DblClick;
  PBox.OnPaint     := @PBox_Paint;
  PBox.OnResize    := @PBox_Resize;
  //Inicia motor
  v2d := TMotGraf.Create(PBox);    //Inicia motor gráfico
  v2d.SetFont('MS Sans Serif');  //define tipo de letra
  v2d.OnChangeView:=@v2d_ChangeView;
  seleccion := TEditorObjList.Create(FALSE);  {crea lista sin posesión", porque la
                                             administración la hará "objects".}
  RestoreState;
  incWheel  := 0.1;
  ClearEventState;   //Limpia tabla de eventos de state, por seguridad
  //Crea lista de eventos. Debe crearse para todos los valores de TViewState
  RegisterState(EP_NORMAL   , @proc_NORMAL);
  RegisterState(EP_SELECMULT, @proc_SELECMULT);
  RegisterState(EP_MOV_OBJS , @proc_MOV_OBJS);
  RegisterState(EP_DESP_PANT, @proc_DESP_PANT);
  RegisterState(EP_DESP_ANG , @proc_DESP_ANG);
  RegisterState(EP_DIMEN_OBJ, @proc_DIMEN_OBJ);
  RegisterState(EP_RAT_ZOOM , @proc_RAT_ZOOM);
  //Comandos
  RegisterState(EP_COMM_LINE, @proc_COMM_LINE);
  RegisterState(EP_COMM_RECTAN, @proc_COMM_RECTAN);
  ///////////!!!!!!!!!!!!!!!!!!!!!!
argGraphicObject := TMyObject.Create(v2d);
GraphicObjectAdd(argGraphicObject);
end;
destructor TView3D.Destroy;
begin
  seleccion.Free;
  v2d.Free;      //Libera
  //resatura eventos
  PBox.OnMouseUp:=nil;
  PBox.OnMouseDown:=nil;
  PBox.OnMouseMove:=nil;
  PBox.OnMouseWheel:=nil;
  PBox.OnDblClick:=nil;
  PBox.OnPaint:=nil;
  PBox.OnResize:=nil;
  inherited;     //llama al destructor
end;

end.

