{                                frameVisCplex
Este Frame será usado para colocar nuestro editor gráfico. Incluye un PaintBox,
como salida gráfica.
Al ser un frame, puede incluirse en un formulario cualquiera.

                                              Por Tito Hinostroza  04/01/2017
}
unit guiFramePaintBox;
{$mode objfpc}{$H+}
interface
uses
  Classes, Forms, ExtCtrls,
  sketchEditor, sketchCore;
type
  TOnObjetosElim = procedure of object;
  {Evento para movimiento del Mouse. Notar que además de las coordenaadas del ratón,
  proporciona coordenadas virtuales}
  TEveMouseVisGraf = procedure(Shift: TShiftState; xp, yp: Integer;
                                      xv, yv, zv: Single) of object;

  { TfraPaintBox }

  TfraPaintBox = class(TFrame)
  published
    PaintBox1: TPaintBox;
  private
    function GetAlfa: Single;
    function GetFi: Single;
    function GetxCam: Single;
    function GetyCam: Single;
    function GetZoom: Single;
    procedure motEdi_ChangeView;
    procedure visEdi_Modif;
    procedure SetAlfa(AValue: Single);
    procedure SetFi(AValue: Single);
    procedure SetxCam(AValue: Single);
    procedure SetxDes(AValue: integer);
    function GetxDes: integer;
    procedure SetyCam(AValue: Single);
    procedure SetyDes(AValue: integer);
    function GetyDes: integer;
    procedure SetZoom(AValue: Single);
    procedure visEdi_ChangeState(ViewState: TViewState);
    procedure visEdi_MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure visEdi_SendMessage(msg: string);
  public
    objetos : TlistObjGraf; //Lista de objetos
    viewEdi  : TVisGraf3D;  //motor de edición  (La idesa es que pueda haber más de uno)
    Modif   : Boolean;      //bandera para indicar Diagrama Modificado
    OnObjetosElim  : TOnObjetosElim;   //cuando se elminan uno o más objetos
    OnChangePersp: procedure of object; //Cambia x_des,y_des,x_cam,y_cam,alfa,fi o zoom
    OnMouseMoveVirt: TEveMouseVisGraf;
    OnChangeState  : TEvChangeState;  //Cambia el state del Visor
    OnSendMessage  : TEvSendMessage;  //Envía un mensaje. Usado para respuesta a comandos
    procedure EliminarTodosObj;
  public //Propiedades reflejadas
    property xDes: integer read GetxDes write SetxDes;
    property yDes: integer read GetyDes write SetyDes;
    property xCam: Single read GetxCam write SetxCam;
    property yCam: Single read GetyCam write SetyCam;
    property Alfa: Single read GetAlfa write SetAlfa;
    property Fi: Single read GetFi write SetFi;
    property Zoom: Single read GetZoom write SetZoom;
    procedure ExecuteCommand(command: string);
    function StateAsStr: string; //Cadena de descripción de state
  public  //Inicialización
    procedure InitView;
    constructor Create(AOwner: TComponent; ListObjGraf: TlistObjGraf);
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}

procedure TfraPaintBox.EliminarTodosObj;
//Elimina todos los objetos gráficos existentes
begin
  if objetos.Count=0 then exit;  //no hay qué eliminar
  //elimina
  viewEdi.DeseleccionarTodos;  //por si acaso hay algun simbolo seleccionado
  objetos.Clear;          //limpia la lista de objetos
  viewEdi.RestoreState;
  Modif := true;          //indica que se modificó
  if OnObjetosElim<>nil then OnObjetosElim;
End;
function TfraPaintBox.GetxDes: integer;
begin
  Result := viewEdi.v2d.x_des;
end;
procedure TfraPaintBox.SetxDes(AValue: integer);
begin
  viewEdi.v2d.x_des:=AValue;
end;
function TfraPaintBox.GetyDes: integer;
begin
  Result := viewEdi.v2d.y_des;
end;
procedure TfraPaintBox.SetyDes(AValue: integer);
begin
  viewEdi.v2d.y_des:=AValue;
end;
function TfraPaintBox.GetxCam: Single;
begin
  Result := viewEdi.v2d.x_cam;
end;
procedure TfraPaintBox.SetxCam(AValue: Single);
begin
  viewEdi.v2d.x_cam:=AValue;
end;
function TfraPaintBox.GetyCam: Single;
begin
  Result := viewEdi.v2d.y_cam;
end;
procedure TfraPaintBox.SetyCam(AValue: Single);
begin
  viewEdi.v2d.y_cam:=AValue;
end;
function TfraPaintBox.GetZoom: Single;
begin
  Result := viewEdi.v2d.Zoom;
end;
procedure TfraPaintBox.ExecuteCommand(command: string);
begin
  viewEdi.ExecuteCommand(command);
end;
function TfraPaintBox.StateAsStr: string;
begin
  Result := viewEdi.StateAsStr;
end;
procedure TfraPaintBox.motEdi_ChangeView;
begin
  if OnChangePersp<>nil then OnChangePersp();
end;
procedure TfraPaintBox.visEdi_Modif;
{Se ejecuta cuando el visor reporta cambios (dimensionamieno, posicionamiento, ...) en
 alguno de los objetos gráficos.}
begin
  Modif := true;
end;
procedure TfraPaintBox.SetZoom(AValue: Single);
begin
  viewEdi.v2d.Zoom:=AValue;
end;
procedure TfraPaintBox.visEdi_ChangeState(ViewState: TViewState);
begin
  if OnChangeState<>nil then OnChangeState(ViewState);
end;
procedure TfraPaintBox.visEdi_MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  xv, yv: Single;
begin
  viewEdi.v2d.XYvirt(X, Y, 0, xv, yv);
  if OnMouseMoveVirt<>nil then OnMouseMoveVirt(Shift, X, Y, xv, yv, 0);
end;
procedure TfraPaintBox.visEdi_SendMessage(msg: string);
begin
  if OnSendMessage<>nil then OnSendMessage(msg);
end;

function TfraPaintBox.GetAlfa: Single;
begin
  Result := viewEdi.v2d.Alfa;
end;
procedure TfraPaintBox.SetAlfa(AValue: Single);
begin
  viewEdi.v2d.Alfa := AValue;
end;
function TfraPaintBox.GetFi: Single;
begin
  REsult := viewEdi.v2d.Fi;
end;
procedure TfraPaintBox.SetFi(AValue: Single);
begin
  viewEdi.v2d.Fi := AValue;
end;
procedure TfraPaintBox.InitView;
{Ubica la perspectiva y los ejes, de forma que el origen (0,0) aparezza en la
esquina inferior izquierda. Se debe llamar cuando ya el frame tenga su tamaño final}
begin
  viewEdi.v2d.Alfa:=0;
  viewEdi.v2d.Fi:=0;
  viewEdi.v2d.Zoom:=0.5;
  //Ubica (0,0) a 10 pixeles del ángulo inferior izquierdo
  viewEdi.v2d.x_cam:=((PaintBox1.Width div 2)-10)/viewEdi.v2d.Zoom;
  viewEdi.v2d.y_cam:=((PaintBox1.Height div 2)-10)/viewEdi.v2d.Zoom;
end;
constructor TfraPaintBox.Create(AOwner: TComponent; ListObjGraf: TlistObjGraf);
begin
  inherited Create(AOwner);
  objetos := ListObjGraf;  //recibe lista de objetos
  //objetos:= TlistObjGraf.Create(true);  //lista de objetos
  viewEdi := TVisGraf3D.Create(PaintBox1, objetos);
  viewEdi.OnModif      :=@visEdi_Modif;
  viewEdi.OnChangeView :=@motEdi_ChangeView;
  viewEdi.OnMouseMove  :=@visEdi_MouseMove;
  viewEdi.OnChangeState:=@visEdi_ChangeState;
  viewEdi.OnSendMessage:=@visEdi_SendMessage;
end;
destructor TfraPaintBox.Destroy;
begin
  viewEdi.Destroy;
  //objetos.Destroy;
  inherited;
end;

end.

