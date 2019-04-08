{                                frameVisCplex
This Frame will be used to place our graphic editor. Requires a TPaintBox
object, as graphic output. To work as an object editor graphics, you must
create an instance of "TModEdicion" and give it the reference of the PaintBox
Here you should also put the routines that allow you to add the various
graphic objects with which our application will work.

                                               By Tito Hinostroza 05/11/2014
}
unit guiFrameEditor;
{$mode objfpc}{$H+}
interface

uses
  Classes, Forms, ExtCtrls,
  sketchDxf, sketchEditor, sketchCore;
type
  TOnObjectsRemove = procedure of object;

  { TfraEditor }

  TfraEditor = class(TFrame)
  published
    PaintBox1: TPaintBox;
  private
    function Get_Alfa: Single;
    function Get_Fi: Single;
    function Get_X_Cam: Single;
    function Get_Y_Cam: Single;
    function GetZoom: Single;
    procedure SetEditorModified;
    procedure Set_Alfa(AValue: Single);
    procedure Set_Fi(AValue: Single);
    procedure Set_X_Cam(AValue: Single);
    procedure Set_X_offs(AValue: integer);
    function Get_X_Offs: integer;
    procedure Set_Y_Cam(AValue: Single);
    procedure Set_Y_Offs(AValue: integer);
    function Get_Y_Offs: integer;
    procedure SetZoom(AValue: Single);
  public
    objects : TEditorObjList;
    Editor  : TEditor;
    Modified   : Boolean;
    OnObjectsRemove: TOnObjectsRemove;   //when one or more objects are elminated
    procedure GraphicObjectAdd(argGraphicObject: TGraphicObj; AutoPos: boolean=true);
    procedure GraphicObjectDelete(argObj: TGraphicObj);
    procedure GraphicObjectsDeleteAll;
    procedure DeleteSelected;
    function ObjectAdd: TMyObject;
  public
    property xDesp: integer read Get_X_Offs write Set_X_offs;
    property yDesp: integer read Get_Y_Offs write Set_Y_Offs;
    property X_Cam: Single read Get_X_Cam write Set_X_Cam;
    property Y_Cam: Single read Get_Y_Cam write Set_Y_Cam;
    property Alfa: Single read Get_Alfa write Set_Alfa;
    property Fi: Single read Get_Fi write Set_Fi;
    property Zoom: Single read GetZoom write SetZoom;
  public
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}

procedure TfraEditor.GraphicObjectAdd(argGraphicObject: TGraphicObj; AutoPos: boolean = true);
{Add a graphic object to the editor. The graphic object must have been created previously,
and be of type TGraphicObj or a descendant. "AutoPos", allows automatic positioning
to the object on the screen, so that you avoid putting it always in the same position.}
var
  x: single;
  y: single;
begin
  Modified := True;        //Marca el editor como modificado
  //Posiciona tratando de que siempre aparezca en pantalla
  if AutoPos Then begin  //Se calcula posición
    x := Editor.VirtScreen.Xvirt(100, 100) + 30 * objects.Count Mod 400;
    y := Editor.VirtScreen.Yvirt(100, 100) + 30 * objects.Count Mod 400;
    argGraphicObject.PlaceAt(x,y);
  end;
  //configura eventos para ser controlado por este editor
  argGraphicObject.OnSelect   := @Editor.GraphicObject_Select;//reference to selection procedure
  argGraphicObject.OnDeselect := @Editor.GraphicObject_Unselect;
  argGraphicObject.OnCamPoint := @Editor.GraphicObject_SetPointer;
  objects.Add(argGraphicObject);
end;
procedure TfraEditor.GraphicObjectDelete(argObj: TGraphicObj);  //elimina un objeto grafico
begin
  Modified := True;
  argObj.Deselect;
  objects.Remove(argObj);
  argObj := nil;
  if OnObjectsRemove<>nil then OnObjectsRemove;
End;
procedure TfraEditor.GraphicObjectsDeleteAll;
//Remove all existing graphic objects
begin
  if objects.Count=0 then exit;
  //Remove
  Editor.SelectNone;
  objects.Clear;
  Editor.RestoreState;
  Modified := true;
  if OnObjectsRemove<>nil then OnObjectsRemove;
End;
procedure TfraEditor.DeleteSelected;
// Remove the selection.
var
  gobj: TGraphicObj;
  tmp: TOnObjectsRemove;
begin
  tmp := OnObjectsRemove;  //save event
  OnObjectsRemove := nil; //to avoid calling many times
  For gobj In Editor.selection  do  //explore all
    GraphicObjectDelete(gobj);
  OnObjectsRemove := tmp;  //restore
  if OnObjectsRemove<>nil then OnObjectsRemove;
  Editor.Refresh;
end;

function TfraEditor.ObjectAdd: TMyObject;
//Add an object of type TMiObject to the editor.
var obj: TMyObject;
begin
  obj := TMyObject.Create(Editor.VirtScreen);
  GraphicObjectAdd(obj);
  Result := obj;
end;

function TfraEditor.Get_X_Offs: integer;
begin
  Result := Editor.VirtScreen.x_offs;
end;
procedure TfraEditor.Set_X_offs(AValue: integer);
begin
  Editor.VirtScreen.x_offs:=AValue;
end;
function TfraEditor.Get_Y_Offs: integer;
begin
  Result := Editor.VirtScreen.y_offs;
end;
procedure TfraEditor.Set_Y_Offs(AValue: integer);
begin
  Editor.VirtScreen.y_offs:=AValue;
end;
function TfraEditor.Get_X_Cam: Single;
begin
  Result := Editor.VirtScreen.x_cam;
end;
procedure TfraEditor.Set_X_Cam(AValue: Single);
begin
  Editor.VirtScreen.x_cam:=AValue;
end;
function TfraEditor.Get_Y_Cam: Single;
begin
  Result := Editor.VirtScreen.y_cam;
end;
procedure TfraEditor.Set_Y_Cam(AValue: Single);
begin
  Editor.VirtScreen.y_cam:=AValue;
end;
function TfraEditor.GetZoom: Single;
begin
  Result := Editor.VirtScreen.Zoom;
end;
procedure TfraEditor.SetEditorModified;
{It is executed when the viewer reports changes (dimensioning, positioning, etc)
in any of the graphic objects.}
begin
  Modified := true;
end;
procedure TfraEditor.SetZoom(AValue: Single);
begin
  Editor.VirtScreen.Zoom:=AValue;
end;
function TfraEditor.Get_Alfa: Single;
begin
  Result := Editor.VirtScreen.Alfa;
end;
procedure TfraEditor.Set_Alfa(AValue: Single);
begin
  Editor.VirtScreen.Alfa := AValue;
end;
function TfraEditor.Get_Fi: Single;
begin
  REsult := Editor.VirtScreen.Fi;
end;
procedure TfraEditor.Set_Fi(AValue: Single);
begin
  Editor.VirtScreen.Fi := AValue;
end;
{procedure TEditor.KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//Procesa el evento KeyDown()
//var
//  v: TGraphicObj;
begin
  If Shift = [] Then begin  //********************* Teclas normales ***********************
      //If tec = 13 Then PropiedSeleccion ;  //Debe procesarlo el diagrama
      If Key = VK_DELETE Then DeleteSelected;  //DELETE
      If Key = 9 Then SelectNext;  //TAB
      If Key = 27 Then begin  //ESCAPE
          SelectNone;
          Refresh;
      end;
      If selection.Count = 0 Then     ;  //si no hay objects seleccionados
          If Key = 37 Then Call moveRight(OFFSET_EDITOR)        ;  //derecha
          If Key = 39 Then Call moveLeft(OFFSET_EDITOR)      ;  //izquierda
          If Key = 40 Then Call moveUp(OFFSET_EDITOR)         ;  //arriba
          If Key = 38 Then Call moveDown(OFFSET_EDITOR)          ;  //abajo
      Else        ;  //hay seleccionados
          If Key = 37 Then ;  //derecha
              For Each v In selection
                  If Not v.Bloqueado Then v.X = v.X - OFFSET_EDITOR
              Next
              Call Refresh
          End If
          If Key = 39 Then ;  //izquierda
              For Each v In selection
                  If Not v.Bloqueado Then v.X = v.X + OFFSET_EDITOR
              Next
              Call Refresh
          End If
          If Key = 40 Then ;  //arriba
              For Each v In selection
                  If Not v.Bloqueado Then v.Y = v.Y + OFFSET_EDITOR
              Next
              Call Refresh
          End If
          If Key = 38 Then ;  //abajo
              For Each v In selection
                  If Not v.Bloqueado Then v.Y = v.Y - OFFSET_EDITOR
              Next
              Call Refresh
          End If
      end If
  end else If Shift = [ssShift] Then begin //**********************Shift + ************************
      If Key = 9 Then Call SelectPrevious              ;  //TAB
  end else If Shift = [ssCtrl] Then begin  //**********************Ctrl + ************************
      If Key = 107 Then Call ZoomToClick      ;  //+
      If Key = 109 Then Call ZoomReduceClick      ;  //-
      If Key = 37 Then Call moveRight(DESPLAZ_MAYOR)   ;  //derecha
      If Key = 39 Then Call moveLeft(DESPLAZ_MAYOR) ;  //izquierda
      If Key = 40 Then Call moveUp(DESPLAZ_MAYOR)    ;  //arriba
      If Key = 38 Then Call moveDown(DESPLAZ_MAYOR)     ;  //abajo
  end else If Shift = [ssShift, ssCtrl] Then  begin  //******************Shift + Ctrl*************************
    picSal.MousePointer := vbSizeAll;  //indica modo Zoom + desplazamiento
  end;
end;}

constructor TfraEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  objects:= TEditorObjList.Create(true);  //lista de objects
  Editor := TEditor.Create(PaintBox1, objects);
  Editor.VirtScreen.Alfa:=0.7;
  Editor.VirtScreen.Fi:=0.7;
  Editor.OnModify:=@SetEditorModified;
end;

destructor TfraEditor.Destroy;
begin
  Editor.Destroy;
  objects.Destroy;
  inherited;
end;

end.

