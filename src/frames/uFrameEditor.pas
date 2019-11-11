{                                frameVisCplex
This Frame will be used to place our graphic editor. Requires a TPaintBox
object, as graphic output. To work as an object editor graphics, you must
create an instance of "TModEdicion" and give it the reference of the PaintBox
Here you should also put the routines that allow you to add the various
graphic objects with which our application will work.

                                               By Tito Hinostroza 05/11/2014
}
unit uFrameEditor;
{$mode objfpc}{$H+}
interface

uses
  Classes, Forms, ExtCtrls,
  sketchDxf, sketchEditor, sketchCore;
type
  TOnObjectsRemove = procedure of object;

  { TFrameEditor }

  TFrameEditor = class(TFrame)
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
    procedure documentObjectsDeleteAll;
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

procedure TFrameEditor.GraphicObjectAdd(argGraphicObject: TGraphicObj; AutoPos: boolean = true);
{Add a graphic object to the editor. The graphic object must have been created previously,
and be of type TGraphicObj or a descendant. "AutoPos", allows automatic positioning
to the object on the screen, so that you avoid putting it always in the same position.}
var
  x: single;
  y: single;
begin
  Modified := True;
  if AutoPos Then begin
    x := Editor.VirtScreen.Xvirt(100, 100) + 30 * objects.Count Mod 400;
    y := Editor.VirtScreen.Yvirt(100, 100) + 30 * objects.Count Mod 400;
    argGraphicObject.PlaceAt(x,y);
  end;
  argGraphicObject.OnSelect   := @Editor.GraphicObject_Select;//reference to selection procedure
  argGraphicObject.OnDeselect := @Editor.GraphicObject_Unselect;
  argGraphicObject.OnCamPoint := @Editor.GraphicObject_SetPointer;
  objects.Add(argGraphicObject);
end;
procedure TFrameEditor.GraphicObjectDelete(argObj: TGraphicObj);
begin
  Modified := True;
  argObj.Deselect;
  objects.Remove(argObj);
  argObj := nil;
  if OnObjectsRemove<>nil then OnObjectsRemove;
End;
procedure TFrameEditor.documentObjectsDeleteAll;
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
procedure TFrameEditor.DeleteSelected;
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

function TFrameEditor.ObjectAdd: TMyObject;
//Add an object of type TMiObject to the editor.
var obj: TMyObject;
begin
  obj := TMyObject.Create(Editor.VirtScreen);
  GraphicObjectAdd(obj);
  Result := obj;
end;

function TFrameEditor.Get_X_Offs: integer;
begin
  Result := Editor.VirtScreen.x_offs;
end;
procedure TFrameEditor.Set_X_offs(AValue: integer);
begin
  Editor.VirtScreen.x_offs:=AValue;
end;
function TFrameEditor.Get_Y_Offs: integer;
begin
  Result := Editor.VirtScreen.y_offs;
end;
procedure TFrameEditor.Set_Y_Offs(AValue: integer);
begin
  Editor.VirtScreen.y_offs:=AValue;
end;
function TFrameEditor.Get_X_Cam: Single;
begin
  Result := Editor.VirtScreen.x_cam;
end;
procedure TFrameEditor.Set_X_Cam(AValue: Single);
begin
  Editor.VirtScreen.x_cam:=AValue;
end;
function TFrameEditor.Get_Y_Cam: Single;
begin
  Result := Editor.VirtScreen.y_cam;
end;
procedure TFrameEditor.Set_Y_Cam(AValue: Single);
begin
  Editor.VirtScreen.y_cam:=AValue;
end;
function TFrameEditor.GetZoom: Single;
begin
  Result := Editor.VirtScreen.Zoom;
end;
procedure TFrameEditor.SetEditorModified;
{It is executed when the viewer reports changes (dimensioning, positioning, etc)
in any of the graphic objects.}
begin
  Modified := true;
end;
procedure TFrameEditor.SetZoom(AValue: Single);
begin
  Editor.VirtScreen.Zoom:=AValue;
end;
function TFrameEditor.Get_Alfa: Single;
begin
  Result := Editor.VirtScreen.Alfa;
end;
procedure TFrameEditor.Set_Alfa(AValue: Single);
begin
  Editor.VirtScreen.Alfa := AValue;
end;
function TFrameEditor.Get_Fi: Single;
begin
  REsult := Editor.VirtScreen.Fi;
end;
procedure TFrameEditor.Set_Fi(AValue: Single);
begin
  Editor.VirtScreen.Fi := AValue;
end;

constructor TFrameEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  objects:= TEditorObjList.Create(true);
  Editor := TEditor.Create(PaintBox1, objects);
  Editor.VirtScreen.Alfa:=0.7;
  Editor.VirtScreen.Fi:=0.7;
  Editor.OnModify:=@SetEditorModified;
end;

destructor TFrameEditor.Destroy;
begin
  Editor.Destroy;
  objects.Destroy;
  inherited;
end;

end.

