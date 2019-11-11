{
This Frame will be used to place our graphic editor. Includes a PaintBox,
as graphic output.
Being a frame, it can be included in any form.
}
unit uFrameEditor;

{$mode objfpc}{$H+}
interface

uses
  Classes, Forms, ExtCtrls,
  sketchEditor, sketchCore;

type
  TOnObjectsRemove = procedure of object;
  {Event for Mouse movement. Note that in addition to the coordinates of the mouse,
   provides virtual coordinates}
  TEvMousePaintBox = procedure(Shift: TShiftState; xp, yp: integer;
    xv, yv, zv: single) of object;

  { TFramePaintBox }

  TFramePaintBox = class(TFrame)
  published
    PaintBox1: TPaintBox;
  private
    function Get_Alfa: single;
    function Get_Fi: single;
    function Get_X_Cam: single;
    function Get_Y_Cam: single;
    function GetZoom: single;
    procedure Editor_ViewChange;
    procedure Editor_Modified;
    procedure Set_Alfa(AValue: single);
    procedure Set_Fi(AValue: single);
    procedure Set_X_Cam(AValue: single);
    procedure Set_X_Offs(AValue: integer);
    function Get_X_Offs: integer;
    procedure Set_Y_Cam(AValue: single);
    procedure Set_Y_Offs(AValue: integer);
    function Get_Y_Offs: integer;
    procedure SetZoom(AValue: single);
    procedure Editor_ChangeState(ViewState: TViewState);
    procedure Editor_MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure Editor_SendMessage(msg: string);
  public
    objects: TEditorObjList;
    Editor: TEditor;
    Modified: boolean;
    OnObjectsRemove: TOnObjectsRemove;
    OnChangePersp:
    procedure of object;
    OnMouseMoveVirt: TEvMousePaintBox;
    OnChangeState: TEvChangeState;
    OnSendMessage: TEvSendMessage;
    procedure documentObjectsDeleteAll;
  public
    property X_Offs: integer read Get_X_Offs write Set_X_Offs;
    property Y_Offs: integer read Get_Y_Offs write Set_Y_Offs;
    property X_Cam: single read Get_X_Cam write Set_X_Cam;
    property Y_Cam: single read Get_Y_Cam write Set_Y_Cam;
    property Alfa: single read Get_Alfa write Set_Alfa;
    property Fi: single read Get_Fi write Set_Fi;
    property Zoom: single read GetZoom write SetZoom;
    procedure ExecuteCommand(command: string);
    function StateAsStr: string;
  public
    procedure InitView;
    constructor Create(AOwner: TComponent; ListObjGraf: TEditorObjList);
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

procedure TFramePaintBox.documentObjectsDeleteAll;
begin
  if objects.Count = 0 then
    exit;
  Editor.SelectNone;
  objects.Clear;
  Editor.RestoreState;
  Modified := True;
  if OnObjectsRemove <> nil then
    OnObjectsRemove;
end;

function TFramePaintBox.Get_X_Offs: integer;
begin
  Result := Editor.VirtScreen.x_offs;
end;

procedure TFramePaintBox.Set_X_Offs(AValue: integer);
begin
  Editor.VirtScreen.x_offs := AValue;
end;

function TFramePaintBox.Get_Y_Offs: integer;
begin
  Result := Editor.VirtScreen.y_offs;
end;

procedure TFramePaintBox.Set_Y_Offs(AValue: integer);
begin
  Editor.VirtScreen.y_offs := AValue;
end;

function TFramePaintBox.Get_X_Cam: single;
begin
  Result := Editor.VirtScreen.x_cam;
end;

procedure TFramePaintBox.Set_X_Cam(AValue: single);
begin
  Editor.VirtScreen.x_cam := AValue;
end;

function TFramePaintBox.Get_Y_Cam: single;
begin
  Result := Editor.VirtScreen.y_cam;
end;

procedure TFramePaintBox.Set_Y_Cam(AValue: single);
begin
  Editor.VirtScreen.y_cam := AValue;
end;

function TFramePaintBox.GetZoom: single;
begin
  Result := Editor.VirtScreen.Zoom;
end;

procedure TFramePaintBox.ExecuteCommand(command: string);
begin
  Editor.ExecuteCommand(command);
end;

function TFramePaintBox.StateAsStr: string;
begin
  Result := Editor.StateAsStr;
end;

procedure TFramePaintBox.Editor_ViewChange;
begin
  if OnChangePersp <> nil then
    OnChangePersp();
end;

procedure TFramePaintBox.Editor_Modified;
begin
  Modified := True;
end;

procedure TFramePaintBox.SetZoom(AValue: single);
begin
  Editor.VirtScreen.Zoom := AValue;
end;

procedure TFramePaintBox.Editor_ChangeState(ViewState: TViewState);
begin
  if OnChangeState <> nil then
    OnChangeState(ViewState);
end;

procedure TFramePaintBox.Editor_MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  xv, yv: single;
begin
  Editor.VirtScreen.XYvirt(X, Y, 0, xv, yv);
  if OnMouseMoveVirt <> nil then
    OnMouseMoveVirt(Shift, X, Y, xv, yv, 0);
end;

procedure TFramePaintBox.Editor_SendMessage(msg: string);
begin
  if OnSendMessage <> nil then
    OnSendMessage(msg);
end;

function TFramePaintBox.Get_Alfa: single;
begin
  Result := Editor.VirtScreen.Alfa;
end;

procedure TFramePaintBox.Set_Alfa(AValue: single);
begin
  Editor.VirtScreen.Alfa := AValue;
end;

function TFramePaintBox.Get_Fi: single;
begin
  Result := Editor.VirtScreen.Fi;
end;

procedure TFramePaintBox.Set_Fi(AValue: single);
begin
  Editor.VirtScreen.Fi := AValue;
end;

procedure TFramePaintBox.InitView;
{Locate the perspective and the axes, so that the origin (0,0) appears in the
lower left corner. Call when the frame has its final size}
begin
  Editor.VirtScreen.Alfa := 0;
  Editor.VirtScreen.Fi := 0;
  Editor.VirtScreen.Zoom := 0.5;
  //Locate (0,0) to 10 pixels from the bottom left corner
  Editor.VirtScreen.x_cam := ((PaintBox1.Width div 2) - 10) / Editor.VirtScreen.Zoom;
  Editor.VirtScreen.y_cam := ((PaintBox1.Height div 2) - 10) / Editor.VirtScreen.Zoom;
end;

constructor TFramePaintBox.Create(AOwner: TComponent; ListObjGraf: TEditorObjList);
begin
  inherited Create(AOwner);
  objects := ListObjGraf;
  Editor := TEditor.Create(PaintBox1, objects);
  Editor.OnModify := @Editor_Modified;
  Editor.OnChangeView := @Editor_ViewChange;
  Editor.OnMouseMove := @Editor_MouseMove;
  Editor.OnChangeState := @Editor_ChangeState;
  Editor.OnSendMessage := @Editor_SendMessage;
end;

destructor TFramePaintBox.Destroy;
begin
  Editor.Destroy;
  inherited;
end;

end.
