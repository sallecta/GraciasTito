
unit uFrameEditor;

{$mode objfpc}{$H+}
interface

uses
  Classes, Forms, ExtCtrls,
  uDocEditor, uDocCore;

type
  TOnObjectsRemove = procedure of object;
  {Event for Mouse movement. Note that in addition to the coordinates of the mouse,
   provides virtual coordinates}
  TEvMousePaintBox = procedure(Shift: TShiftState; xp, yp: integer;
    xv, yv, zv: single) of object;

  { TframeEditor }

  TframeEditor = class(TFrame)
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
    DocCoreObjects: TDocCoreObjects;
    docEditor: TDocEditor;
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
    constructor Create(AOwner: TComponent; argDocCorePbjects: TDocCoreObjects);
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

procedure TframeEditor.documentObjectsDeleteAll;
begin
  if DocCoreObjects.Count = 0 then
    exit;
  docEditor.SelectNone;
  DocCoreObjects.Clear;
  docEditor.RestoreState;
  Modified := True;
  if OnObjectsRemove <> nil then
    OnObjectsRemove;
end;

function TframeEditor.Get_X_Offs: integer;
begin
  Result := docEditor.Canvas.x_offs;
end;

procedure TframeEditor.Set_X_Offs(AValue: integer);
begin
  docEditor.Canvas.x_offs := AValue;
end;

function TframeEditor.Get_Y_Offs: integer;
begin
  Result := docEditor.Canvas.y_offs;
end;

procedure TframeEditor.Set_Y_Offs(AValue: integer);
begin
  docEditor.Canvas.y_offs := AValue;
end;

function TframeEditor.Get_X_Cam: single;
begin
  Result := docEditor.Canvas.x_cam;
end;

procedure TframeEditor.Set_X_Cam(AValue: single);
begin
  docEditor.Canvas.x_cam := AValue;
end;

function TframeEditor.Get_Y_Cam: single;
begin
  Result := docEditor.Canvas.y_cam;
end;

procedure TframeEditor.Set_Y_Cam(AValue: single);
begin
  docEditor.Canvas.y_cam := AValue;
end;

function TframeEditor.GetZoom: single;
begin
  Result := docEditor.Canvas.Zoom;
end;

procedure TframeEditor.ExecuteCommand(command: string);
begin
  docEditor.ExecuteCommand(command);
end;

function TframeEditor.StateAsStr: string;
begin
  Result := docEditor.StateAsStr;
end;

procedure TframeEditor.Editor_ViewChange;
begin
  if OnChangePersp <> nil then
    OnChangePersp();
end;

procedure TframeEditor.Editor_Modified;
begin
  Modified := True;
end;

procedure TframeEditor.SetZoom(AValue: single);
begin
  docEditor.Canvas.Zoom := AValue;
end;

procedure TframeEditor.Editor_ChangeState(ViewState: TViewState);
begin
  if OnChangeState <> nil then
    OnChangeState(ViewState);
end;

procedure TframeEditor.Editor_MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  xv, yv: single;
begin
  docEditor.Canvas.XYvirt(X, Y, xv, yv);
  if OnMouseMoveVirt <> nil then
    OnMouseMoveVirt(Shift, X, Y, xv, yv, 0);
end;

procedure TframeEditor.Editor_SendMessage(msg: string);
begin
  if OnSendMessage <> nil then
    OnSendMessage(msg);
end;

function TframeEditor.Get_Alfa: single;
begin
  Result := docEditor.Canvas.Alfa;
end;

procedure TframeEditor.Set_Alfa(AValue: single);
begin
  docEditor.Canvas.Alfa := AValue;
end;

function TframeEditor.Get_Fi: single;
begin
  writeln('uframeeditor Get_Fi');
  Result := docEditor.Canvas.Fi;
end;

procedure TframeEditor.Set_Fi(AValue: single);
begin
  docEditor.Canvas.Fi := AValue;
end;

procedure TframeEditor.InitView;
{Locate the perspective and the axes, so that the origin (0,0) appears in the
lower left corner. Call when the frame has its final size}
begin
  docEditor.Canvas.Alfa := 0;
  docEditor.Canvas.Fi := 0;
  docEditor.Canvas.Zoom := 0.5;
  docEditor.Canvas.x_cam := ((PaintBox1.Width div 2) - 10) / docEditor.Canvas.Zoom;
  docEditor.Canvas.y_cam := ((PaintBox1.Height div 2) - 10) / docEditor.Canvas.Zoom;
end;

constructor TframeEditor.Create(AOwner: TComponent; argDocCorePbjects: TDocCoreObjects);
begin
  inherited Create(AOwner);
  DocCoreObjects := argDocCorePbjects;
  docEditor := TDocEditor.Create(PaintBox1, DocCoreObjects);
  docEditor.OnModify := @Editor_Modified;
  docEditor.OnChangeView := @Editor_ViewChange;
  docEditor.OnMouseMove := @Editor_MouseMove;
  docEditor.OnChangeState := @Editor_ChangeState;
  docEditor.OnSendMessage := @Editor_SendMessage;
end;

destructor TframeEditor.Destroy;
begin
  docEditor.Destroy;
  inherited;
end;

end.
