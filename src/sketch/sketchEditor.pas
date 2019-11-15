
unit sketchEditor;
{$mode objfpc}{$H+}
INTERFACE
uses
  Classes, Controls, ExtCtrls, Graphics, LCLProc, LCLType, fgl,
  MotGraf3d, sketchCore, sketchDxf;
const
  ZOOM_MAX_EDITOR = 5  ;//Defines the maximal zoom allowed in a diagram
  ZOOM_MIN_EDITOR = 0.1;//Defines the minimal zoom allowed in a diagram

  ZOOM_STEP_EDITOR = 1.15;
  OFFSET_EDITOR = 10;
type
  //ObjType of event produced in the View
  TViewEvent = (
    vmeMouseDown,
    vmeMouseMove,
    vmeMouseUp,
    vmeCmdStart    //Start of command
  );
  {ObjType of the event handler of the View. Only mouse or event events are expected.}
  TViewEventHandler = procedure(EventType: TViewEvent; Button: TMouseButton;
                            Shift: TShiftState; xp, yp: Integer; txt: string) of object;

  //Point States
  TViewState = (
      //Visual editor states
       VS_NORMAL,      //No operation is being performed
      VS_SELECTINGMULT,   //You are in multiple selection mode
      VS_OBJS_MOVING,    //Indicates that one or more objects are moving
      VS_SCREEN_SCROLLING,
      VS_SCREEN_ANG,   //Indicates offset of View angles
      VS_DIMEN_OBJ,   // Indicates that an object is being dimensioned
      VS_ZOOMING,    //Indicates that you are in a Zoom Processing
      //Additional states for commands
      VS_CMD_ADDING_LINE,
      VS_CMD_ADDING_RECTAN
      );

  TOnClickRight = procedure(x,y:integer) of object;
  TEvChangeState = procedure(ViewState: TViewState) of object;
  TEvSendMessage = procedure(msg: string) of object;

  { TEditor }
  TEditor = class
  private
    FState: TViewState;
    procedure GraphicObjectAdd(argGraphicObject: TGraphicObj; AutoPos: boolean=true);
    procedure GraphicObjectDelete(obj: TGraphicObj);
    procedure DeleteSelected;
    procedure proc_COMM_RECTAN(EventType: TViewEvent; Button: TMouseButton;
      Shift: TShiftState; xp, yp: Integer; txt: string);
    procedure SetState(AValue: TViewState);
    procedure VirtualScreen_ChangeView;
  protected
    PBox         : TPaintBox;   //Output Control
    MovingObject: TGraphicObj;    //reference to object that captured movement
    MarkedObject   : TGraphicObj;
    Moving : Boolean;     //Control flag for the start of the movement
    procedure PBox_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
                        xp, yp: Integer); virtual;
    procedure PBox_MouseUp(Sender: TObject; Button: TMouseButton;Shift: TShiftState; xp, yp: Integer);
    procedure PBox_MouseMove(Sender: TObject; Shift: TShiftState; X,  Y: Integer); virtual;
    procedure PBox_Paint(Sender: TObject);
    procedure PBox_MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PBox_DblClick(Sender: TObject);
    procedure PBox_Resize(Sender: TObject);
  public
    procedure ExecuteCommand(command: string);
  public
    OnClickRight  : TOnClickRight;
    OnMouseUp   : TMouseEvent;
    OnMouseDown : TMouseEvent;
    OnMouseMove : TMouseMoveEvent;
    OnDblClick  : TNotifyEvent;
    OnObjectsMoved: procedure of object;
    OnChangeView: procedure of object;
    OnModify     : procedure of object;
    OnChangeState: TEvChangeState;
    OnSendMessage: TEvSendMessage;
  public
    xvPt       : Single;
    yvPt       : Single;
    zvPt       : Single;
    objects     : TEditorObjList;
    selection   : TEditorObjList;
    VirtScreen         : TVirtScreen;    //graphic output
    Wheel_step    : Single;
    ShowAxes : boolean;
    AxesDistance : integer;
    ShowRotPoint: boolean;
    ShowGrid  : boolean;
    function ObjSelected: TGraphicObj;
    function ObjByName(argName: string): TGraphicObj;
    procedure Refresh;
    procedure SelectAll;
    procedure SelectNone();
  protected
    x1Sel    : integer;
    y1Sel    : integer;
    x2Sel    : integer;
    y2Sel    : integer;
    x1Sel_prev  : integer;
    y1Sel_prev  : integer;
    x2Sel_prev  : integer;
    y2Sel_prev  : integer;
    //mouse coordinates
    x_mouse: integer;
    y_mouse: integer;
    x_cam_prev: Single;  //previous coordinates of x_cam
    y_cam_prev: Single;
    procedure ZoomToClick(factor: real=ZOOM_STEP_EDITOR; xr: integer=0;
      yr: integer=0);
    function PreviousVisible(c: TGraphicObj): TGraphicObj;
    procedure DrawRectangleSeleccion;

    function inRectangleSeleccion(X, Y: Single): Boolean;
    procedure startRectangleSeleccion(X, Y: Integer);
    procedure moveDown(offs: Double=OFFSET_EDITOR);
    procedure moveUp(offs: Double=OFFSET_EDITOR);
    procedure moveRight(offs: Double=OFFSET_EDITOR);
    procedure moveLeft(offs: Double=OFFSET_EDITOR);
    procedure Displace(dx, dy: integer);
    function VisibleObjectsCount: Integer;
    function FirstVisible: TGraphicObj;
    function RectangleSelectionIsNil: Boolean;
    procedure ZoomReduceClick(factor: Real=ZOOM_STEP_EDITOR; x_zoom: Real=0;
      y_zoom: Real=0);
    function SelectObjectAt(xp, yp: Integer): TGraphicObj;
    procedure SelectPrevious;
    procedure SelectNext;
    function NextVisible(c: TGraphicObj): TGraphicObj;
    function LastVisible: TGraphicObj;
    function VerifyMouseMovement(X, Y: Integer): TGraphicObj;
    procedure VerifyToMove(xp, yp: Integer);
  public
    procedure GraphicObject_Select(obj: TGraphicObj);     //Response to Event
    procedure GraphicObject_Unselec(obj: TGraphicObj);    //Response to Event
    procedure GraphicObject_SetPointer(argPoint: integer);  //Response to Event
  private
    {Container that associates the state with its handling procedure. Use to access
      quickly to the management routine, since some events (like PBox_MouseMove),
      generate repeatedly.}
    EventOfState: array[low(TViewState) .. high(TViewState)] of TViewEventHandler;
    property State: TViewState read FState write SetState;
    public function StateAsStr: string; private
    procedure RegisterState(argState: TViewState; EventHandler: TViewEventHandler);
    procedure ClearEventState;
    procedure SendData(Data: string);
    procedure CallEventState(argState: TViewState; EventType: TViewEvent;
      Button: TMouseButton; Shift: TShiftState; xp, yp: Integer; txt: string);
  private
    procedure proc_CMD_ADDING_LINE(EventType: TViewEvent; Button: TMouseButton;
                            Shift: TShiftState; xp, yp: Integer; txt: string);
    procedure proc_NORMAL(EventType: TViewEvent; Button: TMouseButton;
                            Shift: TShiftState; xp, yp: Integer; txt: string);
    procedure proc_SELECTINGMULT(EventType: TViewEvent; Button: TMouseButton;
                            Shift: TShiftState; xp, yp: Integer; txt: string);
    procedure proc_OBJS_MOVING(EventType: TViewEvent; Button: TMouseButton;
                            Shift: TShiftState; xp, yp: Integer; txt: string);
    procedure proc_SCREEN_SCROLLING(EventType: TViewEvent; Button: TMouseButton;
                            Shift: TShiftState; xp, yp: Integer; txt: string);
    procedure proc_SCREEN_ANG(EventType: TViewEvent; Button: TMouseButton;
                            Shift: TShiftState; xp, yp: Integer; txt: string);
    procedure proc_DIMEN_OBJ(EventType: TViewEvent; Button: TMouseButton;
                            Shift: TShiftState; xp, yp: Integer; txt: string);
    procedure proc_ZOOMING(EventType: TViewEvent; Button: TMouseButton;
                            Shift: TShiftState; xp, yp: Integer; txt: string);
  public //Inicialización
    procedure RestoreState(msg: string='');
    constructor Create(PB0: TPaintBox; objectList: TEditorObjList);
    destructor Destroy; override;
  end;

implementation
uses glob;

procedure TEditor.SetState(AValue: TViewState);
begin
  if FState=AValue then Exit;
  FState:=AValue;
  if OnChangeState<>nil then OnChangeState(FState);
end;
procedure TEditor.VirtualScreen_ChangeView;
begin
  if OnChangeView<>nil then OnChangeView;
end;
procedure TEditor.PBox_MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; xp, yp: Integer);
begin
    if OnMouseDown<>nil then OnMouseDown(Sender, Button, Shift, Xp, Yp);
    x_mouse := xp;
    y_mouse := yp;
    //Prepares start of scrolling the screen.
    x_cam_prev := VirtScreen.x_cam;
    y_cam_prev := VirtScreen.y_cam;

    CallEventState(State, vmeMouseDown, Button, Shift, xp, yp, ''); //Process according to state
end;
procedure TEditor.PBox_MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; xp, yp: Integer);
begin
   //Check if the selection is NULL
   If (State = VS_SELECTINGMULT) And RectangleSelectionIsNil Then State := VS_NORMAL;
   CallEventState(State, vmeMouseUp, Button, Shift, xp, yp, '');
   if Button = mbRight then
     if OnClickRight<> nil then OnClickRight(xp,yp);
   if OnMouseUp<>nil then OnMouseUp(Sender, Button, Shift, xp, yp);
end;
procedure TEditor.PBox_MouseMove(Sender: TObject; Shift: TShiftState;
  X,  Y: Integer);
begin
  zvPt := 0;   //we set the work plane at z = 0
  VirtScreen.XYvirt(X,Y,zvPt, xvPt, yvPt);
  if OnMouseMove<>nil then OnMouseMove(Sender, Shift, X, Y);
  if Moving = True Then VerifyToMove(X, Y);
  CallEventState(State, vmeMouseMove, mbExtra1, Shift, x, y, ''); //Process according to state
end;
procedure TEditor.PBox_Paint(Sender: TObject);
var
  o:TGraphicObj;
  x, y, xGrid1, xGrid2, yGrid1, yGrid2: Single;
  nGrid, ix, distCovered, step: Integer;
begin
    VirtScreen.Clear;
    If State = VS_SELECTINGMULT Then DrawRectangleSeleccion;
    if ShowGrid then begin
      //Shows grid
      VirtScreen.SetPen(TColor($404040),1);
      if VirtScreen.Zoom > 7 then begin
        distCovered := 100;  //covered distance (virtual value)
        step := 10;      //step width (virtual value)
      end else if VirtScreen.Zoom > 3 then begin
        distCovered := 200;
        step := 20;
      end else if VirtScreen.Zoom > 1 then begin
        distCovered := 600;
        step := 50;
      end else begin
        distCovered := 1200;
        step := 100;
      end;
      nGrid := distCovered div step;
      xGrid1 := int((VirtScreen.x_cam - distCovered/2)/step)*step;
      xGrid2 := xGrid1 + distCovered;
      yGrid1 := int((VirtScreen.y_cam - distCovered/2)/step)*step;
      yGrid2 := yGrid1 + distCovered;

      x := xGrid1;
      for ix := 0 to nGrid do begin
        VirtScreen.Line(x,yGrid1,0, x, yGrid2, 0);
        x := x + step;
      end;
      y := yGrid1;
      for ix := 0 to nGrid do begin
        VirtScreen.Line(xGrid1, y, 0, xGrid2, y, 0);
        y := y + step;
      end;
    end;
    //Draw objects
    for o In objects do begin
      o.Draw;
    end;
    //Draw axis
    if ShowAxes then begin
      VirtScreen.SetPen(clRed, 1);
      VirtScreen.Line(0,0,0,100,0,0);
      VirtScreen.Line(0,0,0,0,100,0);
      VirtScreen.Line(0,0,0,0,0,100);
      VirtScreen.Texto(100,10,0,'x');
      VirtScreen.Texto(0,100,0,'y');
    end;
    if ShowRotPoint then begin
      x := VirtScreen.x_cam;
      y := VirtScreen.y_cam;
      VirtScreen.SetPen(clGreen, 1);
      VirtScreen.Line(x-30,y,0,  x+30,y,0);
      VirtScreen.Line(x, y-30,0, x, y+30,0);
    end;
end;
procedure TEditor.PBox_MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  d: Single;
begin
  if Shift = [ssCtrl] then begin
    if WheelDelta>0 then d := Wheel_step else d := -Wheel_step;
    VirtScreen.Alfa := VirtScreen.Alfa + d;
  end;
  if Shift = [ssShift] then begin
    if WheelDelta>0 then d := Wheel_step else d := -Wheel_step;
    VirtScreen.Fi := VirtScreen.Fi + d;
  end;
  if Shift = [] then begin
    if WheelDelta>0 then VirtScreen.Zoom:=VirtScreen.Zoom*1.2
    else VirtScreen.Zoom:=VirtScreen.Zoom/1.2;
  end;
  Refresh;
end;
procedure TEditor.PBox_DblClick(Sender: TObject);
begin
  if OnDblClick<>nil then OnDblClick(Sender);
end;
procedure TEditor.PBox_Resize(Sender: TObject);
{It is used to set the rotation point to the center of the control.}
begin
  VirtScreen.x_offs := PBox.Width div 2;
  VirtScreen.y_offs := PBox.Height div 2;
end;
procedure TEditor.ExecuteCommand(command: string);
{Request to execute, a command to the viewer. This must be the only means, in addition to the
mouse events, by which actions are communicated to the viewer. Seen in this way,
ExecuteCommand (), is similar to the event handler routines: PBOX _ ??? (), with the
exception that it is not executed, in response to a mouse event, but must be
externally called }
begin
  {Pass the command event, to the routine corresponding to the current state.
   The only state that this command should treat, would be the NORMAL state. }
  CallEventState(State, vmeCmdStart, mbExtra1, [], 0, 0, command); //Process according to state
end;

procedure TEditor.Refresh();  //   Optional s: TGraphicObj = Nothing
begin
  PBox.Invalidate;
end;
function TEditor.SelectObjectAt(xp, yp: Integer): TGraphicObj;
var
  i: Integer;
  s: TGraphicObj;
begin
  Result := NIL;
  For i := selection.Count-1 downTo 0 do begin
    s := selection[i];
    If not s.SelLocked and s.isSelected(xp, yp) Then begin
        Result:= s;
        Exit;
    End;
  end;
  For i := objects.Count-1 downTo 0 do begin
    s := objects[i];
    If not s.SelLocked and s.isSelected(xp, yp) Then begin
        Result := s;
        Exit;
    End;
  end;
End;
procedure TEditor.VerifyToMove(xp, yp: Integer);
{If the movement starts, first select an element that
could be below the pointer and update "EstPuntero".
It should only be executed once at the beginning of the movement, for this purpose
use the Moving flag, which should be set to FALSE here.}
var s: TGraphicObj;
begin
    for s In selection  do begin
      if s.PosLocked then continue;
      s.StartMove(xp, yp);
      if s.Processing Then begin
          MovingObject := s;
          if s.Resizing then State := VS_DIMEN_OBJ else State := VS_NORMAL;
          Moving := False;
          Exit;
      end;
    end;
    for s In objects do begin
      if s.PosLocked then continue;
      s.StartMove(xp, yp);
      if s.Processing Then begin
          MovingObject := s;
          if s.Resizing then State := VS_DIMEN_OBJ else State := VS_NORMAL;
          State := VS_NORMAL;
          Moving := False;
          exit;
      end;
    end;
{No object has captured, the event, we assume that the
Simple scrolling of selected objects Debug.Print "VerifParaMover: VS_OBJS_MOVING" }
    State := VS_OBJS_MOVING;
    MovingObject := nil;
    Moving := False;
End;
function TEditor.VerifyMouseMovement(X, Y: Integer): TGraphicObj;
{It animates the marking of the objects when the mouse passes over them
Returns reference to the object through which the cirsor passes    }
var s: TGraphicObj;
begin

    s := SelectObjectAt(X, Y);
    Result := s;
    //Se refresca la pantalla optimizando
    If s = NIL Then begin
      If MarkedObject <> NIL Then begin
            MarkedObject.Marked := False;
            MarkedObject := NIL;
            Refresh;
        End;
      PBox.Cursor := crDefault;
    end
    Else begin
      If MarkedObject = NIL Then begin
         MarkedObject := s;
         s.Marked := True;
         Refresh;
      end Else begin
           If MarkedObject = s Then
           Else begin
               MarkedObject.Marked := False;
               MarkedObject := s ;
               s.Marked := True;
               Refresh;
           End;
        End;
    End;

End;
//***********Functions to manage the visible elements and selection by keyboard**********
function TEditor.VisibleObjectsCount: Integer;
//returns the number of visible objects
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
function TEditor.FirstVisible: TGraphicObj;
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
function TEditor.LastVisible: TGraphicObj;
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
function TEditor.NextVisible(c: TGraphicObj): TGraphicObj;
var
  i: Integer;
begin
    For i := 0 To objects.Count-1 do begin
      if objects[i] = c Then break;
    end;
    repeat
      Inc(i);
      If i >= objects.Count Then begin
        Result := FirstVisible;
        Exit;
      end;
    until objects[i].visible;
    Result := objects[i];
end;
function TEditor.PreviousVisible(c: TGraphicObj): TGraphicObj;
var
  i: Integer;
begin
    For i := 0 To objects.Count-1 do begin
      If objects[i] = c Then break;
    end;
    repeat
      Dec(i);
      If i < 0 Then begin
        Result := LastVisible;
        Exit;
      End;
    until objects[i].visible;
    Result := objects[i];
End;
procedure TEditor.SelectNext;
var
  s: TGraphicObj;
begin
    if VisibleObjectsCount() = 0 Then exit;
    if selection.Count = 1 Then begin
        s := selection[0];
        s := NextVisible(s);
        SelectNone;
        s.Select;
    end else begin
        s := FirstVisible;
        SelectNone;
        s.Select;
    end;
    Refresh;
end;
procedure TEditor.SelectPrevious;
var
  s: TGraphicObj;
begin
    if VisibleObjectsCount() = 0 Then exit;
    if selection.Count = 1 then begin
        s := selection[0];
        s := PreviousVisible(s);
        SelectNone;
        s.Select;
    end else begin
        s := LastVisible;
        SelectNone;
        s.Select;
    end;
    Refresh;
end;
//******************* Display functions **********************
procedure TEditor.ZoomToClick(factor: real = ZOOM_STEP_EDITOR;
                        xr: integer = 0; yr: integer = 0);
var screen_width: Real ;
    screen_height: Real ;
    x_zoom, y_zoom: Single;
begin
    If VirtScreen.zoom < ZOOM_MAX_EDITOR Then
        VirtScreen.zoom := VirtScreen.zoom * factor;
    If (xr <> 0) Or (yr <> 0) Then begin  //a central coordinate has been specified
        screen_width := PBox.width / VirtScreen.zoom;
        screen_height := PBox.Height / VirtScreen.zoom;
        VirtScreen.XYvirt(xr, yr, 0, x_zoom, y_zoom);     //convert
        VirtScreen.SetWindow(PBox.Width, PBox.Height,
                x_zoom - screen_width / 2, x_zoom + screen_width / 2, y_zoom - screen_height / 2, y_zoom + screen_height / 2);
    End;
    Refresh;
End;
procedure TEditor.ZoomReduceClick(factor: Real = ZOOM_STEP_EDITOR;
                        x_zoom: Real = 0; y_zoom: Real = 0);
begin
    If VirtScreen.zoom > ZOOM_MIN_EDITOR Then
        VirtScreen.zoom := VirtScreen.zoom / factor;
    Refresh;
End;
///////////////////////// Selection functions/////////////////////////////
procedure TEditor.SelectAll;
var obj: TGraphicObj;
begin
    For obj In objects do obj.Select;
End;
procedure TEditor.SelectNone();
var s: TGraphicObj;
begin
  For s In objects do
    if s.Selected then s.Deselect;
End;
function  TEditor.ObjSelected: TGraphicObj;
//Returns the ObjSelected object. If there is no ObjSelected, it returns NIL.
begin
  Result := nil;
  if selection.Count = 0 then exit;
  //there is at least one
  Result := selection[selection.Count-1];  //returns the only or last
End;
function  TEditor.ObjByName(argName: string): TGraphicObj;
var s: TGraphicObj;
begin
  Result := nil;
  if argName = '' then exit;
  For s In objects do
    if s.Name = argName then begin
       Result := s;
       break;
    end;
End;

procedure TEditor.moveDown(offs: Double = OFFSET_EDITOR) ;
{It generates a displacement on the screen making it independent of the
current expansion factor}
var
    z: Single ;  //zoom
begin
    z := VirtScreen.zoom;
    Displace(0, round(offs / z));
    Refresh;
end;
procedure TEditor.moveUp(offs: Double = OFFSET_EDITOR) ;
{
 It generates a displacement on the screen making it independent of the
  current expansion factor
}
var
    z: Single ;  //zoom
begin
    z := VirtScreen.zoom;
    Displace(0, round(-offs / z));
    Refresh;
end;
procedure TEditor.moveRight(offs: Double = OFFSET_EDITOR) ;
{
It generates a displacement on the screen making it independent of the
current expansion factor
}
var
    z: Single ;  //zoom
begin
    z := VirtScreen.zoom;
    Displace(round(offs / z), 0);
    Refresh;
end;
procedure TEditor.moveLeft(offs: Double = OFFSET_EDITOR) ;
{
It generates a displacement on the screen making it independent of the
  current expansion factor
}
var
    z: Single ;  //zoom
begin
    z := VirtScreen.zoom;
    Displace(round(-offs / z), 0);
    Refresh;
end;
procedure TEditor.Displace(dx, dy: integer);
begin
{
"Standard" procedure to scroll the screen
  Varies the parameters of the perspective "x_cam" and "y_cam"
}
    VirtScreen.Displace(dx, dy);
end;
//Modification of objects
procedure TEditor.GraphicObjectAdd(argGraphicObject: TGraphicObj; AutoPos: boolean = true);
{
Add a graphic object to the editor. The graphic object must have been created previously,
  and be of Type TGraphicObj or a descendant. "AutoPos", allows automatic positioning
  to the object on the screen, so that you avoid putting it always in the same position.
}
var
  x: single;
  y: single;
begin
  if OnModify<>nil then OnModify;
  //Position trying to always appear on the screen
  if AutoPos Then begin  //Position is calculated
    x := VirtScreen.Xvirt(100, 100) + 30 * objects.Count Mod 400;
    y := VirtScreen.Yvirt(100, 100) + 30 * objects.Count Mod 400;
    argGraphicObject.PlaceAt(x,y);
  end;
  //configure events to be controlled by this editor
  argGraphicObject.OnSelect   := @GraphicObject_Select;
  argGraphicObject.OnDeselect := @GraphicObject_Unselec;
  argGraphicObject.OnCamPoint := @GraphicObject_SetPointer;
  objects.Add(argGraphicObject);
end;
procedure TEditor.GraphicObjectDelete(obj: TGraphicObj);
begin
  obj.Deselect;
  objects.Remove(obj);
  obj := nil;
  if OnModify<>nil then OnModify;
End;
procedure TEditor.DeleteSelected;
var
  v: TGraphicObj;
begin
  For v In selection  do  //explore all
    GraphicObjectDelete(v);
  if OnModify<>nil then OnModify;
  Refresh;
end;

/////////////////////////  Selection Rectangle Functions/////////////////////////
procedure TEditor.DrawRectangleSeleccion();
//Draw the selection rectangle on the screen
begin
    VirtScreen.SetPen(clGreen, 1, psDot);
    VirtScreen.rectang0(x1Sel, y1Sel, x2Sel, y2Sel);

    x1Sel_prev := x1Sel; y1Sel_prev := y1Sel;
    x2Sel_prev := x2Sel; y2Sel_prev := y2Sel;
End;
procedure TEditor.startRectangleSeleccion(X, Y: Integer);
//Start the selection rectangle, with the coordinates
begin
    x1Sel:= X; y1Sel := Y;
    x2Sel := X; y2Sel := Y;
    x1Sel_prev := x1Sel;
    y1Sel_prev := y1Sel;
    x2Sel_prev := x2Sel;
    y2Sel_prev := y2Sel;
End;
function TEditor.RectangleSelectionIsNil: Boolean;
 //Indicates whether the selection rectangle is NULL or negligible size
begin
    If (x1Sel = x2Sel) And (y1Sel = y2Sel) Then
        RectangleSelectionIsNil := True
    Else
        RectangleSelectionIsNil := False;
End;
function TEditor.inRectangleSeleccion(X, Y: Single): Boolean;
//Returns true if (x, y) is inside the selection rectangle.
var xMin, xMax: Integer;
    yMin, yMax: Integer;
    xx1, yy1: Single;
    xx2, yy2: Single;
begin
    //get minimum and maximum coordinates
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

    VirtScreen.XYvirt(xMin, yMin, 0, xx1, yy1);
    VirtScreen.XYvirt(xMax, yMax, 0, xx2, yy2);

    //check if you are in region
    If (X >= xx1) And (X <= xx2) And (Y >= yy1) And (Y <= yy2) Then
        inRectangleSeleccion := True
    Else
        inRectangleSeleccion := False;
End;
//////////////////  "TGraphicObj" Events  ///////////////////////
procedure TEditor.GraphicObject_Select(obj: TGraphicObj);
{
Add a graphic object to the "selection" list. This method should not be called directly.
  If you want to select an object you must use the object.Select form.
}
begin
  selection.Add(obj);
End;
procedure TEditor.GraphicObject_Unselec(obj: TGraphicObj);
{
Remove a graphic object from the "selection" list. This method should not be called directly.
  If you want to remove the selection from an object, you must use the object.Select form.
}
begin
//    If not obj.ObjSelected Then Exit;
  selection.Remove(obj);
End;
procedure TEditor.GraphicObject_SetPointer(argPoint: integer);
{
Procedure that changes the mouse pointer. It is used to provide the "TGraphicObj" objects
  the possibility of changing the pointer.
}
begin
  PBox.Cursor := argPoint;        //define cursor
end;
function TEditor.StateAsStr: string;
{It must state as a descriptive string. It is necessary to update the desciprción
for each new state that is being added.}
begin
  case State of
  VS_NORMAL      : Result := msg.get('stateNormal');
  VS_SELECTINGMULT   : Result := msg.get('stateSelMultiple');
  VS_OBJS_MOVING    : Result := msg.get('stateObjectsMoving');
  VS_SCREEN_SCROLLING   : Result := msg.get('stateScreenScrolling');
  VS_SCREEN_ANG    : Result := msg.get('stateScreenRotating');
  VS_DIMEN_OBJ   : Result := msg.get('stateDimensioningObjects');
  VS_ZOOMING    : Result := msg.get('stateMouseZooming');
  VS_CMD_ADDING_LINE   : Result := msg.get('stateLineCreating');
  VS_CMD_ADDING_RECTAN : Result := msg.get('stateRectangleCreating');
  else
    Result := msg.get('stateUnknown');
  end;
end;
procedure TEditor.RegisterState(argState: TViewState;
  EventHandler: TViewEventHandler);
{Register a new mouse state}
begin
  EventOfState[argState] := EventHandler;
end;
procedure TEditor.ClearEventState;
var
  st: TViewState;
begin
  for st := low(TViewState) to high(TViewState) do begin
    EventOfState[st] := nil;
  end;
end;
procedure TEditor.SendData(Data: string);
{Request to send data to the current command (which should be the current state).}
begin
  CallEventState(State, vmeCmdStart, mbExtra1, [], 0, 0, Data);
end;
procedure TEditor.CallEventState(argState: TViewState;
  EventType: TViewEvent; Button: TMouseButton;
  Shift: TShiftState; xp, yp: Integer; txt: string);
{Call the appropriate event for the indicated state}
var
  eveHandler: TViewEventHandler;
begin
  eveHandler := EventOfState[argState];
  if eveHandler=nil then exit;
  eveHandler(EventType, Button, Shift, xp, yp, txt);
end;
// State event handlers
function GetNumber(var txt: string): Single;
{Extracts a number from a text string. If there is an error, devuelev "MaxInt"}
var
  isDecimal: Boolean;
  i: Integer;
  numTxt: String;
begin
  if txt = '' then exit(MaxInt);
  if not (txt[1] in ['0'..'9']) then exit(MaxInt);
  i := 2;
  isDecimal := false;
  while (i<=length(txt)) and (txt[i] in ['0'..'9','.']) do begin
    if txt[i]='.' then begin
      if isDecimal then break;
      isDecimal := true;
    end;
    Inc(i)
  end;
  //Finished exploring the chain
  numTxt := copy(txt, 1, i-1);
  Result := StrToDouble(numTxt);//should not fail if the number has been correctly extracted
  delete(txt, 1, i-1);
end;
function GetSeparator(var txt: string): boolean;
{Extract a separator (space or comma) from a text string, ignoring the spaces
multiple If you can not find a separator, return FALSE}
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
procedure TEditor.proc_NORMAL(EventType: TViewEvent; Button: TMouseButton;
  Shift: TShiftState; xp, yp: Integer; txt: string);
{Process events, in the NORMAL state. This is the stable state or Doc defect.
From here they are passed to all other states.}
var
  o: TGraphicObj;
  s: TGraphicObj;
  o_sel: TGraphicObj;  //ObjSelected
begin
  if EventType = vmeMouseDown then begin
     o_sel := SelectObjectAt(xp, yp);
     if          Shift = [ssRight] then begin     //Right button
         if o_sel = nil Then begin
             SelectNone;
             Refresh;
             State := VS_SELECTINGMULT;
             startRectangleSeleccion(x_mouse, y_mouse);
         end else begin //Select one, there may be others selected
             if o_sel.Selected Then  begin
                 o_sel.MouseDown(Self, Button, Shift, xp, yp);//Pass the event
                 exit;
             end;
             //You select one that had no selection
             if Shift = [ssRight] Then  //Without Control or Shift
               SelectNone;
             o_sel.MouseDown(Self, Button, Shift, xp, yp);  //Pass the event
             Refresh;
         end;
     end else If Shift = [ssLeft] then begin   // Left button
         if o_sel = NIL Then  begin
             SelectNone;
             Refresh;
             State := VS_SELECTINGMULT;
             startRectangleSeleccion(x_mouse, y_mouse);
         end Else begin     //select one, there may be others selected
             If o_sel.Selected Then begin
                 o_sel.MouseDown(Self, Button, Shift, xp, yp);  //Pass the event
                 Moving := True;
                 Exit;
             end;
             //You select one that had no selection
             if Shift = [ssLeft] Then  //Without Control or Shift
                SelectNone;
             o_sel.MouseDown(Self, Button, Shift, xp, yp);  //Pass the event
             Moving := True;
         end;
     end else if Shift >= [ssCtrl, ssShift] then begin
         State := VS_ZOOMING;
         Exit;
     end else if (Shift = [ssMiddle]) or (Shift = [ssCtrl, ssShift, ssRight]) then begin
         State := VS_SCREEN_SCROLLING;
     end else if Shift = [ssMiddle, ssShift] then begin  //Center button and Shift
         State := VS_SCREEN_ANG;
     end;
  end else if EventType = vmeMouseMove then begin
    If MovingObject <> NIL Then begin
       MovingObject.MoveR(Xp, Yp, selection.Count);
       Refresh;
    end Else begin  //Simple movement
        s := VerifyMouseMovement(Xp, Yp);
        if s <> NIL then s.MouseMove(self, Shift, Xp, Yp);  //Pass the event  
    end;
  end else if EventType = vmeMouseUp then begin //Released button
      o := SelectObjectAt(xp, yp);
      if Button = mbRight then begin
      end else If Button = mbLeft Then begin
          If o = NIL Then
          else begin
              If Shift = [] Then SelectNone;
              o.Select;
              o.MouseUp(self, Button, Shift, xp, yp, false);
              Refresh;
          End;
          MovingObject := NIL;      //start event capture flag
          Moving := False;
      end;
  end else if EventType = vmeCmdStart then begin // Execute command
      if txt = 'LINE' then begin
        State := VS_CMD_ADDING_LINE;
        CallEventState(State, vmeCmdStart, mbExtra1, [], 0, 0, '');
      end else if txt = 'RECTANGLE' then begin
        State := VS_CMD_ADDING_RECTAN;
        CallEventState(State, vmeCmdStart, mbExtra1, [], 0, 0, '');
      end else if UpCase(txt) = 'CANCEL' then begin
        //Cancels all active commands
        RestoreState;
      end else begin
        if OnSendMessage<>nil then OnSendMessage(msg.get('CommandUnknown') + txt);
      end;
  end;
end;
procedure TEditor.proc_SELECTINGMULT(EventType: TViewEvent;
  Button: TMouseButton; Shift: TShiftState; xp, yp: Integer; txt: string);
var
  o: TGraphicObj;
  s: TGraphicObj;
begin
  if EventType = vmeMouseDown then begin
  end else if EventType = vmeMouseMove then begin
    x2Sel := xp;
    y2Sel := xp;
    //check those that are selected
    if objects.Count < 100 Then begin//iterate for few objects only
        for s In objects do begin
          if s.SelLocked then continue;
          if inRectangleSeleccion(s.XCent, s.YCent) And Not s.Selected Then begin
            s.Select;
          End;
          if Not inRectangleSeleccion(s.XCent, s.YCent) And s.Selected Then begin
            s.Deselect;
          end;
        end;
    End;
    Refresh
  end else if EventType = vmeMouseUp then begin
    if objects.Count > 100 Then begin  //You need to update because the multiple selection is different
      for o in objects do
        if inRectangleSeleccion(o.XCent, o.YCent) And Not o.Selected Then o.Select;
    end;
    State := VS_NORMAL;
  end;
end;
procedure TEditor.proc_OBJS_MOVING(EventType: TViewEvent;
  Button: TMouseButton; Shift: TShiftState; xp, yp: Integer; txt: string);
var
  s: TGraphicObj;
  o: TGraphicObj;
begin
  if EventType = vmeMouseDown then begin
  end else if EventType = vmeMouseMove then begin
    if OnModify<>nil then OnModify;
    for s in selection do
        s.MoveR(xp,yp, selection.Count);
    Refresh;
  end else if EventType = vmeMouseUp then begin
    for o In selection do  //Pass the event to the selection
        o.MouseUp(self, Button, Shift, xp, yp, State = VS_OBJS_MOVING);
    State := VS_NORMAL;  //end of movement
    Refresh;
    //Generate events The moved objects can be determined from the selection.
    if OnObjectsMoved<>nil then OnObjectsMoved;
  end;
end;
procedure TEditor.proc_SCREEN_SCROLLING(EventType: TViewEvent;
  Button: TMouseButton; Shift: TShiftState; xp, yp: Integer; txt: string);
var
  dx, dy: Single;
begin
  if EventType = vmeMouseDown then begin
  end else if EventType = vmeMouseMove then begin
    VirtScreen.ObtenerDesplazXY( xp, yp, x_mouse, y_mouse, dx, dy);
    VirtScreen.x_cam -= dx;
    VirtScreen.y_cam -= dy;
    x_mouse := xp; y_mouse := yp;  {Maybe you should use other variables than x_mouse, and
                                   y_mouse, so as not to interfere}
    Refresh;
  end else if EventType = vmeMouseUp then begin
    //If it was moving, it returns to the normal state
    State := VS_NORMAL;
  end;
end;
procedure TEditor.proc_SCREEN_ANG(EventType: TViewEvent;
  Button: TMouseButton; Shift: TShiftState; xp, yp: Integer; txt: string);
var
  dx, dy: Single;
begin
  if EventType = vmeMouseDown then begin
  end else if EventType = vmeMouseMove then begin
    VirtScreen.ObtenerDesplazXY( xp, yp, x_mouse, y_mouse, dx, dy);
    VirtScreen.Alfa := VirtScreen.Alfa + dx/100;
    VirtScreen.Fi   := VirtScreen.Fi + dy/100;
    x_mouse := xp; y_mouse := yp;  {Maybe you should use other variables than x_mouse, and
                                    y_mouse, so as not to interfere}
    Refresh;
  end else if EventType = vmeMouseUp then begin
    State := VS_NORMAL;
  end;
end;
procedure TEditor.proc_DIMEN_OBJ(EventType: TViewEvent;
  Button: TMouseButton; Shift: TShiftState; xp, yp: Integer; txt: string);
begin
  if EventType = vmeMouseDown then begin
  end else if EventType = vmeMouseMove then begin
      //an object is being sizing
      MovingObject.MoveR(Xp, Yp, selection.Count);
      Refresh;
  end else if EventType = vmeMouseUp then begin
    //pass event to object that was being dimensioned
    MovingObject.MouseUp(self, Button, Shift, xp, yp, false);
    //ends state
    State := VS_NORMAL;
    MovingObject := NIL;      //start event capture flag
    Moving := False;
  end;
end;
procedure TEditor.proc_ZOOMING(EventType: TViewEvent;
  Button: TMouseButton; Shift: TShiftState; xp, yp: Integer; txt: string);
begin
  if EventType = vmeMouseDown then begin
  end else if EventType = vmeMouseMove then begin
  end else if EventType = vmeMouseUp then begin
    If Button = mbLeft Then ZoomToClick(1.2, xp, yp) ;  //<Shift> + <Ctrl> + left click
    If Button = mbRight Then ZoomReduceClick(1.2, xp, yp) ;  //<Shift> + <Ctrl> + right click
    State := VS_NORMAL;
  end;
end;
procedure TEditor.proc_CMD_ADDING_LINE(EventType: TViewEvent;
  Button: TMouseButton; Shift: TShiftState; xp, yp: Integer; txt: string);
const
  {We use constant with Type because there is no STATIC in FreePascal, and like this
   procedure will be implemented repeatedly, we need to preserve the value of the
   variables, between session and session.}
  step: integer  = 0;
  line: TDxf = nil;
  x0: Single = 0;
  y0: Single = 0;
var
  xLine, yLine: Single;
begin
  if step = 0 then begin
    OnSendMessage(msg.get('enterStartingPoint'));
    step := 1;
  end else if step = 1 then begin
    case EventType of
    vmeCmdStart: begin
      //We expect initial coordinates
      if txt = 'CANCEL' then begin
        RestoreState(msg.get('commandPrompt'));
        step := 0;   //restart
        exit;
      end;
      if not GetCoords(txt, xLine, yLine) then begin
        OnSendMessage(msg.get('errEnterStartPoint'));
        exit;
      end;
      //Add straight, with coord. given
      line := TDxf.Create(VirtScreen);
      line.SetP0(xLine, yLine, 0); //Specify the first point
      line.SetP1(xvPt, yvPt, 0); //Specify next default point
      x0 := xLine; y0 := yLine;  //saves first point
    end;
    vmeMouseDown: begin
      //Add straight, with coord. given
      line := TDxf.Create(VirtScreen);
      line.SetP0(xvPt, yvPt, 0);
      line.SetP1(xvPt, yvPt, 0);
      x0 := xvPt; y0 := yvPt;
    end;
    else exit;  //It goes out for the other events, but it can generate error
    end;
    GraphicObjectAdd(line);
    Refresh;
    OnSendMessage(msg.get('enterFollowingPoint'));
    step := 2;
  end else if step = 2 then begin
    case EventType of
    vmeCmdStart: begin
      if txt = 'CANCEL' then begin
        //The last straight line must be eliminated
        GraphicObjectDelete(line);   //It would be better, if done with an UNDO
        Refresh;
        RestoreState(msg.get('commandPrompt'));  // Ends
        step := 0;   //restart
        exit;
      end;
      if txt = 'C' then begin
        // Close lines
        line.SetP1(x0, y0, 0);
        Refresh;
        step := 0;   //restart
        exit;
      end;
      if not GetCoords(txt, xLine, yLine) then begin
        OnSendMessage(msg.get('enterNextPoint'));
        exit;
      end;
      line.SetP1(xLine, yLine, 0);
      Refresh;

      //Start another line, without leaving the state
      line := TDxf.Create(VirtScreen);
      line.SetP0(xLine, yLine, 0);
      line.SetP1(xvPt, yvPt, 0);
      GraphicObjectAdd(line);
      Refresh;
      OnSendMessage(msg.get('enterFollowingPoint'));
    end;
    vmeMouseMove: begin
      line.SetP1(xvPt, yvPt, 0);
      Refresh;
    end;
    vmeMouseDown: begin
      line.SetP1(xvPt, yvPt, 0);
      Refresh;
      //Start another line, without leaving the state
      line := TDxf.Create(VirtScreen);
      line.SetP0(xvPt, yvPt, 0);
      line.SetP1(xvPt, yvPt, 0);
      GraphicObjectAdd(line);
      Refresh;
      OnSendMessage(msg.get('enterNextPoint'));
    end;
    end;
  end;
end;
procedure TEditor.proc_COMM_RECTAN(EventType: TViewEvent;
  Button: TMouseButton; Shift: TShiftState; xp, yp: Integer; txt: string);
const
  {We use constant with Type because there is no STATIC in FreePascal, and like this
   procedure will be implemented repeatedly, we need to preserve the value of the
   variables, between session and session.}
  step: integer  = 0;
  line: TDxf = nil;
var
  xline, yline: Single;
begin
  case EventType of
  vmeCmdStart: begin
      if step = 0 then begin
        OnSendMessage(msg.get('enterStartingPoint'));
        step := 1;
      end else if step = 1 then begin
        if txt = 'CANCEL' then begin
          RestoreState(msg.get('commandPrompt'));
          step := 0;
          exit;
        end;
        if not GetCoords(txt, xline, yline) then begin
          OnSendMessage('>> ERROR: Ingrese punto inicial:');
          exit;
        end;
        //Add straight, with coord. given
        line := TDxf.Create(VirtScreen);
        line.SetP0(xline, yline, 0); //Specify the first point
        line.SetP1(xvPt, yvPt, 0);
        GraphicObjectAdd(line);
        Refresh;
        OnSendMessage(msg.get('enterNextPoint'));
        step := 2;
      end else if step = 2 then begin
        if txt = 'CANCEL' then begin
          //The last straight line must be eliminated
          GraphicObjectDelete(line);
          Refresh;
          RestoreState(msg.get('commandPrompt'));
          step := 0;
          exit;
        end;
        if not GetCoords(txt, xline, yline) then begin
          OnSendMessage(msg.get('errEnterStartPoint'));
          exit;
        end;
        line.SetP1(xline, yline, 0);
        Refresh;

        RestoreState(msg.get('commandPrompt'));
        step := 0;
        line := nil;
      end;
    end;
  vmeMouseMove: begin
    if step = 2 then begin
        {
     In this phase, the animation should be done if the Mouse is used for PlaceAt
         the next point.
        }
        line.SetP1(xvPt, yvPt, 0);
        Refresh;
    end;
  end;
  vmeMouseDown: begin
      if step = 1 then begin
        //Add straight, with coord. given
        line := TDxf.Create(VirtScreen);
        line.SetP0(xvPt, yvPt, 0);
        line.SetP1(xvPt, yvPt, 0);
        GraphicObjectAdd(line);
        Refresh;
        OnSendMessage(msg.get('enterNextPoint'));
        step := 2;
      end else if step = 2 then begin
        line.SetP1(xvPt, yvPt, 0);
        Refresh;

        //Start another line, without leaving the state
        line := TDxf.Create(VirtScreen);
        line.SetP0(xvPt, yvPt, 0);
        line.SetP1(xvPt, yvPt, 0);
        GraphicObjectAdd(line);
        Refresh;
        OnSendMessage(msg.get('enterNextPoint'));
      end;
    end;
  end;
end;
//Initialization
procedure TEditor.RestoreState(msg: string='');
{Resaturate the state of the Viewer, putting it in the VS_NORMAL state.
If "msg" is indicated, the OnSendMessage () event is generated.}
begin
  State := VS_NORMAL;
  Moving := false;
  MovingObject := nil;
  MarkedObject := nil;
  PBox.Cursor := crDefault;
  if msg<>'' then OnSendMessage(msg);
end;
constructor TEditor.Create(PB0: TPaintBox; objectList: TEditorObjList);
{Initialization method of the Viewer class. The PaintBox should be indicated
output where the graphic objects will be controlled.
and you should also receive the list of objects to be managed.}
var
  argGraphicObject: TMyObject;
begin
  PBox := PB0;
  objects := objectList;
  //Intercept events
  PBox.OnMouseUp   := @PBox_MouseUp;
  PBox.OnMouseDown := @PBox_MouseDown;
  PBox.OnMouseMove := @PBox_MouseMove;
  PBox.OnMouseWheel:= @PBox_MouseWheel;
  PBox.OnDblClick  := @PBox_DblClick;
  PBox.OnPaint     := @PBox_Paint;
  PBox.OnResize    := @PBox_Resize;
  VirtScreen := TVirtScreen.Create(PBox);
  VirtScreen.SetFont('MS Sans Serif');
  VirtScreen.OnChangeView:=@VirtualScreen_ChangeView;
  selection := TEditorObjList.Create(FALSE);  {create list without possession ", because the
                                              administration will do "objects".}
  RestoreState;
  Wheel_step  := 0.1;
  ClearEventState;
  //Create list of events. Must be created for all TViewState values
  RegisterState(VS_NORMAL   , @proc_NORMAL);
  RegisterState(VS_SELECTINGMULT, @proc_SELECTINGMULT);
  RegisterState(VS_OBJS_MOVING , @proc_OBJS_MOVING);
  RegisterState(VS_SCREEN_SCROLLING, @proc_SCREEN_SCROLLING);
  RegisterState(VS_SCREEN_ANG , @proc_SCREEN_ANG);
  RegisterState(VS_DIMEN_OBJ, @proc_DIMEN_OBJ);
  RegisterState(VS_ZOOMING , @proc_ZOOMING);
  //Commands
  RegisterState(VS_CMD_ADDING_LINE, @proc_CMD_ADDING_LINE);
  RegisterState(VS_CMD_ADDING_RECTAN, @proc_COMM_RECTAN);
argGraphicObject := TMyObject.Create(VirtScreen);
GraphicObjectAdd(argGraphicObject);
end;
destructor TEditor.Destroy;
begin
  selection.Free;
  VirtScreen.Free;
  //reset events
  PBox.OnMouseUp:=nil;
  PBox.OnMouseDown:=nil;
  PBox.OnMouseMove:=nil;
  PBox.OnMouseWheel:=nil;
  PBox.OnDblClick:=nil;
  PBox.OnPaint:=nil;
  PBox.OnResize:=nil;
  inherited;
end;

end.

