unit uDocCore;
{$mode objfpc}{$H+}
interface
uses
  Classes, Controls, SysUtils, Fgl, Graphics, GraphType, Types, ExtCtrls,
  //local
  glob,uDraw;

const
  MINWIDTH = 20;//Minimum width of graphic DocCoreObjects in pixels (Virtual Coord)
  MINHEIGHT = 20;//High minimum of graphic DocCoreObjects in Twips (Virtual Coord)

type
  { TDocCoreCommon }
  TDocCoreCommon = class
  protected
    Xvirt,Yvirt  : Single;//virtual coordinates
    Canvas       : TDrawCanvas;
    Xprev,Yprev : Integer;   //previous coordinates
  public
    Width     : Single;
    Height    : Single;
    Selected  : Boolean;
    Visible   : boolean;
    procedure PlaceAt(const argXv, argYv: Single);
    function isSelected(argXp, argYp: Integer): Boolean;
    function CoordPrevSet(argXr, argYr: Integer): Boolean;
    property x: Single read Xvirt;
    property y: Single read Yvirt;
  end;

  TDocCoreControlPointOffset = (//Type of control point offset
    CPO_NO_POS,//without position. It will not be relocated automatically
    CPO_TOP_LEFT,  //top left, moves wide (for left) and high (for top)
    CPO_TOP_CENTER,  //upper center, moves high on top
    CPO_TOP_RIGHT,  //top right, moves wide (for right) and high (for top)

    CPO_CENTER_LEFT,  //left center, moves wide (left)
    CPO_CENTER_RIGHT,  //center right, moves wide (by right)

    CPO_BOTTOM_LEFT,  //lower left
    CPO_BOTTOM_CENTER,  //lower central
    CPO_BOTTOM_RIGHT   //lower right
   );

  {
Event that generates a control point when it is being moved by the Moues.
   (x_vPoint, y_vPoint) is the objective point where the control point is expected to be located, and x_dv / y_dv, are the expected displacements according to the displacement of the mouse. The final displacement can be obtained only with dx and dy, but it is sent also the objective point, for when you want to limit the displacement of a check Point.}
  TDocCoreEventCtrlPointMoveXY = procedure(x_vPoint, y_vPoint, x_dv, y_dv: Single) of object;

  { TDocCoreControlPoint }
  TDocCoreControlPoint = class(TDocCoreCommon)
  private
    Offset: TDocCoreControlPointOffset;
    procedure SetControlPointOffs(AValue: TDocCoreControlPointOffset);
  public
    //The type of displacement, in general, must depend solely on the position
    property displacementType: TDocCoreControlPointOffset read Offset write SetControlPointOffs;
    constructor Create(argCanvas: TDrawCanvas; argDdisplaceType: TDocCoreControlPointOffset;
      ProcMove: TDocCoreEventCtrlPointMoveXY);
    procedure Draw();
    procedure StartMove(xr, yr: Integer; x0, y0: Single);
    procedure MoveR(xr, yr: Integer);  //Dimension the indicated variables
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; xp, yp: Integer);
    function isSelected(xp, yp: Integer):boolean;
  private
    typeOfPoint: Integer;  //Type of point
    OnPointCtrlMoveXY : TDocCoreEventCtrlPointMoveXY;//Control Point shift event.
    x_vPoint, y_vPoint: Single;  {Target coordinates for the dimensions. Used to generate the OnPCdimen event}
  end;
  TDocCoreControlPoints = specialize TFPGObjectList<TDocCoreControlPoint>; //Ready to manage control points

  {Tbot Object - Allows you to manage the buttons}

//Procedure-event for event Click on Button
  TDocCoreEvenBTclk = procedure(state: Boolean) of object;

  TDocCoreBtnType =
   (BTN_CLOSE,   //close button
    BTN_EXPAND,   // expand/collapse
    BTN_CHECK);   //play / stop


  { TDocCoreButton }
  TDocCoreButton = class(TDocCoreCommon)
    state     : Boolean;//Allows Show the state of the button or check
    drawBackground   : boolean;//indicates whether Draw the background
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; xp, yp: Integer);
  private
    btnType : TDocCoreBtnType;
    OnClick: TDocCoreEvenBTclk
  end;

  TDocCoreButtons = specialize TFPGObjectList<TDocCoreButton>;//To manage the buttons

  TDocCoreObject = class;
  TDocCoreEventSelect = procedure(obj: TDocCoreObject) of object;//Procedure-event to select
  TDocCoreEventChangePoint = procedure(PointType: Integer) of object; //Procedure-event to change point

  { TDocCoreObject }
  {This is the parent Object of all visible graphic DocCoreObjects that are managed by the editing engine}
  TDocCoreObject = class(TDocCoreCommon)
  private
  protected
    ControlPoint        : TDocCoreControlPoint;      //variable for Control Point
    ControlPoints: TDocCoreControlPoints; 
    Buttons    : TDocCoreButtons;
    procedure ReconstructGeom; virtual;//Rebuild the geometry of the object
    function getSelectedControlPoint(xp, yp: integer): TDocCoreControlPoint;
  public
    Name      : String;
    Marked     : Boolean;//Indicates that it is marked, because the mouse passes over
    SimpleDrawingMode  : Boolean;//indicates that you are in simplified drawing mode
    canHighlight   : Boolean;//indicates whether it allows the highlighting of the object
    SizeLocked  : boolean;//protects the resized object
    PosLocked   : Boolean;
    SelLocked   : Boolean;
    ColorFill     : TColor;//Fill color
    Processing     : Boolean;   //flag
    Resizing    : boolean;  //indicates that the object is being sized
    Erased      : boolean;  
    //Class events
    OnSelect  : TDocCoreEventSelect;
    OnDeselect: TDocCoreEventSelect;
    OnChangePoint: TDocCoreEventChangePoint;
    function XCent: Single;
    function YCent: Single; 
    procedure PlaceAt(x0, y0: Single);
    procedure Select;
    procedure Deselect;  
    procedure Delete;
    procedure MoveR(xr, yr : Integer; argObjsQuantity : Integer); virtual;
    function isSelected(xr, yr:integer): Boolean; virtual;
    procedure Draw; virtual; 
    procedure StartMove(xr, yr : Integer);
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; xp, yp: Integer); virtual;
    procedure MouseUp(Sender: TObject; mbtn: TMouseButton; Shift: TShiftState; xp, yp: Integer; solto_objeto: Boolean); virtual;
    procedure MouseMove(Sender: TObject; Shift: TShiftState; xp, yp: Integer); virtual;
    procedure MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean); virtual;
    function AddControlPoint(argDdisplaceType: TDocCoreControlPointOffset; ProcDimen: TDocCoreEventCtrlPointMoveXY): TDocCoreControlPoint;

    constructor Create(argCanvas: TDrawCanvas); virtual;
    destructor Destroy; override;
  end;

  TDocCoreObjects = specialize TFPGObjectList<TDocCoreObject>;

implementation
const
  WIDTH_CONTROLPOINT2 = 5;//half the width of control point width_

{ TDocCoreCommon }

procedure TDocCoreCommon.PlaceAt(const argXv, argYv: Single);
{Locate in virtual coordinates}
begin
  Xvirt := argXv;
  Yvirt := argYv;
end;

function TDocCoreCommon.isSelected(argXp, argYp: Integer): Boolean;
//Indicates whether the mouse coordinates select the object in its current position
var xv, yv: Single;    //virtual coordinates
begin
    Canvas.XYvirt(argXp, argYp, xv, yv);
    isSelected := False;    //Default value
    If (xv > Xvirt - 2) And (xv < Xvirt + width + 2) And
       (yv > Yvirt - 2) And (yv < Yvirt + height + 2) Then
        isSelected := True;
end;
function TDocCoreCommon.CoordPrevSet(argXr, argYr: Integer): Boolean;
begin
  Result := false; 
  if not visible then exit;  
  // current position capture, to calculate the displacements
  Xprev := argXr;
  Yprev := argYr;
end;




procedure TDocCoreButton.MouseUp(Button: TMouseButton; Shift: TShiftState; xp, yp: Integer);
begin
   if isSelected(xp,yp) then begin    //he let go on the button
      //change the state, if applicable
      if btnType in [BTN_EXPAND, BTN_CHECK] then state := not state;
      if Assigned(OnClick) then
         OnClick(state);    //run event
   end;
end;

{ TDocCoreObject }
function TDocCoreObject.getSelectedControlPoint(xp, yp:integer): TDocCoreControlPoint;
//Indicates if you select any control point and returns the reference.
var point: TDocCoreControlPoint;
begin
  getSelectedControlPoint := NIL;//Default value
  for point in ControlPoints do
      if point.isSelected(xp,yp) then begin getSelectedControlPoint := point; Exit; end;
end;
function TDocCoreObject.XCent: Single;
begin
   Result := Xvirt + width / 2;
end;
function TDocCoreObject.YCent: Single;
begin
   Result := Yvirt + height / 2;
end;
procedure TDocCoreObject.Select;
begin
   if Selected then exit; 
   Selected := true; 
   //Call the event that selects the object. The editor must respond
   if Assigned(OnSelect) then OnSelect(self);//call the event
end;
procedure TDocCoreObject.Deselect;
begin
   if not Selected then exit; 
   Selected := false; 
   //Call the event that selects the object. The editor must respond
   if Assigned(OnDeselect) then OnDeselect(self);  //llama al evento
end;
procedure TDocCoreObject.Delete;
begin
  Erased := true;
end;
procedure TDocCoreObject.MoveR(xr, yr: Integer; argObjsQuantity: Integer);
{Method that works as MouseMove event of the object. The normal thing that
produces a displacement of the object.
"nobjects" is the number of DocCoreObjects that move. Usually it's just one}
var dx , dy: Single;
begin
     If Selected Then begin
        Canvas.GetOffsetXY( xr, yr, Xprev, Yprev, dx, dy);
        if Processing then//some element of the object has processed the movement event
           begin
              if ControlPoint <> NIL then begin
                 //there is a control point processing the MouseMove event
                 if not SizeLocked then
                   ControlPoint.MoveR(xr, yr);//allows to size the object
              end;
//              Processing := True;  'ya alguien ha capturado el evento
           end
        else  //ningún elemento del objeto lo ha procesado, pasamos a mover todo el objeto
           begin
              Xvirt := Xvirt + dx; Yvirt := Yvirt + dy;
              Processing := False;
           End;
        Xprev := xr; Yprev := yr;
     End;
end;

function TDocCoreObject.isSelected(xr, yr:integer): Boolean;
{ Returns true if the screen coordinate xr, yr falls at such a point
that "would" the selection of the form. }
var xv , yv : Single; //virtual corodinates
begin
    Canvas.XYvirt(xr, yr, xv, yv);
    isSelected := False; //Default value
    //check selection area
    If (xv > Xvirt - 1) And (xv < Xvirt + width + 1) And (yv > Yvirt - 1) And (yv < Yvirt + height + 1) Then
      isSelected := True;
    if Selected then begin//Selected, has a larger selection area
      if getSelectedControlPoint(xr,yr) <> NIL then isSelected := True;
    end;
End;
procedure TDocCoreObject.Draw;
const tm = 3;
var
  cp  : TDocCoreControlPoint;
begin
  //---------------draw highlighted --------------
  If Marked and canHighlight Then begin
    Canvas.SetPen(clBlue, 2, psSolid);   //RGB(128, 128, 255)
    Canvas.rectangXY(Xvirt - tm, Yvirt - tm, Xvirt + width + tm, Yvirt + height + tm);
  End;
  //---------------draw selection marker--------------
  If Selected Then begin
     for cp in ControlPoints do cp.Draw;   //Draw control points
  End;
end;
procedure TDocCoreObject.StartMove(xr, yr: Integer);
{ Procedure to process the StartMove event of the graphic DocCoreObjects
It is executed at the beginning of movement to the object }
begin
  Xprev := xr; Yprev := yr;
  Processing := False;
  if not Selected then exit;//to prevent it from responding before being selected
  //Search if any control point processes it
  ControlPoint := getSelectedControlPoint(xr,yr);
  if ControlPoint <> NIL  then begin
      ControlPoint.StartMove(xr, yr, Xvirt, Yvirt);//ready for movement Yvirt sizing
      Processing := True;{ Mark to tell the Yvirt editor to MoveR () that this object will process the Yvirt event, I did not pass it on to the others that may be selected. }
      Resizing := True; 
   end;
end;
procedure TDocCoreObject.MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; xp, yp: Integer);
begin
  Processing := False;
  If isSelected(xp, yp) Then begin  
    If Not Selected Then Select;
    Processing := True;
  End;
End;
procedure TDocCoreObject.MouseUp(Sender: TObject; mbtn: TMouseButton;
  Shift: TShiftState; xp, yp: Integer; solto_objeto: Boolean);
var
  btn: TDocCoreButton;
begin
    Processing := False;
    //check if it falls from a drag
    If solto_objeto And Selected Then begin
        Processing := True; Exit;    //does not remove the selection
    end;
    //The mouse was released
    If mbtn = mbLeft Then  begin          //let go left
       //pass event to controls
       for btn in Buttons do btn.MouseUp(mbtn, Shift, xp, yp);
    end else If mbtn = mbRight Then begin //let go right
        If isSelected(xp, yp) Then
            Processing := True;
    end;
    //Restore point if it was being sized just in case
    if Resizing then begin
       if not ControlPoint.isSelected(xp,yp) then //it went out of focus
          if Assigned(OnChangePoint) then OnChangePoint(crDefault);//asks to retake the point
       Resizing := False;//remove flag, in case it was Resizing
       exit;
    end;
end;
procedure TDocCoreObject.MouseMove(Sender: TObject; Shift: TShiftState; xp, yp: Integer);
//Response to the MouseMove event. Must be received when the Mouse passes over the object
var pc: TDocCoreControlPoint;
begin
    if not Selected then Exit;
   { Here we're supposed to take control because it's Selected
     Process the pointer change. }
    if Assigned(OnChangePoint) then begin
        pc := getSelectedControlPoint(xp,yp);
        if pc<> NIL then
           OnChangePoint(pc.typeOfPoint)//change to supuntero
        else
           OnChangePoint(crDefault);
    end;
end;
procedure TDocCoreObject.MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin

end;
constructor TDocCoreObject.Create(argCanvas: TDrawCanvas);
begin
  inherited Create;
  erased := false;
  Canvas := argCanvas;
  width := 100;   //width by default
  height := 100;    //height by default
  Xvirt := 100;
  Yvirt := 100;
  ControlPoints:= TDocCoreControlPoints.Create(True);
  Buttons    := TDocCoreButtons.Create(True);
  Selected := False;
  Marked := False;
  Processing := false;
  SimpleDrawingMode := false;
  canHighlight := true;
end;



procedure TDocCoreObject.ReconstructGeom;
begin

end;
destructor TDocCoreObject.Destroy;
begin
  Buttons.Free;
  ControlPoints.Free;
  inherited Destroy;
end;
procedure TDocCoreObject.PlaceAt(x0, y0: Single);
//Locate the object in specific coordinates
begin
  Xvirt := x0;
  Yvirt := y0;
end;

function TDocCoreObject.AddControlPoint(argDdisplaceType: TDocCoreControlPointOffset; ProcDimen: TDocCoreEventCtrlPointMoveXY): TDocCoreControlPoint;
//Add a control point
begin
  Result := TDocCoreControlPoint.Create(Canvas, argDdisplaceType, ProcDimen);
  ControlPoints.Add(Result);
end;
 //////////////////////////////  TDocCoreControlPoint  //////////////////////////////
procedure TDocCoreControlPoint.SetControlPointOffs(AValue: TDocCoreControlPointOffset);
begin
  if Offset=AValue then Exit;
  Offset:=AValue;
  //update point btnType
  case displacementType of
  CPO_TOP_LEFT: typeOfPoint := crSizeNW;
  CPO_TOP_CENTER: typeOfPoint := crSizeNS;
  CPO_TOP_RIGHT: typeOfPoint := crSizeNE;

  CPO_CENTER_LEFT: typeOfPoint := crSizeWE;
  CPO_CENTER_RIGHT: typeOfPoint := crSizeWE;

  CPO_BOTTOM_LEFT: typeOfPoint := crSizeNE;
  CPO_BOTTOM_CENTER: typeOfPoint := crSizeNS;
  CPO_BOTTOM_RIGHT: typeOfPoint := crSizeNW;
  else        typeOfPoint := crDefault ;
  end;
end;
constructor TDocCoreControlPoint.Create(argCanvas: TDrawCanvas; argDdisplaceType: TDocCoreControlPointOffset;
  ProcMove: TDocCoreEventCtrlPointMoveXY);
begin
  displacementType := argDdisplaceType; 
  OnPointCtrlMoveXY := ProcMove;//Assign event to change dimensions
  visible := true;
  Xvirt :=0;
  Yvirt :=0;
end;
procedure TDocCoreControlPoint.Draw();
//Draw the Control Point in the defined position
var
  d: Single;
begin
    if not visible then exit; 
    Canvas.SetPen(TColor($FF8000), 1);
    Canvas.FillFixed(TColor($FF8000));
    d := WIDTH_CONTROLPOINT2 / Canvas.Zoom;//correct so that it always comes out the same size
    Canvas.rectangXYr(Xvirt - d, Yvirt - d, Xvirt + d, Yvirt + d);
end;
procedure TDocCoreControlPoint.StartMove(xr, yr: Integer; x0, y0: Single);
//Procedure to process the StartMove event of the control point
begin
    if not visible then exit;
    inherited CoordPrevSet(xr,yr);
    {Start at the current coordinates of the Control Point }
    x_vPoint := Xvirt;
    y_vPoint := Yvirt;
end;
procedure TDocCoreControlPoint.MoveR(xr, yr: Integer);
{Make the change of the indicated variables according to the type of control and to
the indicated variations (dx, dy)}
var dx, dy: Single;
begin
    if not visible then exit;
    Canvas.GetOffsetXY(xr, yr, Xprev, Yprev, dx, dy);
    if OnPointCtrlMoveXY <>nil then OnPointCtrlMoveXY(x_vPoint, y_vPoint, dx, dy);
    x_vPoint := x_vPoint + dx;
    y_vPoint := y_vPoint + dy;
    Xprev := xr; Yprev := yr;//update, for the calculation of GetDesplazXY ()
end;
procedure TDocCoreControlPoint.MouseUp(Button: TMouseButton; Shift: TShiftState; xp,  yp: Integer);
//Process the MouseUp event of the "mouse".
begin
end;
function TDocCoreControlPoint.isSelected(xp, yp: Integer): boolean;
//Indicates whether the coordinates are selected
var xp0, yp0 : Integer; //virtual corodinates
begin
    isSelected := False;
    if not visible then exit; 
    Canvas.XYpant(Xvirt, Yvirt, xp0, yp0);//get your coordinates on screen
    //compare in screen coordinates
    If (xp >= xp0 - WIDTH_CONTROLPOINT2) And (xp <= xp0 + WIDTH_CONTROLPOINT2) And
       (yp >= yp0 - WIDTH_CONTROLPOINT2) And (yp <= yp0 + WIDTH_CONTROLPOINT2) Then
         isSelected := True;
End;

end.

