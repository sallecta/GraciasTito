{
Unit sketchCore
бля, пиздец нахуй
}
unit sketchCore;
{$mode objfpc}{$H+}
interface
uses
  Classes, Controls, SysUtils, Fgl, Graphics, GraphType, Types, ExtCtrls, MotGraf3d;

const
  MINWIDTH = 20;//Minimum width of graphic sketchCoreObjects in pixels (Virtual Coord)
  MINHEIGHT = 20;//High minimum of graphic sketchCoreObjects in Twips (Virtual Coord)

type
  { TObjVsible }
  //Base class for all visible sketchCoreObjects
  TObjVsible = class
  protected
    Xvirt,Yvirt,Zvirt  : Single;//virtual coordinates
    VirtScreen       : TVirtScreen;
    Xprev,Yprev : Integer;   //previous coordinates
  public
    Id        : Integer;   //Identifier of the Object. Not used by the class. It is left for easy identification.
    Width     : Single;
    Height    : Single;
    Selected  : Boolean;
    Visible   : boolean;
    procedure CreateIt(argVirtScreen: TVirtScreen; argWidth, argHeight: Integer); 
    procedure PlaceAt(const xv, yv, zv: Single);
    procedure PlaceAt(const P: TPoint3D);
    function isSelected(xp, yp: Integer): Boolean;
    function StartMove(xr, yr: Integer): Boolean;
    property x: Single read Xvirt;
    property y: Single read Yvirt;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TControlPointOffs = (//Type of control point offset
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
  TEvPointCtrlMoveXY = procedure(x_vPoint, y_vPoint, x_dv, y_dv: Single) of object;

  { TControlPoint }
  TControlPoint = class(TObjVsible)
  private
    ControlPointOffs: TControlPointOffs;
    procedure SetControlPointOffs(AValue: TControlPointOffs);
  public
    //The type of displacement, in general, must depend solely on the position
    property displacementType: TControlPointOffs read ControlPointOffs write SetControlPointOffs;
    constructor CreateIt(argVirtScreen: TVirtScreen; argDdisplaceType: TControlPointOffs;
      ProcMove: TEvPointCtrlMoveXY);
    procedure Draw();
    procedure StartMove(xr, yr: Integer; x0, y0: Single);
    procedure MoveR(xr, yr: Integer);  //Dimension the indicated variables
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; xp, yp: Integer);
    function isSelected(xp, yp: Integer):boolean;
  private
    typeOfPoint: Integer;  //Type of point
    OnPointCtrlMoveXY : TEvPointCtrlMoveXY;//Control Point shift event.
    x_vPoint, y_vPoint: Single;  {Target coordinates for the dimensions. Used to generate the OnPCdimen event}
  end;
  TPtosControls = specialize TFPGObjectList<TControlPoint>; //Ready to manage control points

  {Tbot Object - Allows you to manage the buttons}

//Procedure-event for event Click on Button
  TEvenBTclk = procedure(state: Boolean) of object;

  TBtnType =
   (BTN_CLOSE,   //close button
    BTN_EXPAND,   // expand/collapse
    BTN_CHECK,    //check
    BTN_PLAY);   //play / stop

  TSBOrientation =
   (SB_HORIZONT,    //horizontal
    SB_VERTICAL);   //vertical

  { TogButton }
  TogButton = class(TObjVsible)
    state     : Boolean;//Allows Show the state of the button or check
    drawBackground   : boolean;//indicates whether Draw the background
    constructor Create(argVirtScreen: TVirtScreen; argType: TBtnType; argEvenBTclk: TEvenBTclk);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; xp, yp: Integer);
  private
    btnType : TBtnType;
    OnClick: TEvenBTclk
  end;

  ButtonsList = specialize TFPGObjectList<TogButton>;//To manage the buttons

  sketchCoreObj = class;
  TEventSelect = procedure(obj: sketchCoreObj) of object;//Procedure-event to select
  TEventChPoint = procedure(PointType: Integer) of object; //Procedure-event to change point

  { sketchCoreObj }
  {This is the parent Object of all visible graphic sketchCoreObjects that are managed by the editing engine}
  sketchCoreObj = class(TObjVsible)
  private
  protected
    ControlPoint        : TControlPoint;      //variable for Control Point
    ControlPoints: TPtosControls; 
    Buttons    : ButtonsList;
    procedure RelocateElements; virtual;
    procedure ReconstructGeom; virtual;//Rebuild the geometry of the object
    function getSelectedControlPoint(xp, yp: integer): TControlPoint;
  public
    Name      : String;//Identificación del objeto
    Marked     : Boolean;//Indicates that it is marked, because the mouse passes over
    SimpleDrawingMode  : Boolean;//indicates that you are in simplified drawing mode
    canHighlight   : Boolean;//indicates whether it allows the highlighting of the object
    SizeLocked  : boolean;//protects the resized object
    PosLocked   : Boolean;
    SelLocked   : Boolean; 
    ObjType        : Integer;//ObjType of object. Not used by the library. It remains for the user.
    Data        : string;//Additional information Not used by the library. It remains for the user.
    Obj         : pointer;//Additional information Not used by the library. It remains for the user.
    ColorFill     : TColor;//Fill color
    Processing     : Boolean;   //flag
    Resizing    : boolean;  //indicates that the object is being sized
    Erased      : boolean;  
    //Class events
    OnSelect  : TEventSelect;
    OnDeselect: TEventSelect;
    OnCamPoint: TEventChPoint;
    function XCent: Single;
    function YCent: Single; 
    procedure PlaceAt(x0, y0: Single);
    procedure Select;//Unique method to select the object
    procedure Deselect;  
    procedure Delete;
    procedure MoveR(xr, yr : Integer; nobjetos : Integer); virtual;
    function isSelected(xr, yr:integer): Boolean; virtual;
    procedure Draw; virtual;  //Dibuja el objeto gráfico
    procedure StartMove(xr, yr : Integer);
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; xp, yp: Integer); virtual;//Method that works as a mouse_down event
    procedure MouseUp(Sender: TObject; mbtn: TMouseButton; Shift: TShiftState; xp, yp: Integer; solto_objeto: Boolean); virtual;
    procedure MouseMove(Sender: TObject; Shift: TShiftState; xp, yp: Integer); virtual;
    procedure MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean); virtual;
    function AddControlPoint(argDdisplaceType: TControlPointOffs; ProcDimen: TEvPointCtrlMoveXY): TControlPoint;
    function AddButton(argWidth, argHeight: Integer; argType: TBtnType;
      argEvenBTclk: TEvenBTclk): TogButton;
    constructor Create(argVirtScreen: TVirtScreen); virtual;
    destructor Destroy; override;
  end;

  TSketchCoreObjects = specialize TFPGObjectList<sketchCoreObj>;

implementation
const
  WIDTH_CONTROLPOINT2 = 5;//half the width of control point width_

{ TObjVsible }
procedure TObjVsible.CreateIt(argVirtScreen: TVirtScreen; argWidth, argHeight: Integer);
begin
  VirtScreen := argVirtScreen;
  width:=argWidth;
  height :=argHeight;
  visible := true;
end;
procedure TObjVsible.PlaceAt(const xv, yv, zv: Single);
{Locate in virtual coordinates}
begin
  Xvirt := xv;
  Yvirt := yv;
  Zvirt := zv;
end;
procedure TObjVsible.PlaceAt(const P: TPoint3D);
{Locate in virtual coordinates}
begin
  Xvirt := P.x;
  Yvirt := P.y;
  Zvirt := P.z;
end;

function TObjVsible.isSelected(xp, yp: Integer): Boolean;
//Indicates whether the mouse coordinates select the object in its current position
var xv, yv: Single;    //virtual coordinates
begin
    VirtScreen.XYvirt(xp, yp, 0, xv, yv);
    isSelected := False;    //Default value
    If (xv > Xvirt - 2) And (xv < Xvirt + width + 2) And
       (yv > Yvirt - 2) And (yv < Yvirt + height + 2) Then
        isSelected := True;
end;
function TObjVsible.StartMove(xr, yr: Integer): Boolean;
begin
  Result := false; 
  if not visible then exit;  
  // current position capture, to calculate the displacements
  Xprev := xr;
  Yprev := yr;
end;
constructor TObjVsible.Create;
begin
  inherited Create;
end;
destructor TObjVsible.Destroy;
begin
  inherited Destroy;
end;

{ TogButton }
constructor TogButton.Create(argVirtScreen: TVirtScreen; argType: TBtnType;
  argEvenBTclk: TEvenBTclk);
begin
   inherited CreateIt(argVirtScreen, 16, 16);    //crea
   btnType := argType;
   OnClick := argEvenBTclk;
   state := FALSE;//start at 0 (check not Marked, or button to be contracted)
   drawBackground := true;
end;

procedure TogButton.MouseUp(Button: TMouseButton; Shift: TShiftState; xp, yp: Integer);
begin
   if isSelected(xp,yp) then begin    //he let go on the button
      //change the state, if applicable
      if btnType in [BTN_EXPAND, BTN_CHECK, BTN_PLAY] then state := not state;
      if Assigned(OnClick) then
         OnClick(state);    //run event
   end;
end;

{ sketchCoreObj }
function sketchCoreObj.getSelectedControlPoint(xp, yp:integer): TControlPoint;
//Indicates if you select any control point and returns the reference.
var point: TControlPoint;
begin
  getSelectedControlPoint := NIL;//Default value
  for point in ControlPoints do
      if point.isSelected(xp,yp) then begin getSelectedControlPoint := point; Exit; end;
end;
function sketchCoreObj.XCent: Single;
begin
   Result := Xvirt + width / 2;
end;
function sketchCoreObj.YCent: Single;
begin
   Result := Yvirt + height / 2;
end;
procedure sketchCoreObj.Select;
begin
   if Selected then exit; 
   Selected := true; 
   //Call the event that selects the object. The editor must respond
   if Assigned(OnSelect) then OnSelect(self);//call the event
end;
procedure sketchCoreObj.Deselect;
begin
   if not Selected then exit; 
   Selected := false; 
   //Call the event that selects the object. The editor must respond
   if Assigned(OnDeselect) then OnDeselect(self);  //llama al evento
end;
procedure sketchCoreObj.Delete;
begin
  Erased := true;
end;
procedure sketchCoreObj.MoveR(xr, yr: Integer; nobjetos: Integer);
{Method that works as MouseMove event of the object. The normal thing that produces a displacement of the object.
"nobjects" is the number of sketchCoreObjects that move. Usually it's just one}
var dx , dy: Single;
begin
     If Selected Then begin
        VirtScreen.ObtenerDesplazXY( xr, yr, Xprev, Yprev, dx, dy);
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
              RelocateElements;  //relocate the elements
              Processing := False;
           End;
        Xprev := xr; Yprev := yr;
     End;
end;

function sketchCoreObj.isSelected(xr, yr:integer): Boolean;
{ Returns true if the screen coordinate xr, yr falls at such a point
that "would" the selection of the form. }
var xv , yv : Single; //virtual corodinates
begin
    VirtScreen.XYvirt(xr, yr, 0, xv, yv);
    isSelected := False; //Default value
    //check selection area
    If (xv > Xvirt - 1) And (xv < Xvirt + width + 1) And (yv > Yvirt - 1) And (yv < Yvirt + height + 1) Then
      isSelected := True;
    if Selected then begin//Selected, has a larger selection area
      if getSelectedControlPoint(xr,yr) <> NIL then isSelected := True;
    end;
End;
procedure sketchCoreObj.Draw;
const tm = 3;
var
  pdc  : TControlPoint;
  button  : TogButton;
begin
  //---------------draw highlighted --------------
  If Marked and canHighlight Then begin
    VirtScreen.SetPen(clBlue, 2, psSolid);   //RGB(128, 128, 255)
    VirtScreen.rectangXY(Xvirt - tm, Yvirt - tm, Xvirt + width + tm, Yvirt + height + tm,0);
  End;
  //---------------dibuja marca de selection--------------
  If Selected Then begin
//    VirtScreen.FijaLapiz(psSolid, 1, clGreen);
//    VirtScreen.rectang(Xvirt, Yvirt, Xvirt + width, Yvirt + height);
     for pdc in ControlPoints do pdc.Draw;   //Draw control points
  End;
end;
procedure sketchCoreObj.StartMove(xr, yr: Integer);
{ Procedure to process the StartMove event of the graphic sketchCoreObjects
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
procedure sketchCoreObj.MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; xp, yp: Integer);
//Method that works as an "MouseDown" event
begin
//  MovingObject := NIL;
  Processing := False;
  If isSelected(xp, yp) Then begin  //only responds instantly to the selection case
    If Not Selected Then Select;
    Processing := True;
  End;
End;
procedure sketchCoreObj.MouseUp(Sender: TObject; mbtn: TMouseButton;
  Shift: TShiftState; xp, yp: Integer; solto_objeto: Boolean);
{ Method that works as a MouseUp event the "solto_objeto" flag indicates that
the object has been dropped after being dragged }
var
  btn: TogButton;
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
          if Assigned(OnCamPoint) then OnCamPoint(crDefault);//asks to retake the point
       Resizing := False;//remove flag, in case it was Resizing
       exit;
    end;
end;
procedure sketchCoreObj.MouseMove(Sender: TObject; Shift: TShiftState; xp, yp: Integer);
//Response to the MouseMove event. Must be received when the Mouse passes over the object
var pc: TControlPoint;
begin
    if not Selected then Exit;
   { Here we're supposed to take control because it's Selected
     Process the pointer change. }
    if Assigned(OnCamPoint) then begin
        pc := getSelectedControlPoint(xp,yp);
        if pc<> NIL then
           OnCamPoint(pc.typeOfPoint)//change to supuntero
        else
           OnCamPoint(crDefault);
    end;
end;
procedure sketchCoreObj.MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin

end;
constructor sketchCoreObj.Create(argVirtScreen: TVirtScreen);
begin
  inherited Create;
  erased := false;
  VirtScreen := argVirtScreen;
  width := 100;   //width by default
  height := 100;    //height by default
  Xvirt := 100;
  Yvirt := 100;
  ControlPoints:= TPtosControls.Create(True);
  Buttons    := ButtonsList.Create(True); 
  Selected := False;
  Marked := False;
  Processing := false;
  SimpleDrawingMode := false;
  canHighlight := true;
end;

procedure sketchCoreObj.RelocateElements;
begin
end;

procedure sketchCoreObj.ReconstructGeom;
begin
  RelocateElements;
end;
destructor sketchCoreObj.Destroy;
begin
  Buttons.Free; 
  ControlPoints.Free;
  inherited Destroy;
end;
procedure sketchCoreObj.PlaceAt(x0, y0: Single);
//Locate the object in specific coordinates
begin
  Xvirt := x0;
  Yvirt := y0;
  RelocateElements;
end;
function sketchCoreObj.AddButton(argWidth, argHeight: Integer; argType: TBtnType;
  argEvenBTclk: TEvenBTclk): TogButton;
//Add a button to the object.
begin
  Result := TogButton.Create(VirtScreen, argType, argEvenBTclk);
  Result.width := argWidth;
  Result.height := argHeight;
  Buttons.Add(Result);
end;
function sketchCoreObj.AddControlPoint(argDdisplaceType: TControlPointOffs; ProcDimen: TEvPointCtrlMoveXY): TControlPoint;
//Add a control point
begin
  Result := TControlPoint.CreateIt(VirtScreen, argDdisplaceType, ProcDimen);
  ControlPoints.Add(Result);
end;
 //////////////////////////////  TControlPoint  //////////////////////////////
procedure TControlPoint.SetControlPointOffs(AValue: TControlPointOffs);
begin
  if ControlPointOffs=AValue then Exit;
  ControlPointOffs:=AValue;
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
constructor TControlPoint.CreateIt(argVirtScreen: TVirtScreen; argDdisplaceType: TControlPointOffs;
  ProcMove: TEvPointCtrlMoveXY);
begin
  inherited CreateIt(argVirtScreen, 2*WIDTH_CONTROLPOINT2, 2*WIDTH_CONTROLPOINT2);    //crea
  displacementType := argDdisplaceType; 
  OnPointCtrlMoveXY := ProcMove;//Assign event to change dimensions
  visible := true;
  Xvirt :=0;
  Yvirt :=0;
end;
procedure TControlPoint.Draw();
//Draw the Control Point in the defined position
var
  d: Single;
begin
    if not visible then exit; 
    VirtScreen.SetPen(TColor($FF8000), 1);
    VirtScreen.FillFixed(TColor($FF8000));
    d := WIDTH_CONTROLPOINT2 / VirtScreen.Zoom;//correct so that it always comes out the same size
    VirtScreen.rectangXYr(Xvirt - d, Yvirt - d, Xvirt + d, Yvirt + d, Zvirt);
end;
procedure TControlPoint.StartMove(xr, yr: Integer; x0, y0: Single);
//Procedure to process the StartMove event of the control point
begin
    if not visible then exit;
    inherited StartMove(xr,yr);
    {Start at the current coordinates of the Control Point }
    x_vPoint := Xvirt;
    y_vPoint := Yvirt;
end;
procedure TControlPoint.MoveR(xr, yr: Integer);
{Make the change of the indicated variables according to the type of control and to
the indicated variations (dx, dy)}
var dx, dy: Single;
begin
    if not visible then exit;
    VirtScreen.ObtenerDesplazXY(xr, yr, Xprev, Yprev, dx, dy);
    if OnPointCtrlMoveXY <>nil then OnPointCtrlMoveXY(x_vPoint, y_vPoint, dx, dy);
    x_vPoint := x_vPoint + dx;
    y_vPoint := y_vPoint + dy;
    Xprev := xr; Yprev := yr;//update, for the calculation of GetDesplazXY ()
end;
procedure TControlPoint.MouseUp(Button: TMouseButton; Shift: TShiftState; xp,  yp: Integer);
//Process the MouseUp event of the "mouse".
begin
end;
function TControlPoint.isSelected(xp, yp: Integer): boolean;
//Indicates whether the coordinates are selected
var xp0, yp0 : Integer; //virtual corodinates
begin
    isSelected := False;
    if not visible then exit; 
    VirtScreen.XYpant(Xvirt, Yvirt, Zvirt, xp0, yp0);//get your coordinates on screen
    //compare in screen coordinates
    If (xp >= xp0 - WIDTH_CONTROLPOINT2) And (xp <= xp0 + WIDTH_CONTROLPOINT2) And
       (yp >= yp0 - WIDTH_CONTROLPOINT2) And (yp <= yp0 + WIDTH_CONTROLPOINT2) Then
         isSelected := True;
End;

end.

