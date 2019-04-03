unit guiForm1;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, ActnList, Menus,
  StdCtrls, ComCtrls, LCLProc, LCLType, Buttons, MisUtils, guiFormConfig,
  guiFrameCfgGeneral, sketchDocument, guiFramePaintBox, guiFormProject,
  glob, guiFrameProjExplorer, guiFormPerspective, guiFormViewProp,
  sketchEditor;

const
  NUM_CUAD = 20;
  ZOOM_INI = 12;

type

  { TForm1 }
  TForm1 = class(TForm)
  published
    acToolbarDesp: TAction;
    acToolbarPoint: TAction;
    acToolbarRot: TAction;
    acProjFileNew: TAction;
    acProjFileClose: TAction;
    acProjFileSave: TAction;
    acProjFileLeave: TAction;
    acProjInsPolyline: TAction;
    acProjProp: TAction;
    acProjInsRect: TAction;
    acProInsCube: TAction;
    acProjInsRectan: TAction;
    acProjAddPage: TAction;
    acPageProp: TAction;
    acPageCamNom: TAction;
    acPageRemove: TAction;
    acPageAddLine: TAction;
    acViewProp: TAction;
    acVerViewSup: TAction;
    acVerConView: TAction;
    BitBtn1: TBitBtn;
    Edit1: TEdit;
    Label1: TLabel;
    Memo1: TMemo;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    acToolbarConfig: TAction;
    ActionList1: TActionList;
    acProjFileOpen: TAction;
    ImgActions16: TImageList;
    ImgActions32: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    PageControl1: TPageControl;
    Panel1: TPanel;
    panCommand: TPanel;
    PopupView: TPopupMenu;
    PopupPage: TPopupMenu;
    PopupProject: TPopupMenu;
    PopupObjects: TPopupMenu;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure acProjFileCloseExecute(Sender: TObject);
    procedure acProjFileSaveExecute(Sender: TObject);
    procedure acProjFileNewExecute(Sender: TObject);
    procedure acProjFileLeaveExecute(Sender: TObject);
    procedure acToolbarDespExecute(Sender: TObject);
    procedure acToolbarPointExecute(Sender: TObject);
    procedure acToolbarRotExecute(Sender: TObject);
    procedure acPageAddLineExecute(Sender: TObject);
    procedure acPageRemoveExecute(Sender: TObject);
    procedure acProjAddPageExecute(Sender: TObject);
    procedure acProjInsRectanExecute(Sender: TObject);
    procedure acProjPropExecute(Sender: TObject);
    procedure acVerConViewExecute(Sender: TObject);
    procedure acVerViewSupExecute(Sender: TObject);
    procedure acViewPropExecute(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure acToolbarConfigExecute(Sender: TObject);
    procedure ToolBar1Click(Sender: TObject);
  private
    curProject: TProject;
    ExploreProjPage: TDocPage;     //page selected in the project explorer
    ExploreProjView: TfraPaintBox;  //View selected in the project explorer
    procedure ConfigPropertiesChanged;
    procedure curProjectActivePageViewSendMessage(msg: string);
    procedure curProject_Modified;
    procedure curProject_ChangeState(ViewState: TViewState);
    procedure curProject_MouseMoveVirt(Shift: TShiftState; xp, yp: integer;
      xv, yv, zv: single);
    procedure curProject_ChangeActivePage;
    procedure fraExploreProj_ClickRightPage(Page: TDocPage);
    procedure fraExploreProj_ClickRightProj(Proj: TProject);
    procedure fraExploreProj_ClickRightView(View: TfraPaintBox);
    procedure curProject_ChangeView(View: TfraPaintBox);
    function MessageSaveChanges: integer;
    procedure RefreshEnvironment;
    procedure Refresh;
    procedure RefreshPanelView;
  public
    fraExploreProj: TfraExploreProject;  //Project Explorer
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}
const
  BUT_CANCEL = 3;

procedure TForm1.fraExploreProj_ClickRightProj(Proj: TProject);
begin
  PopupProject.PopUp;
end;

procedure TForm1.fraExploreProj_ClickRightPage(Page: TDocPage);
begin
  ExploreProjPage := Page;
  PopupPage.PopUp;
end;

procedure TForm1.fraExploreProj_ClickRightView(View: TfraPaintBox);
begin
  ExploreProjView := View;
  PopupView.PopUp;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  //Configure Project Explorer
  fraExploreProj := TfraExploreProject.Create(self);
  fraExploreProj.Parent := self;
  fraExploreProj.Name := 'fraExpProy';
  fraExploreProj.Caption := 'Explorador de Proyectos1';
  fraExploreProj.OnClickRightProject := @fraExploreProj_ClickRightProj;
  fraExploreProj.OnClickRightPage := @fraExploreProj_ClickRightPage;
  fraExploreProj.OnClickRightView := @fraExploreProj_ClickRightView;
  fraExploreProj.OnDeletePage := @acPageRemoveExecute;
  fraExploreProj.Initiate(@curProject);

  //Set the alignment
  fraExploreProj.Align := alLeft;
  Splitter2.Align := alLeft;
  fraExploreProj.Visible := True;
  panCommand.Align := alBottom;
  PageControl1.Align := alClient;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Config.SetLanguage('en');
  Config.Initiate(nil);  //Start the configuration
  Config.OnPropertiesChanged := @ConfigPropertiesChanged;
  ConfigPropertiesChanged;
  Refresh;
  acProjFileNewExecute(self);
  //acVerConViewExecute(self);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Config.writeIniFile();
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if curProject <> nil then
    curProject.Destroy;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
{Intercept the keyboard to manage it according to the chosen control}
begin
  if curProject = nil then
    exit;
  //Send all commands to the command box
  if TabSheet1.Focused then
    edit1.SetFocus//pass the focus
  else if PageControl1.Focused then
    edit1.SetFocus//pass the focus
  ;
end;

procedure TForm1.ConfigPropertiesChanged;
//Configuration properties are changed
begin
  StatusBar1.Visible := Config.fcGeneral.StatusBar;
  ToolBar1.Visible := Config.fcGeneral.ToolBar;
  case Config.fcGeneral.StateToolbar of
    stb_SmallIcon:
    begin
      ToolBar1.ButtonHeight := 22;
      ToolBar1.ButtonWidth := 22;
      ToolBar1.Height := 26;
      ToolBar1.Images := ImgActions16;
    end;
    stb_BigIcon:
    begin
      ToolBar1.ButtonHeight := 38;
      ToolBar1.ButtonWidth := 38;
      ToolBar1.Height := 42;
      ToolBar1.Images := ImgActions32;
    end;
  end;
end;

function TForm1.MessageSaveChanges: integer;
{Displays a window to confirm whether changes are saved or not. If selected
cancel, the BUT_CANCEL value is returned.}
var
  answer: byte;
begin
  if (curProject <> nil) and curProject.Modified then
  begin
    answer := MsgYesNoCancel('El presupuesto ha sido modificado, ¿Guardar cambios?');
    if answer = 3 then
      exit(BUT_CANCEL);
    if answer = 1 then
      curProject.SaveFile;
  end;
  Result := 0;   //Default value
end;

procedure TForm1.RefreshEnvironment;
{Refreshes the main screen to reflect the current state.}
  procedure AccionsModified(state: boolean);
  begin
    //disable modification options
    //    acPreAgrTab.Enabled   :=state;
    //    acPreActPresup.Enabled:=state;
    //    acPreImpTabler.Enabled:=state;
    //    acPreRplMarca.Enabled :=state;
    //    acTabAgrMater.Enabled :=state;
    //    acTabEliTab.Enabled   :=state;
    //    acMatDefNMat.Enabled  :=state;
    //    acMatSubir.Enabled    :=state;
    //    acMatBajar.Enabled    :=state;
    //    acMatElimin.Enabled   :=state;
  end;

  procedure AccionsReports(state: boolean);
  begin
    //    acRepPresXLS.Enabled:=state;
    //    acRepPresPDF.Enabled:=state;
    //    acRepDiaCAD.Enabled:=state;
    //    acRepMaterAgrup.Enabled:=state;
    //    acRepListTab.Enabled:=state;
    //    acRepMaterTab.Enabled:=state;
    //    acRepMaterResum.Enabled:=state;
  end;

  procedure MenuStateUpdate;
  {Update the state menus}
  begin
    //    if curProject = nil then begin
    //      acPreMarCrea.Enabled:=false;
    //      acPreMarTerm.Enabled:=false;
    //      acPreMarRev.Enabled:=false;
    //      acPreMarCerr.Enabled:=false;
    //    end else begin
    //      //Las opciones disponibles, dependen del state actual
    //      case curProject.state of
    //      epCreado  : begin
    //        acPreMarCrea.Enabled:=false;
    //        acPreMarTerm.Enabled:=true;
    //        acPreMarRev.Enabled:=false;
    //        acPreMarCerr.Enabled:=false;
    //        AccionsModified(true); //opciones de modificación
    //        end;
    //      epTerminado: begin
    //        acPreMarCrea.Enabled:=true;
    //        acPreMarTerm.Enabled:=false;
    //        acPreMarRev.Enabled:=true;
    //        acPreMarCerr.Enabled:=false;
    //        AccionsModified(true); //opciones de modificación
    //        end;
    //      epRevisado: begin
    //        acPreMarCrea.Enabled:=true;
    //        acPreMarTerm.Enabled:=false;
    //        acPreMarRev.Enabled:=false;
    //        acPreMarCerr.Enabled:=true;
    //        AccionsModified(true); //opciones de modificación
    //        end;
    //      epCerrado: begin
    //        acPreMarCrea.Enabled:=false;
    //        acPreMarTerm.Enabled:=false;
    //        acPreMarRev.Enabled:=false;
    //        acPreMarCerr.Enabled:=false;
    //        AccionsModified(false); //opciones de modificación
    //        end;
    //      end;
    //    end;
  end;

begin
  if curProject = nil then
    Caption := APP_NAME + ' ' + APP_VERSION//    //No hay presupuesto abierto
    //    mnPresup.Enabled:=false;  //desactiva todo el menú
    //    mnTablero.Enabled:=false;
    //    mnReportes.Enabled:=false;
    //    acProjFileSave.Enabled:=false;
    //    acArcGuarCom.Enabled:=false;
    //    acProjFileClose.Enabled:=false;
    //    acPreAgrTab.Enabled:=false;
    //    acPrePropied.Enabled:=false;
    //    acPreValidar.Enabled:=false;
    //    acMatBajar.Enabled:=false;
    //    acMatSubir.Enabled:=false;
    //    AccionsReports(false);
  else
    Caption := APP_NAME + ' ' + APP_VERSION + ' - ' +
      curProject.Name//There is an open project
    //    mnPresup.Enabled:=true;  //activa todo el menú
    //    mnTablero.Enabled:=true;
    //    mnReportes.Enabled:=true;
    //    acProjFileSave.Enabled := curProject.Modified;
    //    acArcGuarCom.Enabled:=true;
    //    acProjFileClose.Enabled:=true;
    //    acPreAgrTab.Enabled:=true;
    //    acPrePropied.Enabled:=true;
    //    acPreValidar.Enabled:=true;
    //    acMatBajar.Enabled:=true;
    //    acMatSubir.Enabled:=true;
    //    AccionsReports(true);
  ;
  //  menuRec.ActualMenusReciente(Self);
  MenuStateUpdate;  //update the menus state
end;

procedure TForm1.RefreshPanelView;
begin
  //  fraMotEdicion.motEdi.Refresh;
end;

procedure TForm1.Refresh;
{Rerfresca the entire interface}
begin
  RefreshEnvironment;
  fraExploreProj.Refresh;   //Refresh project explorer
  RefreshPanelView;
end;

procedure TForm1.curProject_Modified;
//Call when the project has been modified.
begin
  acProjFileSave.Enabled := True;
end;

procedure TForm1.curProject_ChangeState(ViewState: TViewState);
begin
  StatusBar1.Panels[0].Text := curProject.ActivePage.View.StateAsStr;
end;

procedure TForm1.curProject_MouseMoveVirt(Shift: TShiftState;
  xp, yp: integer; xv, yv, zv: single);
begin
  StatusBar1.Panels[3].Text :=
    'x=' + formatfloat('0.00', xv) + ' ' + 'y=' + formatfloat('0.00', yv) +
    ' ' + 'z=' + formatfloat('0.00', zv);
end;

procedure TForm1.curProject_ChangeView(View: TfraPaintBox);
begin
  StatusBar1.Panels[1].Text :=
    'Alfa=' + formatfloat('0.00', View.Alfa) + ' ' + 'Fi=' +
    formatfloat('0.00', View.Fi);
  //FloatToStr(fraMotEdicion.Alfa);
  StatusBar1.Panels[2].Text :=
    'Zoom=' + formatfloat('0.00', View.Zoom);
end;

procedure TForm1.curProject_ChangeActivePage;
{The active page of the current project was changed. Show it on the screen}
var
  ap: TDocPage;
begin
  if curProject = nil then
    exit;
  //Plug the viewer into PageControl1, to show it;
  curProject.HideAllPages; {Hide all the pages first because you may have
                                      already put your "Parent" in viewer.}
  ap := curProject.ActivePage;
  ap.View.Parent := TabSheet1;   //Place it here
  ap.View.Left := Random(200);
  ap.View.Top := Random(200);
  ap.View.Align := alClient;
  ap.View.Visible := True;  //it makes it visible
end;

function ComponentFromAction(Sender: TObject): TComponent;
  {Returns the component that triggered an action. If not object, returns NIL}
var
  compSource: TComponent;
begin
  if not (Sender is Taction) then
    exit(nil);
  compSource := TAction(Sender).ActionComponent;
  //We already have the source component
  if compSource is TMenuItem then
    exit(TMenuItem(compSource).GetParentComponent)//It's a menu item, but which one?
  else if compSource is TToolButton then
    exit(TToolButton(compSource).GetParentComponent)
    //It's a button on a toolbar, but which one?
  else
    exit(compSource)//It's another thing
  ;
end;

procedure TForm1.Edit1KeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  strCmd: string;
begin
  if curProject = nil then
    exit;
  if key = VK_RETURN then
  begin
    strCmd := trim(edit1.Text);
    if strCmd = '' then
      exit;
    strCmd := UpCase(strCmd);  //Convert upper case
    //Command entered.
    curProject.ActivePage.View.ExecuteCommand(strCmd);
  end
  else if key = VK_ESCAPE then
    curProject.ActivePage.View.ExecuteCommand('CANCEL')//Convert key to command
  ;
end;

procedure TForm1.curProjectActivePageViewSendMessage(msg: string);
{A message has arrived from the project}
begin
  Memo1.Lines.Add(Edit1.Text);
  Label1.Caption := msg;
  Edit1.Text := '';
end;
///////////////////////////// Actions ///////////////////////////////
procedure TForm1.acProjFileNewExecute(Sender: TObject);
var
  tmpProj: TProject;
begin
  //check if changes must be saved
  if MessageSaveChanges = BUT_CANCEL then
    exit;
  //Create temporary project
  tmpProj := TProject.Create;
  if not frmProject.ExecNew(tmpProj) then
  begin
    //it was canceled
    tmpProj.Destroy;  //It will not work
    exit;  //leaving the current project
  end;
  //Close current project and assign the temporary to the current one
  acProjFileCloseExecute(self);   //close current if it was open
  curProject := tmpProj;  //points to the temporary
  curProject.OnModify := @curProject_Modified;
  curProject.OnChangePersp := @curProject_ChangeView;
  curProject.OnChangeActivePage := @curProject_ChangeActivePage;
  curProject.OnMouseMoveVirt := @curProject_MouseMoveVirt;
  curProject.OnChangeState := @curProject_ChangeState;
  curProject.ActivePage.View.OnSendMessage := @curProjectActivePageViewSendMessage;
  curProject_ChangeActivePage;  //for Refresh in your viewer
  curProject.ActivePage.View.InitView;  //start the axes
  //curProject.Modified:=true;
  curProject.SaveFile;
  //  menuRec.AgregArcReciente(curProject.GenerarNombreArch);  //Add recent file
  Refresh;
end;

procedure TForm1.acProjFileSaveExecute(Sender: TObject);
begin

end;

procedure TForm1.acProjFileCloseExecute(Sender: TObject);
begin
  if curProject = nil then
    exit;  //there is no open project
  //check if there is a modified project
  if MessageSaveChanges = BUT_CANCEL then
    exit;
  curProject.Destroy;
  curProject := nil;   //marks it as closed
  Refresh;
end;

procedure TForm1.acProjFileLeaveExecute(Sender: TObject);
begin
  Self.Close;
end;

procedure TForm1.acToolbarDespExecute(Sender: TObject);
begin

end;

procedure TForm1.acToolbarPointExecute(Sender: TObject);
begin

end;

procedure TForm1.acToolbarRotExecute(Sender: TObject);
begin

end;

procedure TForm1.acVerConViewExecute(Sender: TObject);
begin
  if curProject = nil then
    exit;
  FormPerspective.Exec(curProject.ActivePage.View);
end;

procedure TForm1.acVerViewSupExecute(Sender: TObject);
begin
  if curProject = nil then
    exit;
  curProject.ActivePage.View.Alfa := 0;
  curProject.ActivePage.View.Fi := 0;
  curProject.ActivePage.View.viewEdi.Refresh;
end;

procedure TForm1.acViewPropExecute(Sender: TObject);
begin
  FormPropView.Exec(ExploreProjView);
end;

procedure TForm1.acProjAddPageExecute(Sender: TObject);
begin
  if curProject = nil then
    exit;  //there is no open project
  curProject.AddPage;
  Refresh;
end;

procedure TForm1.acProjPropExecute(Sender: TObject);
begin
  if curProject = nil then
    exit;
  if frmProject.Exec(curProject, @RefreshPanelView) then
  begin
    curProject.Modified := True;
    fraExploreProj.Refresh;
    RefreshPanelView;
  end;
end;

procedure TForm1.acPageAddLineExecute(Sender: TObject);  //Add line
var
  Page: TDocPage;
begin
  if curProject = nil then
    exit;
  {It checks if the action comes from the project explorer, to give you the
    possibility to take actions, on non-active pages}
  if ComponentFromAction(Sender) = PopupPage then
    Page := ExploreProjPage
  else
    Page := curProject.ActivePage;
  Page.View.ExecuteCommand('LINE');
  Refresh;
end;

procedure TForm1.acProjInsRectanExecute(Sender: TObject);
var
  Page: TDocPage;
begin
  if curProject = nil then
    exit;
  {It checks if the action comes from the project explorer, to give you the
    possibility to take actions, on non-active pages}
  if ComponentFromAction(Sender) = PopupPage then
    Page := ExploreProjPage
  else
    Page := curProject.ActivePage;
  Page.View.ExecuteCommand('RECTANGLE');
  Refresh;
end;

procedure TForm1.acPageRemoveExecute(Sender: TObject);
begin
  if curProject = nil then
    exit;
  if MsgYesNo('¿Eliminar página "%s"?', [curProject.ActivePage.Name]) <> 1 then
    exit;
  curProject.RemovePage(curProject.ActivePage);
  Refresh;
end;

procedure TForm1.acToolbarConfigExecute(Sender: TObject);
begin
  Config.doConfig();
end;

procedure TForm1.ToolBar1Click(Sender: TObject);
begin

end;

end.
