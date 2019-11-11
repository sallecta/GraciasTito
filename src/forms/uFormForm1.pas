unit uFormForm1;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, ActnList, Menus,
  StdCtrls, ComCtrls, LCLProc, LCLType, Buttons,
  sketchDocument, uFramePaintBox, uFormProject,
  glob, uFramePojectExplorer, uFormPerspective, uFormViewProp,
  sketchEditor,
  Dialogs;

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
    cmdInput: TEdit;
    cmdInputLabel: TLabel;
    cmdInputWrapper: TPanel;
    cmdMessages: TMemo;
    cmdRunBtn: TSpeedButton;
    frameProjectExplorer: TframeProjectExplorer;
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
    Splitter1: TSplitter;
    splitterVert1: TSplitter;
    StatusBar: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    WrapperBottom: TPanel;
    WrapperMiddle: TPanel;
    PopupView: TPopupMenu;
    PopupPage: TPopupMenu;
    PopupProject: TPopupMenu;
    PopupObjects: TPopupMenu;
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
    procedure acProjNew_onExecute(Sender: TObject);
    procedure acProjFileLeaveExecute(Sender: TObject);
    procedure acPageAddLineExecute(Sender: TObject);
    procedure acPageRemoveExecute(Sender: TObject);
    procedure acProjAddPageExecute(Sender: TObject);
    procedure acProjInsRectanExecute(Sender: TObject);
    procedure acProjPropExecute(Sender: TObject);
    procedure acVerConViewExecute(Sender: TObject);
    procedure acVerViewSupExecute(Sender: TObject);
    procedure acViewPropExecute(Sender: TObject);
    procedure cmdMessagesChange(Sender: TObject);
    procedure cmdRunBtnClick(Sender: TObject);
    procedure cmdInputKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure acToolbarConfigExecute(Sender: TObject);
    procedure SplitterHorCanOffset(Sender: TObject; var NewOffset: Integer;
      var Accept: Boolean);
    procedure WrapperBottomClick(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private
    priv_curProject: TProject;
    priv_ExploreProjPage: TDocPage;     //page selected in the project explorer
    priv_ExploreProjView: TFramePaintBox;  //View selected in the project explorer
    procedure priv_ConfigPropertiesChanged;
    procedure priv_curProjectActivePageViewSendMessage(msg: string);
    procedure priv_curProjectModified;
    procedure priv_curProjectChangeState(ViewState: TViewState);
    procedure priv_curProjectMouseMoveVirt(Shift: TShiftState; xp, yp: integer;
      xv, yv, zv: single);
    procedure priv_curProjectChangeActivePage;
    procedure priv_frameExploreProjClickRightPage(Page: TDocPage);
    procedure priv_fraExploreProjClickRightProj(Proj: TProject);
    procedure priv_frameExploreProjClickRightView(View: TFramePaintBox);
    procedure priv_curProjectChangeView(View: TFramePaintBox);
    function priv_MessageSaveChanges: integer;
    procedure priv_form1SetCaption;
    procedure priv_Refresh;
  public
    //frameProjectExplorer: TframeProjectExplorer;  //Project Explorer
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}
const
  BUT_CANCEL = 3;

procedure TForm1.priv_fraExploreProjClickRightProj(Proj: TProject);
begin
  PopupProject.PopUp;
end;

procedure TForm1.priv_frameExploreProjClickRightPage(Page: TDocPage);
begin
  priv_ExploreProjPage := Page;
  PopupPage.PopUp;
end;

procedure TForm1.priv_frameExploreProjClickRightView(View: TFramePaintBox);
begin
  priv_ExploreProjView := View;
  PopupView.PopUp;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  //Configure Project Explorer
  frameProjectExplorer.Name := 'frameProjectExplorer';
  frameProjectExplorer.OnClickRightProject := @priv_fraExploreProjClickRightProj;
  frameProjectExplorer.OnClickRightPage := @priv_frameExploreProjClickRightPage;
  frameProjectExplorer.OnClickRightView := @priv_frameExploreProjClickRightView;
  frameProjectExplorer.OnDeletePage := @acPageRemoveExecute;
  frameProjectExplorer.Initiate(@priv_curProject);

  //Set the alignment
  //frameProjectExplorer.Align := alLeft;
  //SplitterVert.Align := alLeft;
  //frameProjectExplorer.Visible := True;
  //WrapperBottom.Align := alBottom;
  //PageControl1.Align := alClient;

  //translating
  MenuItem1.Caption :=msg.get('file');
  MenuItem11.Caption :=msg.get('newProject');
  MenuItem10.Caption :=msg.get('open');
  MenuItem9.Caption :=msg.get('save');
  MenuItem8.Caption :=msg.get('close');
  MenuItem2.Caption :='-';
  MenuItem12.Caption :=msg.get('exit');
  //
  MenuItem3.Caption :=msg.get('view');
  MenuItem4.Caption :=msg.get('newitem8');
  MenuItem5.Caption :=msg.get('properties');
  //
  MenuItem13.Caption :=msg.get('project');
  MenuItem22.Caption :=msg.get('addPage');
  MenuItem20.Caption :=msg.get('insertRectangle');
  MenuItem14.Caption :=msg.get('insertPolyline');
  MenuItem19.Caption :=msg.get('properties');
  //
  MenuItem23.Caption :=msg.get('page');
  MenuItem24.Caption :=msg.get('rename');
  MenuItem25.Caption :=msg.get('remove');
  MenuItem26.Caption :=msg.get('properties');
  //
  MenuItem6.Caption :=msg.get('tools');
  MenuItem7.Caption :=msg.get('config');
  //
  MenuItem21.Caption :=msg.get('addPage');
  MenuItem15.Caption :=msg.get('insert');
  MenuItem16.Caption :=msg.get('insertPolyline');
  MenuItem18.Caption :=msg.get('properties');
  //
  MenuItem17.Caption :=msg.get('insertPolyline');
  //
  MenuItem31.Caption :=msg.get('addLine');
  MenuItem27.Caption :=msg.get('rename');
  MenuItem28.Caption :=msg.get('remove');
  MenuItem29.Caption :=msg.get('properties');
  //
  MenuItem30.Caption :=msg.get('properties');
  // Toolbar1.
  ToolButton1.Caption :=msg.get('rotate'); 
  ToolButton2.Caption :=msg.get('rotate');
  ToolButton3.Caption :=msg.get('rotate');
  ToolButton4.Caption :=msg.get('ToolButton4');
  ToolButton6.Caption :=msg.get('newProject');
  ToolButton7.Caption :=msg.get('save');
  ToolButton8.Caption :=msg.get('ToolButton8');
  ToolButton9.Caption :=msg.get('config');
  ToolButton10.Caption :=msg.get('open');
  ToolButton5.Caption :=msg.get('viewSuperior');
  ToolButton11.Caption :=msg.get('acProjInsRectan');
  ToolButton12.Caption :=msg.get('insertPolyline');
  ToolButton13.Caption :=msg.get('addLine');
  //
  TabSheet1.Caption :=msg.get('editor');
  TabSheet2.Caption :=msg.get('TabSheet2');
  //
  cmdInputLabel.Caption :=msg.get('commandPrompt');
  //btnRubtnRunCmd_oldion :=msg.get('');
  ToolButton13.Caption :=msg.get('addLine');
  ToolButton13.Caption :=msg.get('addLine');
  ToolButton13.Caption :=msg.get('addLine');
  ToolButton13.Caption :=msg.get('addLine');
  ToolButton13.Caption :=msg.get('addLine');
  ToolButton13.Caption :=msg.get('addLine');
  ToolButton13.Caption :=msg.get('addLine');
  ToolButton13.Caption :=msg.get('addLine');
  ToolButton13.Caption :=msg.get('addLine');
  ToolButton13.Caption :=msg.get('addLine');
  ToolButton13.Caption :=msg.get('addLine');
  ToolButton13.Caption :=msg.get('addLine');
  ToolButton13.Caption :=msg.get('addLine');
  ToolButton13.Caption :=msg.get('addLine');
  ToolButton13.Caption :=msg.get('addLine');
  //
  frameProjectExplorer.Caption := msg.Get('ProjectExplorer');
  
  //assign actions to functions
  //acProjFileNew.OnExecute := @acProjNew_onExecute;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  priv_ConfigPropertiesChanged;
  priv_Refresh;
  //acProjNew_onExecute(self);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if priv_curProject <> nil then
    priv_curProject.Destroy;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
{Intercept the keyboard to manage it according to the chosen control}
begin
  if priv_curProject = nil then
    exit;
  //Send all commands to the command box
  if TabSheet1.Focused then
    cmdInput.SetFocus//pass the focus
  else if PageControl1.Focused then
    cmdInput.SetFocus//pass the focus
  ;
end;

procedure TForm1.priv_ConfigPropertiesChanged;
//Configuration properties are changed
begin
  StatusBar.Visible := true;
  ToolBar1.Visible := true;
  ToolBar1.ButtonHeight := 38;
  ToolBar1.ButtonWidth := 38;
  ToolBar1.Height := 42;
  ToolBar1.Images := ImgActions32;
end;

function TForm1.priv_MessageSaveChanges: integer;
{Displays a window to confirm whether changes are saved or not. If selected
cancel, the BUT_CANCEL value is returned.}
var
  answer: Integer;
begin
  if (priv_curProject <> nil) and priv_curProject.Modified then
  begin
    answer := Application.MessageBox(PChar(msg.Get('ProjectModifiedSaveChanges')), 'Question',
           (MB_ICONQUESTION + MB_YESNO));
    if answer = IDNO then
      exit(BUT_CANCEL);
    if answer = IDYES then
      priv_curProject.SaveFile;
  end;
  Result := 0;   //Default value
end;

procedure TForm1.priv_form1SetCaption;
begin
  if priv_curProject = nil then
    Caption := APP_NAME + ' ' + APP_VERSION
  else
    Caption := APP_NAME + ' ' + APP_VERSION + ' - ' +
      priv_curProject.Name;
end;

procedure TForm1.priv_Refresh;
{Refresh the entire interface}
begin
  priv_form1SetCaption;
  frameProjectExplorer.Refresh; 
end;

procedure TForm1.priv_curProjectModified;
//Call when the project has been modified.
begin
  acProjFileSave.Enabled := True;
end;

procedure TForm1.priv_curProjectChangeState(ViewState: TViewState);
begin
  StatusBar.Panels[0].Text := priv_curProject.ActivePage.View.StateAsStr;
end;

procedure TForm1.priv_curProjectMouseMoveVirt(Shift: TShiftState;
  xp, yp: integer; xv, yv, zv: single);
begin
  StatusBar.Panels[3].Text :=
    'x=' + formatfloat('0.00', xv) + ' ' + 'y=' + formatfloat('0.00', yv) +
    ' ' + 'z=' + formatfloat('0.00', zv);
end;

procedure TForm1.priv_curProjectChangeView(View: TFramePaintBox);
begin
  StatusBar.Panels[1].Text :=
    'Alfa=' + formatfloat('0.00', View.Alfa) + ' ' + 'Fi=' +
    formatfloat('0.00', View.Fi);
  //FloatToStr(fraMotEdicion.Alfa);
  StatusBar.Panels[2].Text :=
    'Zoom=' + formatfloat('0.00', View.Zoom);
end;

procedure TForm1.priv_curProjectChangeActivePage;
{The active page of the current project was changed. Show it on the screen}
var
  ap: TDocPage;
begin
  if priv_curProject = nil then
    exit;
  //Plug the viewer into PageControl1, to show it;
  priv_curProject.HideAllPages; {Hide all the pages first because you may have
                                      already put your "Parent" in viewer.}
  ap := priv_curProject.ActivePage;
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

procedure TForm1.cmdInputKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  strCmd: string;
begin
  if priv_curProject = nil then
    exit;
  if key = VK_RETURN then
  begin
    strCmd := trim(cmdInput.Text);
    if strCmd = '' then
      exit;
    strCmd := UpCase(strCmd);  //Convert upper case
    //Command entered.
    priv_curProject.ActivePage.View.ExecuteCommand(strCmd);
  end
  else if key = VK_ESCAPE then
    priv_curProject.ActivePage.View.ExecuteCommand('CANCEL')//Convert key to command
  ;
end;

procedure TForm1.priv_curProjectActivePageViewSendMessage(msg: string);
{A message has arrived from the project}
begin
  cmdMessages.Lines.Add(cmdInput.Text);
  cmdInputLabel.Caption := msg;
  cmdInput.Text := '';
end;
///////////////////////////// Actions ///////////////////////////////
procedure TForm1.acProjNew_onExecute(Sender: TObject);
var
  tmpProj: TProject;
begin
  //check if changes must be saved
  if priv_MessageSaveChanges = BUT_CANCEL then
  begin
	//WriteLn('butcancel');
    exit;
  end;
  //Create temporary project
  tmpProj := TProject.Create;
  if not formProject.ExecNew(tmpProj) then
  begin
    //it was canceled
    tmpProj.Destroy;  //It will not work
    exit;  //leaving the current project
  end;
  //Close current project and assign the temporary to the current one
  acProjFileCloseExecute(self);   //close current if it was open
  priv_curProject := tmpProj;  //points to the temporary
  priv_curProject.OnModify := @priv_curProjectModified;
  priv_curProject.OnChangePersp := @priv_curProjectChangeView;
  priv_curProject.OnChangeActivePage := @priv_curProjectChangeActivePage;
  priv_curProject.OnMouseMoveVirt := @priv_curProjectMouseMoveVirt;
  priv_curProject.OnChangeState := @priv_curProjectChangeState;
  priv_curProject.ActivePage.View.OnSendMessage := @priv_curProjectActivePageViewSendMessage;
  priv_curProjectChangeActivePage;  //for priv_Refresh in your viewer
  priv_curProject.ActivePage.View.InitView;  //start the axes
  priv_curProject.SaveFile;
  priv_Refresh;
end;

procedure TForm1.acProjFileCloseExecute(Sender: TObject);
begin
  if priv_curProject = nil then
    exit;  //there is no open project
  //check if there is a modified project
  if priv_MessageSaveChanges = BUT_CANCEL then
    exit;
  priv_curProject.Destroy;
  priv_curProject := nil;   //marks it as closed
  priv_Refresh;
end;

procedure TForm1.acProjFileLeaveExecute(Sender: TObject);
begin
  Self.Close;
end;

procedure TForm1.acVerConViewExecute(Sender: TObject);
begin
  if priv_curProject = nil then
    exit;
  formPerspective.Exec(priv_curProject.ActivePage.View);
end;

procedure TForm1.acVerViewSupExecute(Sender: TObject);
begin
  if priv_curProject = nil then
    exit;
  priv_curProject.ActivePage.View.Alfa := 0;
  priv_curProject.ActivePage.View.Fi := 0;
  priv_curProject.ActivePage.View.Editor.Refresh;
end;

procedure TForm1.acViewPropExecute(Sender: TObject);
begin
  formViewProp.Exec(priv_ExploreProjView);
end;

procedure TForm1.cmdMessagesChange(Sender: TObject);
begin

end;

procedure TForm1.cmdRunBtnClick(Sender: TObject);
begin

end;

procedure TForm1.acProjAddPageExecute(Sender: TObject);
begin
  if priv_curProject = nil then
    exit;  //there is no open project
  priv_curProject.AddPage;
  priv_Refresh;
end;

procedure TForm1.acProjPropExecute(Sender: TObject);
begin
  if priv_curProject = nil then
    exit;
  if formProject.Exec(priv_curProject) then
  begin
    priv_curProject.Modified := True;
    frameProjectExplorer.Refresh;
  end;
end;

procedure TForm1.acPageAddLineExecute(Sender: TObject);  //Add line
var
  Page: TDocPage;
begin
  if priv_curProject = nil then
    exit;
  {It checks if the action comes from the project explorer, to give you the
    possibility to take actions, on non-active pages}
  if ComponentFromAction(Sender) = PopupPage then
    Page := priv_ExploreProjPage
  else
    Page := priv_curProject.ActivePage;
  Page.View.ExecuteCommand('LINE');
  priv_Refresh;
end;

procedure TForm1.acProjInsRectanExecute(Sender: TObject);
var
  Page: TDocPage;
begin
  if priv_curProject = nil then
    exit;
  {It checks if the action comes from the project explorer, to give you the
    possibility to take actions, on non-active pages}
  if ComponentFromAction(Sender) = PopupPage then
    Page := priv_ExploreProjPage
  else
    Page := priv_curProject.ActivePage;
  Page.View.ExecuteCommand('RECTANGLE');
  priv_Refresh;
end;



procedure TForm1.acPageRemoveExecute(Sender: TObject);
begin
  if priv_curProject = nil then
    exit;
  if Application.MessageBox(PChar(msg.Get('AskPageDelete')+priv_curProject.ActivePage.Name), '', (MB_ICONQUESTION + MB_YESNO)) <> IDYES then
    exit;
  priv_curProject.RemovePage(priv_curProject.ActivePage);
  priv_Refresh;
end;

procedure TForm1.acToolbarConfigExecute(Sender: TObject);
begin

end;

procedure TForm1.SplitterHorCanOffset(Sender: TObject; var NewOffset: Integer;
  var Accept: Boolean);
begin

end;

procedure TForm1.WrapperBottomClick(Sender: TObject);
begin

end;

procedure TForm1.Label2Click(Sender: TObject);
begin

end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin

end;

begin



end.
