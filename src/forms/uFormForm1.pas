unit uFormForm1;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, ActnList, Menus,
  StdCtrls, ComCtrls, LCLProc, LCLType, Buttons, Dialogs,
  //local
  sketchDocument, uFrameEditor, uFormDocument,
  uFrameDocumetExplorer, uFormViewProp,
  sketchEditor
  ;

const
  NUM_CUAD = 20;
  ZOOM_INI = 12;

type

  { TForm1 }
  TForm1 = class(TForm)
  published
    acDocProp: TAction;
    acPageProp: TAction;
    acDrawLinePoly: TAction;
    acViewProp: TAction;
    btnPoly: TToolButton;
    btnSep1: TToolButton;
    cmdInput: TEdit;
    cmdInputLabel: TLabel;
    cmdInputWrapper: TPanel;
    cmdMessages: TMemo;
    cmdRunBtn: TSpeedButton;
    frameDocumentExplorer: TFrameDocumentExplorer;
    mfileNewCustom: TMenuItem;
    mfileExit: TMenuItem;
    mDoc: TMenuItem;
    mdocInsertPoly: TMenuItem;
    pobjectsItem1: TMenuItem;
    pdocProperies: TMenuItem;
    mdocProperties: TMenuItem;
    pdocAddPage: TMenuItem;
    mdocAddPage: TMenuItem;
    mPage: TMenuItem;
    mpageRename: TMenuItem;
    mpageRemove: TMenuItem;
    mpageProperies: TMenuItem;
    ppageRename: TMenuItem;
    ppageRemove: TMenuItem;
    ppageProperties: TMenuItem;
    pviewProperties: TMenuItem;
    ppageAddlPoly: TMenuItem;
    mviewProps: TMenuItem;
    MenuItem5: TMenuItem;
    actions: TActionList;
    Images16: TImageList;
    Images32: TImageList;
    menuMain: TMainMenu;
    mFile: TMenuItem;
    mView: TMenuItem;
    mfileClose: TMenuItem;
    PageControl1: TPageControl;
    Splitter1: TSplitter;
    splitterVert1: TSplitter;
    StatusBar: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    btnNewCustomDoc: TToolButton;
    WrapperBottom: TPanel;
    WrapperMiddle: TPanel;
    pmenuView: TPopupMenu;
    pmenuPage: TPopupMenu;
    pmenuDoc: TPopupMenu;
    pmenuObjects: TPopupMenu;
    ToolBar: TToolBar;
    procedure acDrawLinePolyExecute(Sender: TObject);
    procedure acPagePropExecute(Sender: TObject);
    procedure acDocInsRectanExecute(Sender: TObject);
    procedure acDocPropExecute(Sender: TObject);
    procedure acToolbarPointExecute(Sender: TObject);
    procedure acVerViewSupExecute(Sender: TObject);
    procedure acViewPropExecute(Sender: TObject);
    procedure btnNewCustomDocClick(Sender: TObject);
    procedure cmdMessagesChange(Sender: TObject);
    procedure cmdRunBtnClick(Sender: TObject);
    procedure cmdInputKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure acToolbarConfigExecute(Sender: TObject);
    procedure mdocAddPageClick(Sender: TObject);
    procedure mFileClick(Sender: TObject);
    procedure mfileCloseClick(Sender: TObject);
    procedure mfileExitClick(Sender: TObject);
    procedure mfileNewCustomClick(Sender: TObject);
    procedure mpageRemoveClick(Sender: TObject);
    procedure mviewPropsClick(Sender: TObject);
    procedure pdocAddPageClick(Sender: TObject);
    procedure ppageRemoveClick(Sender: TObject);
    procedure SplitterHorCanOffset(Sender: TObject; var NewOffset: Integer;
      var Accept: Boolean);
    procedure ToolBarClick(Sender: TObject);
    procedure WrapperBottomClick(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  public
    doc: TDocument;
    ExploreDocPage: TDocPage;     //page selected in the document explorer
    ExploreDocView: TframeEditor;  //View selected in the document explorer
    public
    procedure curDocumentActivePageViewSendMessage(msg: string);
    procedure curDocumentChangeState(ViewState: TViewState);
    procedure curDocumentMouseMoveVirt(Shift: TShiftState; xp, yp: integer;
      xv, yv, zv: single);
    procedure curDocumentChangeActivePage;
    procedure frameExploreDocClickRightPage(Page: TDocPage);
    procedure fraExploreDocClickRightDoc(argDoc: TDocument);
    procedure frameExploreDocClickRightView(View: TframeEditor);
    procedure curDocumentChangeView(View: TframeEditor);
    function MessageSaveChanges: integer;
    procedure form1SetCaption;
    procedure Refresh;

  end;

implementation
uses glob;
{$R *.lfm}

procedure TForm1.fraExploreDocClickRightDoc(argDoc: TDocument);
begin
  pmenuDoc.PopUp;
end;

procedure TForm1.frameExploreDocClickRightPage(Page: TDocPage);
begin
  ExploreDocPage := Page;
  pmenuPage.PopUp;
end;

procedure TForm1.frameExploreDocClickRightView(View: TframeEditor);
begin
  ExploreDocView := View;
  pmenuView.PopUp;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  //Configure Document Explorer
  frameDocumentExplorer.Name := 'frameDocumentExplorer';
  frameDocumentExplorer.OnClickRightDocument := @fraExploreDocClickRightDoc;
  frameDocumentExplorer.OnClickRightPage := @frameExploreDocClickRightPage;
  frameDocumentExplorer.OnClickRightView := @frameExploreDocClickRightView;
  frameDocumentExplorer.Initiate(@doc);

  //translating
  mFile.Caption :=msg.get('file');
  mfileNewCustom.Caption :=msg.get('newCustomDocument');
  mfileClose.Caption :=msg.get('close');
  mfileExit.Caption :=msg.get('exit');
  //
  mView.Caption :=msg.get('view');
  mviewProps.Caption :=msg.get('properties');
  //
  mDoc.Caption :=msg.get('document');
  mdocAddPage.Caption:=msg.get('addPage');
  mdocInsertPoly.Caption :=msg.get('insertPolyline');
  mdocProperties.Caption :=msg.get('properties');
  //
  mPage.Caption :=msg.get('page');
  mpageRename.Caption :=msg.get('rename');
  mpageRemove.Caption :=msg.get('remove');
  mpageProperies.Caption :=msg.get('properties');
  //
  pdocAddPage.Caption :=msg.get('addPage');
  pdocProperies.Caption :=msg.get('properties');
  //
  pobjectsItem1.Caption :=msg.get('insertPolyline');
  //
  ppageAddlPoly.Caption :=msg.get('addLine');
  ppageRename.Caption :=msg.get('rename');
  ppageRemove.Caption :=msg.get('remove');
  ppageProperties.Caption :=msg.get('properties');
  //
  pviewProperties.Caption :=msg.get('properties');
  // ToolBar.
  btnPoly.Caption :=msg.get('addLine');
  //
  TabSheet1.Caption :=msg.get('editor');
  TabSheet2.Caption :=msg.get('TabSheet2');
  //
  cmdInputLabel.Caption :=msg.get('commandPrompt');
  //
  frameDocumentExplorer.Caption := msg.Get('DocumentExplorer');

end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Refresh;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if doc <> nil then
    doc.Destroy;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
{Intercept the keyboard to manage it according to the chosen control}
begin
  if doc = nil then
    exit;
  //Send all commands to the command box
  if TabSheet1.Focused then
    cmdInput.SetFocus//pass the focus
  else if PageControl1.Focused then
    cmdInput.SetFocus//pass the focus
  ;
end;

function TForm1.MessageSaveChanges: integer;
{Displays a window to confirm whether changes are saved or not. If selected
cancel, the glob.BUT_CANCEL value is returned.}
var
  answer: Integer;
begin
  if (doc <> nil) and doc.Modified then
  begin
    answer := Application.MessageBox(PChar(msg.Get('DocumentModifiedSaveChanges')), 'Question',
           (MB_ICONQUESTION + MB_YESNO));
    if answer = IDNO then
      exit(glob.BUT_CANCEL);
    if answer = IDYES then
      doc.SaveFile;
  end;
  Result := 0;   //Default value
end;

procedure TForm1.form1SetCaption;
begin
  if doc = nil then
    Caption := APP_NAME + ' ' + APP_VERSION
  else
    Caption := APP_NAME + ' ' + APP_VERSION + ' - ' +
      doc.Name;
end;

procedure TForm1.Refresh;
{Refresh the entire interface}
begin
  form1SetCaption;
  frameDocumentExplorer.Refresh; 
end;



procedure TForm1.curDocumentChangeState(ViewState: TViewState);
begin
  StatusBar.Panels[0].Text := doc.ActivePage.View.StateAsStr;
end;

procedure TForm1.curDocumentMouseMoveVirt(Shift: TShiftState;
  xp, yp: integer; xv, yv, zv: single);
begin
  StatusBar.Panels[3].Text :=
    'x=' + formatfloat('0.00', xv) + ' ' + 'y=' + formatfloat('0.00', yv) +
    ' ' + 'z=' + formatfloat('0.00', zv);
end;

procedure TForm1.curDocumentChangeView(View: TframeEditor);
begin
  StatusBar.Panels[1].Text :=
    'Alfa=' + formatfloat('0.00', View.Alfa) + ' ' + 'Fi=' +
    formatfloat('0.00', View.Fi);
  //FloatToStr(fraMotEdicion.Alfa);
  StatusBar.Panels[2].Text :=
    'Zoom=' + formatfloat('0.00', View.Zoom);
end;

procedure TForm1.curDocumentChangeActivePage;
{The active page of the current document was changed. Show it on the screen}
var
  ap: TDocPage;
begin
  if doc = nil then
    exit;
  //Plug the viewer into PageControl1, to show it;
  doc.HideAllPages; {Hide all the pages first because you may have
                                      already put your "Parent" in viewer.}
  ap := doc.ActivePage;
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
  if doc = nil then
    exit;
  if key = VK_RETURN then
  begin
    strCmd := trim(cmdInput.Text);
    if strCmd = '' then
      exit;
    strCmd := UpCase(strCmd);  //Convert upper case
    //Command entered.
    doc.ActivePage.View.ExecuteCommand(strCmd);
  end
  else if key = VK_ESCAPE then
    doc.ActivePage.View.ExecuteCommand('CANCEL')//Convert key to command
  ;
end;

procedure TForm1.curDocumentActivePageViewSendMessage(msg: string);
{A message has arrived from the document}
begin
  cmdMessages.Lines.Add(cmdInput.Text);
  cmdInputLabel.Caption := msg;
  cmdInput.Text := '';
end;
///////////////////////////// Actions ///////////////////////////////




procedure TForm1.acVerViewSupExecute(Sender: TObject);
begin
  if doc = nil then
    exit;
  doc.ActivePage.View.Alfa := 0;
  doc.ActivePage.View.Fi := 0;
  doc.ActivePage.View.Editor.Refresh;
end;

procedure TForm1.acViewPropExecute(Sender: TObject);
begin
  formViewProp.Exec(ExploreDocView);
end;

procedure TForm1.btnNewCustomDocClick(Sender: TObject);
begin
  hDoc.doNewCustom(self);
end;

procedure TForm1.cmdMessagesChange(Sender: TObject);
begin

end;

procedure TForm1.cmdRunBtnClick(Sender: TObject);
begin

end;

procedure TForm1.acDocPropExecute(Sender: TObject);
begin
  if doc = nil then
    exit;
  if formDocument.Exec(doc) then
  begin
    doc.Modified := True;
    frameDocumentExplorer.Refresh;
  end;
end;

procedure TForm1.acToolbarPointExecute(Sender: TObject);
begin

end;

procedure TForm1.acDrawLinePolyExecute(Sender: TObject);  //Add line
var
  Page: TDocPage;
begin
  if doc = nil then
    exit;
  {It checks if the action comes from the document explorer, to give you the
    possibility to take actions, on non-active pages}
  if ComponentFromAction(Sender) = pmenuPage then
    Page := ExploreDocPage
  else
    Page := doc.ActivePage;
  Page.View.ExecuteCommand('LINE');
  Refresh;
end;

procedure TForm1.acPagePropExecute(Sender: TObject);
begin

end;

procedure TForm1.acDocInsRectanExecute(Sender: TObject);
var
  Page: TDocPage;
begin
  if doc = nil then
    exit;
  {It checks if the action comes from the document explorer, to give you the
    possibility to take actions, on non-active pages}
  if ComponentFromAction(Sender) = pmenuPage then
    Page := ExploreDocPage
  else
    Page := doc.ActivePage;
  Page.View.ExecuteCommand('RECTANGLE');
  Refresh;
end;

procedure TForm1.acToolbarConfigExecute(Sender: TObject);
begin

end;

procedure TForm1.mdocAddPageClick(Sender: TObject);
begin
  hpage.doAdd(self);
end;

procedure TForm1.mFileClick(Sender: TObject);
begin

end;

procedure TForm1.mfileCloseClick(Sender: TObject);
begin
  hdoc.doClose(self);
end;

procedure TForm1.mfileExitClick(Sender: TObject);
begin
  self.close;
end;

procedure TForm1.mfileNewCustomClick(Sender: TObject);
begin
  hdoc.doNewCustom(self);
end;

procedure TForm1.mpageRemoveClick(Sender: TObject);
begin
  hpage.doRemove(self);
end;

procedure TForm1.mviewPropsClick(Sender: TObject);
begin

end;

procedure TForm1.pdocAddPageClick(Sender: TObject);
begin
  hpage.doAdd(self);
end;

procedure TForm1.ppageRemoveClick(Sender: TObject);
begin
  hpage.doRemove(self);
end;

procedure TForm1.SplitterHorCanOffset(Sender: TObject; var NewOffset: Integer;
  var Accept: Boolean);
begin

end;

procedure TForm1.ToolBarClick(Sender: TObject);
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
