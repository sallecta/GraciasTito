unit uForm1;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, ActnList, Menus,
  StdCtrls, ComCtrls, LCLProc, LCLType, Buttons, Dialogs,
  //local
  uDoc, uFrameEditor, uFormDocProps,
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
    btnPoly: TToolButton;
    btnSep1: TToolButton;
    cmdInput: TEdit;
    cmdInputLabel: TLabel;
    cmdInputWrapper: TPanel;
    cmdMessages: TMemo;
    cmdRunBtn: TSpeedButton;
    frameDocumentExplorer: TFrameDocumentExplorer;
    mDraw: TMenuItem;
    mdrawPoly: TMenuItem;
    mpageAdd: TMenuItem;
    mfileNewReserved: TMenuItem;
    mfileExit: TMenuItem;
    mDoc: TMenuItem;
    pobjectsItem1: TMenuItem;
    pdocProperies: TMenuItem;
    mdocProperties: TMenuItem;
    pdocAddPage: TMenuItem;
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
    tabEditor: TTabSheet;
    TabSheet2: TTabSheet;
    btnNewReservedDoc: TToolButton;
    WrapperBottom: TPanel;
    WrapperMiddle: TPanel;
    pmenuView: TPopupMenu;
    pmenuPage: TPopupMenu;
    pmenuDoc: TPopupMenu;
    pmenuObjects: TPopupMenu;
    ToolBar: TToolBar;
    procedure acToolbarPointExecute(Sender: TObject);
    procedure acVerViewSupExecute(Sender: TObject);
    procedure btnNewReservedDocClick(Sender: TObject);
    procedure btnPolyClick(Sender: TObject);
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
    procedure mdocInsertPolyClick(Sender: TObject);
    procedure mdocPropertiesClick(Sender: TObject);
    procedure mdrawPolyClick(Sender: TObject);
    procedure mFileClick(Sender: TObject);
    procedure mfileCloseClick(Sender: TObject);
    procedure mfileExitClick(Sender: TObject);
    procedure mfileNewReservedClick(Sender: TObject);
    procedure mpageAddClick(Sender: TObject);
    procedure mpageRemoveClick(Sender: TObject);
    procedure mviewPropsClick(Sender: TObject);
    procedure pdocAddPageClick(Sender: TObject);
    procedure ppageRemoveClick(Sender: TObject);
    procedure pviewPropertiesClick(Sender: TObject);
    procedure SplitterHorCanOffset(Sender: TObject; var NewOffset: Integer;
      var Accept: Boolean);
    procedure ToolBarClick(Sender: TObject);
    procedure WrapperBottomClick(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  public
    doc: TDoc;
    DocumetExplorer_page: TDoc.TDocPage;     //page selected in the document explorer
    DocumetExplorer_frameEditor: TframeEditor;  //frameEditor selected in the document explorer
    public
    procedure curDocumentChangeState(ViewState: TViewState);
    procedure curDocumentMouseMoveVirt(Shift: TShiftState; xp, yp: integer;
      xv, yv, zv: single);
    procedure showActivePage;
    procedure frameExploreDocClickRightPage(Page: TDoc.TDocPage);
    procedure fraExploreDocClickRightDoc(argDoc: TDoc);
    procedure frameExploreDocClickRightView(frameEditor: TframeEditor);
    procedure form1SetCaption;
    procedure Refresh;

  end;

implementation
uses glob;
{$R *.lfm}

procedure TForm1.fraExploreDocClickRightDoc(argDoc: TDoc);
begin
  pmenuDoc.PopUp;
end;

procedure TForm1.frameExploreDocClickRightPage(Page: TDoc.TDocPage);
begin
  DocumetExplorer_page := Page;
  pmenuPage.PopUp;
end;

procedure TForm1.frameExploreDocClickRightView(frameEditor: TframeEditor);
begin
  DocumetExplorer_frameEditor := frameEditor;
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
  mfileNewReserved.Caption :=msg.get('newReservedDocument');
  mfileClose.Caption :=msg.get('close');
  mfileExit.Caption :=msg.get('exit');
  //
  mView.Caption :=msg.get('frameEditor');
  mviewProps.Caption :=msg.get('properties');
  //
  mDoc.Caption :=msg.get('document');
  mdocProperties.Caption :=msg.get('properties');
  //
  mPage.Caption :=msg.get('page');
  mpageAdd.Caption :=msg.get('addPage');
  mpageRename.Caption :=msg.get('rename');
  mpageRemove.Caption :=msg.get('remove');
  mpageProperies.Caption :=msg.get('properties');
  //
  mDraw.Caption :=msg.get('draw');
  mdrawPoly.Caption :=msg.get('drawPoly');
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
  tabEditor.Caption :=msg.get('editor');
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
  if tabEditor.Focused then
    cmdInput.SetFocus//pass the focus
  else if PageControl1.Focused then
    cmdInput.SetFocus//pass the focus
  ;
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
  StatusBar.Panels[0].Text := doc.ActivePage.frameEditor.StateAsStr;
end;

procedure TForm1.curDocumentMouseMoveVirt(Shift: TShiftState;
  xp, yp: integer; xv, yv, zv: single);
begin
  StatusBar.Panels[3].Text :=
    'x=' + formatfloat('0.00', xv) + ' ' + 'y=' + formatfloat('0.00', yv) +
    ' ' + 'z=' + formatfloat('0.00', zv);
end;


procedure TForm1.showActivePage;
begin
  if doc = nil then
    exit;
  doc.ActivePage.frameEditor.Parent := tabEditor;   //Place it here
  doc.ActivePage.frameEditor.Left := Random(200);
  doc.ActivePage.frameEditor.Top := Random(200);
  doc.ActivePage.frameEditor.Align := alClient;
  doc.ActivePage.frameEditor.Visible := True;  //it makes it visible
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
    doc.ActivePage.frameEditor.ExecuteCommand(strCmd);
  end
  else if key = VK_ESCAPE then
    doc.ActivePage.frameEditor.ExecuteCommand('CANCEL')//Convert key to command
  ;
end;


///////////////////////////// Actions ///////////////////////////////




procedure TForm1.acVerViewSupExecute(Sender: TObject);
begin
  if doc = nil then
    exit;
  doc.ActivePage.frameEditor.Alfa := 0;
  doc.ActivePage.frameEditor.Fi := 0;
  doc.ActivePage.frameEditor.Editor.Refresh;
end;

procedure TForm1.btnNewReservedDocClick(Sender: TObject);
begin
  hDoc.doNewReserved(self);
end;

procedure TForm1.btnPolyClick(Sender: TObject);
begin
  hdraw.doPolyLine(self);
end;

procedure TForm1.cmdMessagesChange(Sender: TObject);
begin

end;

procedure TForm1.cmdRunBtnClick(Sender: TObject);
begin

end;

procedure TForm1.acToolbarPointExecute(Sender: TObject);
begin

end;

procedure TForm1.acToolbarConfigExecute(Sender: TObject);
begin

end;

procedure TForm1.mdocAddPageClick(Sender: TObject);
begin
  hpage.doAdd(self);
end;

procedure TForm1.mdocInsertPolyClick(Sender: TObject);
begin

end;

procedure TForm1.mdocPropertiesClick(Sender: TObject);
begin
  hdoc.doProps(self);
end;

procedure TForm1.mdrawPolyClick(Sender: TObject);
begin
  hdraw.doPolyLine(self);
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

procedure TForm1.mfileNewReservedClick(Sender: TObject);
begin
  hdoc.doNewReserved(self);
end;

procedure TForm1.mpageAddClick(Sender: TObject);
begin
  hpage.doAdd(self);
end;



procedure TForm1.mpageRemoveClick(Sender: TObject);
begin
  hpage.doRemove(self);
end;

procedure TForm1.mviewPropsClick(Sender: TObject);
begin
  hview.doView(self);
end;

procedure TForm1.pdocAddPageClick(Sender: TObject);
begin
  hpage.doAdd(self);
end;

procedure TForm1.ppageRemoveClick(Sender: TObject);
begin
  hpage.doRemove(self);
end;

procedure TForm1.pviewPropertiesClick(Sender: TObject);
begin
  hview.doView(self);
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
