
unit uFrameDocumetExplorer;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, StdCtrls, LCLType, ActnList,
  sketchDocument, uFrameEditor, sketchCore,
  glob
  ;

type
  TEvClickRightDoc = procedure(Doc: TDocument) of object;
  TEvClickRightPage = procedure(Page: TDocPage) of object;
  TEvClickRightObj = procedure(obj: TDrawObjetcs_list) of object;
  TEvClickRightView = procedure(View: TframeEditor) of object;

  { TFrameDocumentExplorer }

  TFrameDocumentExplorer = class(TFrame)
    treeNav: TTreeView;
    docexplImages16: TImageList;
    docexplImages32: TImageList;
    lblTreeNav: TLabel;
    procedure treeNavKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  private
    curDocument: TPtrDocument;
    function NodeIsObject(node: TTreeNode): boolean;
    function NodeIsView(node: TTreeNode): boolean;
    function NodeObjectSelected: TTreeNode;
    function NodeGetSelected: TTreeNode;
    function NodeName(node: TTreeNode): string;
    function NodeSelectedGetName: string;
    procedure NodeSelectByName(argName: string);

    function NodeIsPage(node: TTreeNode): boolean;
    function NodeDocumentSelected: TTreeNode;
    function NodePageSelected: TTreeNode;
    function NodeViewSelected: TTreeNode;

    procedure treeNavMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure treeNavSelectionChanged(Sender: TObject);
  public
    OnClickRightDocument: TEvClickRightDoc;
    OnClickRightPage: TEvClickRightPage;
    OnClickRightView: TEvClickRightView;
    OnClickRightObject: TEvClickRightObj;
    OnDeletePage: TNotifyEvent;
    procedure Refresh;
  public
    procedure Initiate(argDoc: TPtrDocument);
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}
const
  IMIND_DOC = 0;
  IMIND_PAGE = 1;
  IMIND_VIEW = 2;
  IMIND_GRAOBJ = 3;

{ TFrameDocumentExplorer }
procedure TFrameDocumentExplorer.treeNavKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then
    if OnDeletePage <> nil then
      OnDeletePage(self);
end;

function TFrameDocumentExplorer.NodeGetSelected: TTreeNode;
  {Returns the currently selected node. }
begin
  if curDocument^ = nil then
    exit(nil);
  Result := treeNav.Selected;
end;

function TFrameDocumentExplorer.NodeName(node: TTreeNode): string;
  {Returns the name of a node. }
begin
  Result := node.Text;
end;

function TFrameDocumentExplorer.NodeSelectedGetName: string;
  {Returns the name of the selected node.}
begin
  if treeNav.Selected = nil then
    exit('');
  Result := NodeName(treeNav.Selected);
end;

procedure TFrameDocumentExplorer.NodeSelectByName(argName: string);
{Select a node in the tree, using its name. It will work if the tree has or does not focus.}
var
  node: TTreeNode;
begin
  for node in treeNav.Items do
    if (NodeName(node) = argName) then
    begin
      node.Selected := True;
      exit;
    end;
end;
//Identification of selected nodes
function TFrameDocumentExplorer.NodeIsPage(node: TTreeNode): boolean;
begin
  if node = nil then
    exit(False);
  Result := (node.Level = 1) and (node.ImageIndex = IMIND_PAGE);
end;

function TFrameDocumentExplorer.NodeIsView(node: TTreeNode): boolean;
begin
  if node = nil then
    exit(False);
  Result := (node.Level = 2) and (node.ImageIndex = IMIND_VIEW);
end;

function TFrameDocumentExplorer.NodeIsObject(node: TTreeNode): boolean;
begin
  if node = nil then
    exit(False);
  Result := (node.Level = 2) and (node.ImageIndex = IMIND_GRAOBJ);
end;

function TFrameDocumentExplorer.NodeDocumentSelected: TTreeNode;
begin
  if NodeGetSelected = nil then
    exit(nil);
  if NodeGetSelected.Level = 0 then
    exit(NodeGetSelected);
  exit(nil);
end;

function TFrameDocumentExplorer.NodePageSelected: TTreeNode;
begin
  if NodeGetSelected = nil then
    exit(nil);
  if NodeGetSelected.Visible = False then
    exit(nil);
  if NodeIsPage(NodeGetSelected) then
    exit(NodeGetSelected);
  exit(nil);
end;

function TFrameDocumentExplorer.NodeObjectSelected: TTreeNode;
begin
  if NodeGetSelected = nil then
    exit(nil);
  if NodeGetSelected.Visible = False then
    exit(nil);
  if NodeIsPage(NodeGetSelected) then
    exit(NodeGetSelected);
  exit(nil);
end;

function TFrameDocumentExplorer.NodeViewSelected: TTreeNode;
begin
  if NodeGetSelected = nil then
    exit(nil);
  if NodeGetSelected.Visible = False then
    exit(nil);
  if NodeIsView(NodeGetSelected) then
    exit(NodeGetSelected);
  exit(nil);
end;
//Tree events
procedure TFrameDocumentExplorer.treeNavMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  node: TTreeNode;
  Page: TDocPage;
begin
  node := treeNav.GetNodeAt(x, y);
  if node = nil then
    treeNav.Selected := nil;
  //Generate events according to the selected node
  if Button = mbRight then
  begin
    if treeNav.GetNodeAt(X, Y) <> nil then
      treeNav.GetNodeAt(X, Y).Selected := True;
    if NodeDocumentSelected <> nil then
    begin //selected document
      if OnClickRightDocument <> nil then
        OnClickRightDocument(curDocument^);
    end
    else if NodePageSelected <> nil then
    begin  //selected page
      Page := curDocument^.PageByName(NodeSelectedGetName);
      if OnClickRightPage <> nil then
        OnClickRightPage(Page);
    end
    else if NodeViewSelected <> nil then
    begin
      Page := curDocument^.PageByName(NodeGetSelected.Parent.Text);
      if OnClickRightView <> nil then
        OnClickRightView(Page.View);
    end
    else if NodeObjectSelected <> nil then
    begin
      Page := curDocument^.PageByName(NodeGetSelected.Parent.Text);
      if OnClickRightObject <> nil then
        OnClickRightObject(Page.docPageObjList);
    end;
  end;
end;

procedure TFrameDocumentExplorer.treeNavSelectionChanged(Sender: TObject);
{The selected item has changed. Take the corresponding actions.}
begin
  if NodePageSelected <> nil then
    curDocument^.SetActivePageByName(NodePageSelected.Text) ;
    //A page has been selected, the active page changes

end;

procedure TFrameDocumentExplorer.Refresh;
{Refreshes the appearance of the frame, according to the current document.}
var
  Page: TDocPage;
  nodePage: TTreeNode;
  nodeDoc: TTreeNode;
  nodeGeom, nodeView, nodeGraphicObj: TTreeNode;
  argGraphicObject: TGraphicObj;
begin
  //show your title
  lblTreeNav.Caption := self.Caption;

  //  ns := NodeSelectedGetName;  //guarda elemento ObjSelected
  treeNav.Items.Clear;  //clean items
  if curDocument^ = nil then
  begin
    //There is no current document
    nodeDoc := treeNav.items.AddChild(nil, msg.get('noDocuments'));
    exit;
  end;
  //There is an open document
  //Add document node
  nodeDoc := treeNav.items.AddChild(nil, curDocument^.Name);  //add current document
  nodeDoc.ImageIndex := IMIND_DOC;
  nodeDoc.SelectedIndex := IMIND_DOC;
  //Add page nodes
  treeNav.BeginUpdate;
  for Page in curDocument^.pages do
  begin
    nodePage := treeNav.Items.AddChild(nodeDoc, Page.Name);
    nodePage.ImageIndex := IMIND_PAGE;
    nodePage.SelectedIndex := IMIND_PAGE;
    //Add graphic objects fields
    nodeGeom := treeNav.items.AddChild(nodePage, msg.get('documentObjects'));
    nodeGeom.ImageIndex := IMIND_GRAOBJ;
    nodeGeom.SelectedIndex := IMIND_GRAOBJ;

    for argGraphicObject in Page.View.objects do
      nodeGraphicObj := treeNav.items.AddChild(nodeGeom, msg.get('defaultObjName'));
    //Add field View
    nodeView := treeNav.items.AddChild(nodePage, 'View');
    nodeView.ImageIndex := IMIND_VIEW;
    nodeView.SelectedIndex := IMIND_VIEW;

    nodePage.Expanded := True;
  end;
  treeNav.EndUpdate;
  nodeDoc.Expanded := True;
  //  NodeSelectByName(ns);  //restore selection
  NodeSelectByName(curDocument^.ActivePage.Name);
end;

procedure TFrameDocumentExplorer.Initiate(argDoc: TPtrDocument);
{Start the frame, passing the address of the current document that is being managed.
At the moment, it is assumed that he can only manage zero or one document.
Note that you are given the address of the referendum, as it may change.}
begin
  curDocument := argDoc; 
  Refresh;
end;

constructor TFrameDocumentExplorer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  treeNav.OnMouseUp := @treeNavMouseUp;
  treeNav.OnSelectionChanged := @treeNavSelectionChanged;
end;

destructor TFrameDocumentExplorer.Destroy;
begin
  inherited Destroy;
end;


end.
