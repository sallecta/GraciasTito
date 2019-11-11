unit sketchDocument;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, fgl, Graphics,
  uFramePaintBox, sketchCore, sketchDxf, sketchEditor, Dialogs,
  glob
  ;

type
  TMeasureUnits = (
    muMeters,
    muFeet
    );

  //Type of graphic object. The ODA standard is followed
  TDrawObjKind = (
    dxfLine
    , dxfCircle
    , dxfPolyline
    , dxfBlock    //block
    );

  TDrawObj = class
    Name: string;
    kinds: TDrawObjKind;
  end;

  TDrawObjetcs_list = specialize TFPGObjectList<TDrawObj>;

  TEvChangePersp = procedure(View: TFramePaintBox) of object;
  TProject = class;

  { TDocPage }
  TDocPage = class
  private
    procedure ViewChangePersp;
    procedure ViewChangeState(ViewState: TViewState);
    procedure ViewMouseMoveVirt(Shift: TShiftState; xp, yp: integer;
      xv, yv, zv: single);
  public
    Name: string;
    parent: TProject;      //Reference to the parent object.
    docPageObjList: TDrawObjetcs_list;  //List of graphic elements
    objects: TEditorObjList; //List of objects
  public  //Managing views
    View: TFramePaintBox;   //One View at the time
    OnChangePersp: TEvChangePersp;
    //Change x_offs, y_offs, x_cam, y_cam, alpha, fi or zoom
    OnMouseMoveVirt: TEvMousePaintBox;
    OnChangeState: TEvChangeState;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TDrawPage_list = specialize TFPGObjectList<TDocPage>;

  { TProject }
  TProject = class
  private
    FActivePage: TDocPage;
    fModified: boolean;   //indicates if it has been modified
    procedure page_ChangeState(ViewState: TViewState);
    procedure page_ChangePerspec(View: TFramePaintBox);
    procedure page_MouseMoveVirt(Shift: TShiftState; xp, yp: integer;
      xv, yv, zv: single);
    procedure SetActivePage(AValue: TDocPage);
    procedure SetModified(AValue: boolean);
  public
    Name: string;
    createdBy: string;
    notes: string;
    MeasureUnits: TMeasureUnits;
    OnModify:
    procedure of object;
    OnChangePersp: TEvChangePersp;  //Change x_offs, y_offs, x_cam, alpha, ...
    OnMouseMoveVirt: TEvMousePaintBox;
    OnChangeState: TEvChangeState;
    property Modified: boolean read fModified write SetModified;
    procedure SaveFile;
  public  //Page fields
    pages: TDrawPage_list; {List of pages. It must contain at least one.}
    OnChangeActivePage:
    procedure of object;
    property ActivePage: TDocPage read FActivePage write SetActivePage;
    function IndexOfPage(Page: TDocPage): integer;
    function PrevPage(Page: TDocPage): TDocPage;
    function NextPage(Page: TDocPage): TDocPage;
    function PageByName(pageName: string): TDocPage;
    procedure SetActivePageByName(pageName: string);
    function AddPage: TDocPage;
    procedure RemovePage(pageName: TDocPage);
    procedure RemovePage(argName: string);
    procedure HideAllPages;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TPtrProject = ^TProject;

implementation

procedure TDocPage.ViewChangePersp;
begin
  if OnChangePersp <> nil then
    OnChangePersp(self.View);
end;

procedure TDocPage.ViewChangeState(ViewState: TViewState);
begin
  if OnChangeState <> nil then
    OnChangeState(ViewState);
end;

procedure TDocPage.ViewMouseMoveVirt(Shift: TShiftState; xp, yp: integer;
  xv, yv, zv: single);
begin
  if OnMouseMoveVirt <> nil then
    OnMouseMoveVirt(Shift, xp, yp, xv, yv, 0);
end;

constructor TDocPage.Create;
begin
  docPageObjList := TDrawObjetcs_list.Create(True);
  objects := TEditorObjList.Create(True);   //container
  View := TFramePaintBox.Create(nil, objects);  //create a View
  View.Editor.VirtScreen.backColor := clBlack;
  View.Editor.ShowAxes := True;
  View.Editor.ShowRotPoint := True;
  View.Editor.ShowGrid := True;
  View.OnChangePersp := @ViewChangePersp;
  View.OnMouseMoveVirt := @ViewMouseMoveVirt;
  View.OnChangeState := @ViewChangeState;
end;

destructor TDocPage.Destroy;
begin
  View.Destroy;
  objects.Destroy;
  docPageObjList.Destroy;
  inherited Destroy;
end;

{ TProject }
procedure TProject.SetModified(AValue: boolean);
begin
  if fModified = AValue then
    Exit;
  fModified := AValue;
  if fModified then
    if OnModify <> nil then
      OnModify;
end;

procedure TProject.page_ChangePerspec(View: TFramePaintBox);
{It is generated if any page changes its perspective}
begin
  if OnChangePersp <> nil then
    OnChangePersp(View);
end;

procedure TProject.page_ChangeState(ViewState: TViewState);
begin
  if OnChangeState <> nil then
    OnChangeState(ViewState);
end;

procedure TProject.page_MouseMoveVirt(Shift: TShiftState; xp, yp: integer;
  xv, yv, zv: single);
begin
  if OnMouseMoveVirt <> nil then
    OnMouseMoveVirt(Shift, xp, yp, xv, yv, zv);
end;

procedure TProject.SaveFile;
begin

end;
//Page fields
procedure TProject.SetActivePage(AValue: TDocPage);
var
  Page: TDocPage;
begin
  if FActivePage = AValue then
    Exit;
  //Verify if the page requested, exists
  for Page in pages do
    if Page = AValue then
    begin
      FActivePage := AValue;
      if OnChangeActivePage <> nil then
        OnChangeActivePage;
      exit;
    end;
end;

function TProject.IndexOfPage(Page: TDocPage): integer;
  {Returns the index of a page within the list of pages. If it does not locate the page, it returns -1.}
var
  i: integer;
begin
  for i := 0 to pages.Count - 1 do
    if pages[i] = Page then
      exit(i);
  //Did not find
  exit(-1);
end;

function TProject.PrevPage(Page: TDocPage): TDocPage;
  {Returns the previous page to a indicated one. If it is the first one, it returns the same page. If there is an error, return NIL.}
var
  i: integer;
begin
  i := IndexOfPage(Page);
  if i = -1 then
    exit(nil);
  if i = 0 then
    exit(Page)//there is no previous, it returns the same
  else
    exit(pages[i - 1])//returns previous
  ;
end;

function TProject.NextPage(Page: TDocPage): TDocPage;
  {Returns the next page to an indicated one. If it is the last one, it returns the same page. If there is an error, return NIL.}
var
  i: integer;
begin
  i := IndexOfPage(Page);
  if i = -1 then
    exit(nil);
  if i = pages.Count - 1 then
    exit(Page)//there is no next, it returns the same
  else
    exit(pages[i + 1])//returns next
  ;
end;

function TProject.PageByName(pageName: string): TDocPage;
  {Returns the reference to a page, given its Name. If you can not find the page, return NIL.}
var
  Page: TDocPage;
begin
  for Page in pages do
    if Page.Name = pageName then
      exit(Page);
  exit(nil);
end;

procedure TProject.SetActivePageByName(pageName: string);
var
  Page: TDocPage;
begin
  for Page in pages do
    if Page.Name = pageName then
    begin
      ActivePage := Page;
      exit;
    end;
end;

function TProject.AddPage: TDocPage;
  {Add a page to the project. Returns the reference to the created page.}
var
  Page: TDocPage;
begin
  Page := TDocPage.Create;
  Page.Name := msg.get('page') + IntToStr(pages.Count + 1);
  Page.parent := self;
  Page.OnChangePersp := @page_ChangePerspec;
  Page.OnMouseMoveVirt := @page_MouseMoveVirt;
  Page.OnChangeState := @page_ChangeState;
  pages.Add(Page);
  Modified := True;
  Result := Page;
end;

procedure TProject.RemovePage(pageName: TDocPage);
{Remove the indicated page.}
begin
  if pages.Count = 1 then
  begin
    ShowMessage(msg.Get('CantDeleteLastPage'));
    exit;
  end;
  if ActivePage = pageName then
    if pageName = pages.First then//it's the first
      ActivePage := NextPage(pageName)  //go to the next
    else
      ActivePage := PrevPage(pageName)
    //The active page is being deleted, it must be moved
    //Go to the previous one
  ;
  pages.Remove(pageName);
  Modified := True;
end;

procedure TProject.RemovePage(argName: string);
var
  Page: TDocPage;
begin
  for Page in pages do
    if Page.Name = argName then
    begin
      RemovePage(Page);
      exit;
    end;
  //Did not find
  ShowMessage(msg.Get('PageDoesNotExist')+ Name);
end;

procedure TProject.HideAllPages;
{It puts the views of all the pages in visible: = FALSE, so they will not be displayed in the assigned control.}
var
  Page: TDocPage;
begin
  for Page in pages do
    Page.View.Visible := False;
end;


constructor TProject.Create;
var
  Page: TDocPage;
begin
  pages := TDrawPage_list.Create(True);
  //Create a page
  Page := AddPage;
  ActivePage := Page;//puts it as active by default
end;

destructor TProject.Destroy;
begin
  pages.Destroy;
  inherited Destroy;
end;

end.
