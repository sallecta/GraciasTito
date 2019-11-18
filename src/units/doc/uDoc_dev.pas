unit uDoc_dev;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, fgl, Graphics, Dialogs;
  //local
  //uFrameEditor, uDocCore, uDocEditor;



  { TDoc_dev }
  type TDoc_dev = class
  { TDoc_dev types }
    { TEntity }
    type TEntity = class
        Name: string;
        dxfEntity: ( //Type of graphic object according to the ODA standard
          dxfLine,
          dxfCircle,
          dxfPolyline,
          dxfBlock
          );
    end;//TEntity
    { TEntities }
    type  TEntities = specialize TFPGObjectList<TEntity>;
    //end  TEntities
    { TPage }
    type TPage = class
      public
        Name: string;
        parent: TDoc_dev;      //Reference to the parent object.
        entities: TEntities;  //List of graphic elements
        DocCoreObjects: TDocCoreObjects; //List of DocCoreObjects
      public
        frameEditor: TframeEditor;
      public
        constructor Create;
        destructor Destroy; override;
    end;//TPage
    { TPages }
    type TPages = specialize TFPGObjectList<TPage>;
    //end TPages
    { TDoc_dev vars and procs}
    private
      FActivePage: TPage;
      procedure SetActivePage(argPage: TPage);
    public
      Name: string;
      createdBy: string;
      notes: string;
    public  //Page fields
      pages: TPages; {List of pages. It must contain at least one.}
      property ActivePage: TPage read FActivePage write SetActivePage;
      function IndexOfPage(Page: TPage): integer;
      function PrevPage(Page: TPage): TPage;
      function NextPage(Page: TPage): TPage;
      function PageByName(pageName: string): TPage;
      procedure SetActivePageByName(pageName: string);
      function AddPage: TPage;
      procedure RemovePage(pageName: TPage);
      procedure RemovePage(argName: string);
    public
      constructor Create;
      destructor Destroy; override;
  end;//TDoc_dev

  type TPtrDocument = ^TDoc_dev;

implementation
uses glob;

constructor TDoc_dev.TPage.Create;
begin
  entities := TEntities.Create(True);
  DocCoreObjects := TDocCoreObjects.Create(True);   //container
  frameEditor := TframeEditor.Create(nil, DocCoreObjects);  //create a frameEditor
  frameEditor.Editor.Canvas.backColor := clBlack;
  frameEditor.Editor.ShowAxes := True;
  frameEditor.Editor.ShowRotPoint := True;
  frameEditor.Editor.ShowGrid := True;
end;

destructor TDoc_dev.TPage.Destroy;
begin
  frameEditor.Destroy;
  DocCoreObjects.Destroy;
  entities.Destroy;
  inherited Destroy;
end;

{ TDoc_dev }
//Page fields
procedure TDoc_dev.SetActivePage(argPage: TPage);
var
  Page: TPage;
begin
  if FActivePage = argPage then
    Exit;
  //Verify if the page requested, exists
  for Page in pages do
    if Page = argPage then
    begin
      FActivePage := argPage;
      exit;
    end;
end;

function TDoc_dev.IndexOfPage(Page: TPage): integer;
var
  i: integer;
begin
  for i := 0 to pages.Count - 1 do
    if pages[i] = Page then
      exit(i);
  //Did not find
  exit(-1);
end;

function TDoc_dev.PrevPage(Page: TPage): TPage;
var
  i: integer;
begin
  i := IndexOfPage(Page);
  if i = -1 then
    exit(nil);
  if i = 0 then
    exit(Page)//there is no previous, return the same
  else
    exit(pages[i - 1]);//return previous
end;

function TDoc_dev.NextPage(Page: TPage): TPage;
var
  i: integer;
begin
  i := IndexOfPage(Page);
  if i = -1 then
    exit(nil);
  if i = pages.Count - 1 then
    exit(Page)//there is no next page, return the same
  else
    exit(pages[i + 1]);//return next page
end;

function TDoc_dev.PageByName(pageName: string): TPage;
var
  Page: TPage;
begin
  for Page in pages do
    if Page.Name = pageName then
      exit(Page);
  exit(nil);//can not find the page
end;

procedure TDoc_dev.SetActivePageByName(pageName: string);
var
  Page: TPage;
begin
  for Page in pages do
    if Page.Name = pageName then
    begin
      ActivePage := Page;
      exit;
    end;
end;

function TDoc_dev.AddPage: TPage;
var
  Page: TPage;
begin
  Page := TPage.Create;
  Page.parent := self;
  pages.Add(Page);
  Result := Page;
end;

procedure TDoc_dev.RemovePage(pageName: TPage);
begin
  if pages.Count = 1 then
  begin
    ShowMessage(msg.Get('CantDeleteLastPage'));
    exit;
  end;
  if ActivePage = pageName then
  begin
  //The active page is being deleted, it must be moved
  //Change active page to the previous one
    if pageName = pages.First then//it's the first
      ActivePage := NextPage(pageName)  //go to the next
    else
      ActivePage := PrevPage(pageName);
  end;
  pages.Remove(pageName);
end;

procedure TDoc_dev.RemovePage(argName: string);
var
  Page: TPage;
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


constructor TDoc_dev.Create;
var
  Page: TPage;
begin
  pages := TPages.Create(True);
  //Create a page
  Page := AddPage;
  ActivePage := Page;//puts it as active by default
end;

destructor TDoc_dev.Destroy;
begin
  pages.Destroy;
  inherited Destroy;
end;

end.
