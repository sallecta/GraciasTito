unit uDoc;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, fgl, Graphics, Dialogs,
  //local
  //uFrameEditor,
  uDocCore, uDocEditor;


  { TDoc }
  type TDoc = class
  { TDoc types }
    //{ TDocPageEntity }
    //type TDocPageEntity = class
    //    Name: string;
    //    dxfEntity: ( //Type of graphic object according to the ODA standard
    //      dxfLine,
    //      dxfCircle,
    //      dxfPolyline,
    //      dxfBlock
    //      );
    //end;//TDocPageEntity
    { TDocPageEntities }
    //type  TDocPageEntities = specialize TFPGObjectList<TDocPageEntity>;
    //end  TDocPageEntities
    { TPage }
    type TPage = class
      public
        Name: string;
        parent: TDoc;      //Reference to the parent object.
        //docPageEntities: TDocPageEntities;  //List of graphic elements
        DocCoreObjects: TDocCoreObjects; //List of DocCoreObjects
      public
        //frameEditor: TframeEditor;
      public
        constructor Create;
        destructor Destroy; override;
    end;//TPage
    { TDocPages }
    type TDocPages = specialize TFPGObjectList<TPage>;
    //end TDocPages
    { TDoc vars and procs}
    private
      FActivePage: TPage;
      procedure SetActivePage(argPage: TPage);
    public
      Name: string;
      createdBy: string;
      notes: string;
    public  //Page fields
      docPages: TDocPages; {List of docPages. It must contain at least one.}
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
  end;//TDoc

  type TPtrDocument = ^TDoc;

implementation
uses glob;

constructor TDoc.TPage.Create;
begin
  DocCoreObjects := TDocCoreObjects.Create(True);   //container
end;

destructor TDoc.TPage.Destroy;
begin
  //frameEditor.Destroy;
  DocCoreObjects.Destroy;
  //docPageEntities.Destroy;
  inherited Destroy;
end;

{ TDoc }
//Page fields
procedure TDoc.SetActivePage(argPage: TPage);
var
  Page: TPage;
begin
  if FActivePage = argPage then
    Exit;
  //Verify if the page requested, exists
  for Page in docPages do
    if Page = argPage then
    begin
      FActivePage := argPage;
      exit;
    end;
end;

function TDoc.IndexOfPage(Page: TPage): integer;
var
  i: integer;
begin
  for i := 0 to docPages.Count - 1 do
    if docPages[i] = Page then
      exit(i);
  //Did not find
  exit(-1);
end;

function TDoc.PrevPage(Page: TPage): TPage;
var
  i: integer;
begin
  i := IndexOfPage(Page);
  if i = -1 then
    exit(nil);
  if i = 0 then
    exit(Page)//there is no previous, return the same
  else
    exit(docPages[i - 1]);//return previous
end;

function TDoc.NextPage(Page: TPage): TPage;
var
  i: integer;
begin
  i := IndexOfPage(Page);
  if i = -1 then
    exit(nil);
  if i = docPages.Count - 1 then
    exit(Page)//there is no next page, return the same
  else
    exit(docPages[i + 1]);//return next page
end;

function TDoc.PageByName(pageName: string): TPage;
var
  Page: TPage;
begin
  for Page in docPages do
    if Page.Name = pageName then
      exit(Page);
  exit(nil);//can not find the page
end;

procedure TDoc.SetActivePageByName(pageName: string);
var
  Page: TPage;
begin
  for Page in docPages do
    if Page.Name = pageName then
    begin
      ActivePage := Page;
      exit;
    end;
end;

function TDoc.AddPage: TPage;
var
  Page: TPage;
begin
  Page := TPage.Create;
  Page.parent := self;
  docPages.Add(Page);
  Result := Page;
end;

procedure TDoc.RemovePage(pageName: TPage);
begin
  if docPages.Count = 1 then
  begin
    ShowMessage(msg.Get('CantDeleteLastPage'));
    exit;
  end;
  if ActivePage = pageName then
  begin
  //The active page is being deleted, it must be moved
  //Change active page to the previous one
    if pageName = docPages.First then//it's the first
      ActivePage := NextPage(pageName)  //go to the next
    else
      ActivePage := PrevPage(pageName);
  end;
  docPages.Remove(pageName);
end;

procedure TDoc.RemovePage(argName: string);
var
  Page: TPage;
begin
  for Page in docPages do
    if Page.Name = argName then
    begin
      RemovePage(Page);
      exit;
    end;
  //Did not find
  ShowMessage(msg.Get('PageDoesNotExist')+ Name);
end;


constructor TDoc.Create;
var
  Page: TPage;
begin
  docPages := TDocPages.Create(True);//init object list
  //Create a page
  Page := AddPage;
  ActivePage := Page;//puts it as active by default
end;

destructor TDoc.Destroy;
begin
  docPages.Destroy;
  inherited Destroy;
end;

end.
