unit sketchDocument;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, MisUtils, Graphics,
  guiFramePaintBox, sketchCore, sketchDxf, sketchEditor;
type
  Tunidades = (
    tmuMetros,
    tmuPies
  );

  //Type of graphic object. The Autocad standard is followed
  TCadTipObjGraf = (
    etyLine
   ,etyCircle
   ,etyPolyline
   ,etyInsert    //block
  );

  TCadObjGraf = class
    nombre: string;
    tipTab: TCadTipObjGraf;
  end;
  TCadObjetos_list = specialize TFPGObjectList<TCadObjGraf>;

  TEveCambiaPerspec = procedure(View: TfraPaintBox) of object;
  TProject = class;

  { TDocPage }
  TDocPage = class
  private
    procedure vistaCambiaPerspec;
    procedure vistaChangeState(ViewState: TViewState);
    procedure vistaMouseMoveVirt(Shift: TShiftState; xp, yp: Integer; xv,
      yv, zv: Single);
  public
    name     : string;
    parent      : TProject;      //Referencia al objeto padre.
    objetosGraf: TCadObjetos_list;  //Lista de elementos gráficos
    objetos : TlistObjGraf; //Lista de objetos
  public  //Manejo de las vistas
    View: TfraPaintBox;   //una sola View por el momento
    OnChangePersp: TEveCambiaPerspec;  //Cambia x_des,y_des,x_cam,y_cam,alfa,fi o zoom
    OnMouseMoveVirt: TEveMouseVisGraf;
    OnChangeState: TEvChangeState;
  public  //Inicialización
    constructor Create;
    destructor Destroy; override;
  end;
  TCadPagina_list = specialize TFPGObjectList<TDocPage>;

{ TProject }
  TProject = class
  private
    FActivePage: TDocPage;
    fModific  : boolean;   //indica si ha sido modificado
    procedure pag_ChangeState(ViewState: TViewState);
    procedure pag_CambiaPerspec(View: TfraPaintBox);
    procedure pag_MouseMoveVirt(Shift: TShiftState; xp, yp: Integer; xv,
      yv, zv: Single);
    procedure SetActivePage(AValue: TDocPage);
    procedure SetModific(AValue: boolean);
  public
    name : string;
    createdBy: string;
    notes    : string;
    unidades : Tunidades;
    OnModify : procedure of object; //Proyecto modificado
    OnChangePersp: TEveCambiaPerspec;  //Cambia x_des, y_des, x_cam, alfa, ...
    OnMouseMoveVirt: TEveMouseVisGraf;
    OnChangeState: TEvChangeState;
    property Modified: boolean read fModific write SetModific;
    procedure SaveFile;
  public  //Campos de página
    paginas: TCadPagina_list; {Lista de páginas. Debe contener al menos una.}
    OnChangeActivePage: procedure of object;
    Property ActivePage: TDocPage read FActivePage write SetActivePage;
    function IndexOfPage(Page: TDocPage): integer;
    function PrevPage(Page: TDocPage): TDocPage;
    function NextPage(Page: TDocPage): TDocPage;
    function PageByName(pagName: string): TDocPage;
    procedure SetActivePageByName(pagName: string);
    function AddPage: TDocPage;
    procedure RemovePage(pagName: TDocPage);
    procedure RemovePage(argName: string);
    procedure HideAllPages;
  public  //Iniicialización
    constructor Create;
    destructor Destroy; override;
  end;

  TCadProyectoPtr = ^TProject;

implementation

procedure TDocPage.vistaCambiaPerspec;
begin
  if OnChangePersp<>nil then OnChangePersp(self.View);   //identifica a la página
end;
procedure TDocPage.vistaChangeState(ViewState: TViewState);
begin
  if OnChangeState<>nil then OnChangeState(ViewState);
end;
procedure TDocPage.vistaMouseMoveVirt(Shift: TShiftState; xp, yp: Integer;
  xv, yv, zv: Single);
begin
  if OnMouseMoveVirt<>nil then OnMouseMoveVirt(Shift, xp, yp, xv, yv, 0);
end;
constructor TDocPage.Create;
begin
  objetosGraf := TCadObjetos_list.Create(true);
  objetos := TlistObjGraf.Create(true);   //contenedor
  View:= TfraPaintBox.Create(nil, objetos);  //crea una View

//  View.Parent := TabSheet1;
//  View.Visible:=true;
//  View.Align:=alClient;
  View.viewEdi.v2d.backColor:=clBlack;
  View.viewEdi.ShowAxes:=true;
  View.viewEdi.ShowRotPoint:=true;
  View.viewEdi.ShowGrid:=true;
//  View.VisEdiGraf.OnChangeView:=@fraMotEdicionmotEdiChangeView;
  View.OnChangePersp:=@vistaCambiaPerspec;
  View.OnMouseMoveVirt:=@vistaMouseMoveVirt;
  View.OnChangeState:=@vistaChangeState;


end;
destructor TDocPage.Destroy;
begin
  View.Destroy;
  objetos.Destroy;
  objetosGraf.Destroy;
  inherited Destroy;
end;
{ TProject }
procedure TProject.SetModific(AValue: boolean);
begin
  if fModific=AValue then Exit;
  fModific:=AValue;
  if FModific then begin
    if OnModify<>nil then OnModify;  //evento
  end;
end;
procedure TProject.pag_CambiaPerspec(View: TfraPaintBox);
{Se genera si alguna página cambia su perspectiva}
begin
  if OnChangePersp<>nil then OnChangePersp(View);
end;
procedure TProject.pag_ChangeState(ViewState: TViewState);
begin
  if OnChangeState<>nil then OnChangeState(ViewState);
end;
procedure TProject.pag_MouseMoveVirt(Shift: TShiftState; xp, yp: Integer;
  xv, yv, zv: Single);
begin
  if OnMouseMoveVirt<>nil then OnMouseMoveVirt(Shift, xp, yp, xv, yv, zv);
end;
procedure TProject.SaveFile;
begin

end;
//Campos de página
procedure TProject.SetActivePage(AValue: TDocPage);
var
  Page: TDocPage;
begin
  if FActivePage=AValue then Exit;
  //Verifica si la página solicitada, existe
  for Page in paginas do begin
    if Page = AValue then begin
      //Existe
      FActivePage:=AValue;
//      Modified := true;  //Cambiar de página es un cambio
      if OnChangeActivePage<>nil then OnChangeActivePage;
      exit;
    end;
  end;
  //No existe
end;
function TProject.IndexOfPage(Page: TDocPage): integer;
{Devuelve el índice de una página dentro de la lista de páginas. Si no ubica a la página
 devuelve -1.}
var
  i: integer;
begin
  for i:=0 to paginas.Count-1 do begin
    if paginas[i] = Page then exit(i);
  end;
  //No encontró
  exit(-1);
end;
function TProject.PrevPage(Page: TDocPage): TDocPage;
{Devuelve la página anterior a una indicada. Si es la primera, devuelve la misma
 página. Si hay error, devuelve NIL.}
var
  i: integer;
begin
  i := IndexOfPage(Page);
  if i=-1 then exit(niL);
  if i=0 then begin  //no hay anterior, devuelve la misma
    exit(Page);
  end else begin
    exit(paginas[i-1]); //devuelve anterior
  end;
end;
function TProject.NextPage(Page: TDocPage): TDocPage;
{Devuelve la página siguiente a una indicada. Si es la última, devuelve la misma
 página. Si hay error, devuelve NIL.}
var
  i: Integer;
begin
  i := IndexOfPage(Page);
  if i=-1 then exit(niL);
  if i=paginas.Count-1 then begin  //no hay siguiente, devuelve la misma
    exit(Page);
  end else begin
    exit(paginas[i+1]); //devuelve siguiente
  end;
end;
function TProject.PageByName(pagName: string): TDocPage;
{Devuelve la referencia a una página, dado su nombre. Si no encuentra la página,
devuelve NIL.}
var
  Page: TDocPage;
begin
  for Page in paginas do begin
    if Page.name = pagName then exit(Page);
  end;
  exit(nil);
end;
procedure TProject.SetActivePageByName(pagName: string);
var
  Page: TDocPage;
begin
  for Page in paginas do begin
    if Page.name = pagName then begin
      ActivePage := Page;
      exit;
    end;
  end;
  //No existe
end;
function TProject.AddPage: TDocPage;
{Agrega una página al proyecto. Devuelve la referecnia a la pa´gina creada.}
var
  Page: TDocPage;
begin
  Page := TDocPage.Create;
  Page.name:='Página'+IntToStr(paginas.Count+1);
  Page.parent := self;
  Page.OnChangePersp:=@pag_CambiaPerspec;
  Page.OnMouseMoveVirt:=@pag_MouseMoveVirt;
  Page.OnChangeState:=@pag_ChangeState;
  paginas.Add(Page);
  Modified:=true;   //es un cambio
  Result := Page;
end;
procedure TProject.RemovePage(pagName: TDocPage);
{Elimina la página indicada.}
begin
  if paginas.Count=1 then begin
    MsgExc('No se pueden eliminar todas las páginas.');
    exit;
  end;
  if ActivePage = pagName then begin
    //Se está borrando la página activa, hay que moverla
    if pagName = paginas.First then   //es la primera
      ActivePage := NextPage(pagName)  //pasa a la siguiente
    else
      ActivePage := PrevPage(pagName);  //pasa a la anetrior
  end;
  paginas.Remove(pagName);
  Modified:=true;   //es un cambio
end;
procedure TProject.RemovePage(argName: string);
var
  Page: TDocPage;
begin
  for Page in paginas do begin
    if Page.name = argName then begin
      RemovePage(Page);
      exit;
    end;
  end;
  //No encontró
  MsgExc('No existe la página: "%s"', [name]);
end;
procedure TProject.HideAllPages;
{Pone las vistas de todas las páginas en visible := FALSE, de modo que no se mostrarán
en el control asignado.}
var
  Page: TDocPage;
begin
  for Page in paginas do begin
    Page.View.Visible:=false;
  end;
end;

//Iniicialización
constructor TProject.Create;
var
  Page: TDocPage;
begin
  paginas:= TCadPagina_list.Create(true);
  //Crea una página
  Page := AddPage;
  ActivePage := Page;   //la pone como activa por defecto
end;
destructor TProject.Destroy;
begin
  paginas.Destroy;
  inherited Destroy;
end;

end.

