{Unidad con formulario de configuración para manejar las propiedades de
 una aplicación. Está pensado para usarse con frames de la clase Tframe,
 definida en la unidad "PropertyFrame".
 }
unit guiFormConfig;
{$mode objfpc}{$H+}
interface
uses
  SysUtils, Forms, Graphics, SynEdit, Buttons, ComCtrls, StdCtrls,
  MisUtils, SynFacilCompletion,
  guiFrameCfgGeneral, guiFrameCfgView, guiFrameCfgPanCom, guiFrameCfgSynEdit,
  MiConfigXML, MiConfigUtils, Classes;

type
  {TEvChangeProp}TEvChangeProp = procedure of object;  //event to indicate that there is change

  { TConfig }

  TConfig = class(TForm)
  btnAccept: TBitBtn;
  btnApply: TBitBtn;
   btnCancel: TBitBtn;
    TreeView1: TTreeView;
    procedure btnAcceptClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TreeView1SelectionChanged(Sender: TObject);
  private
    procedure cfgFilePropertiesChanges;
    procedure  Win2Props;
    procedure  Props2Win;
  public
    msgError: string;
    edTerm  : TSynEdit;    //reference to the SynEdit editor
    edPCom  : TSynEdit;    //reference to the editor command panel
    edMacr  : TSynEdit;    //reference to the editor command panel
    edRemo  : TSynEdit;    //reference to the remote editor
    //Configuration Frames
    fcGeneral : TfraCfgGeneral;
    fcView   : TfraCfgView;
    fcPanCom  : TfraCfgPanCom;   //Command panel
    fcPanComEd: TfraCfgSynEdit;  //Command panel editor
    //Events
    OnPropertiesChanged: procedure of object;
    procedure Initiate(hl0: TSynFacilComplet);
    procedure IniFileRead;
    procedure IniFileWrite;
    procedure doConfig(Id: string='');
    function ContiainsPrompt(const strAct: string): integer;
    procedure SetLanguage(lang: string);
  end;

var
  Config: TConfig;

implementation
{$R *.lfm}

  { TConfig }

procedure TConfig.FormCreate(Sender: TObject);
begin
  fcGeneral := TfraCfgGeneral.Create(self);
  fcGeneral.Name:= 'General';  //Necessary for it to generate its label in the XML
  fcGeneral.Parent := self;

  fcView := TfraCfgView.Create(self);
  fcView.Name:='View';
  fcView.Parent := self;

  fcPanCom := TfraCfgPanCom.Create(self);
  fcPanCom.Name:= 'PanComGen';
  fcPanCom.Parent := self;

  fcPanComEd := TfraCfgSynEdit.Create(self);
  fcPanComEd.Name := 'EdTer';  //Necessary for it to generate its label in the XML
  fcPanComEd.parent := self;

  TreeView1.Items.Clear;  //Clean tree
  LinkFrameToTreeView(TreeView1, '1',   dic('General'), fcGeneral);
  LinkFrameToTreeView(TreeView1, '1.1', dic('View'), fcView);
  LinkFrameToTreeView(TreeView1, '2',   dic('Panel de Comandos'), fcPanCom);
  LinkFrameToTreeView(TreeView1, '2.1', dic('General'), fcPanCom);
  LinkFrameToTreeView(TreeView1, '2.2', dic('Editor') , fcPanComEd);

  //select first option
  TreeView1.Items[0].Selected:=true;
  TreeView1SelectionChanged(self);
  cfgFile.OnPropertiesChanges:=@cfgFilePropertiesChanges;
end;
procedure TConfig.FormDestroy(Sender: TObject);
begin
//  Free_AllConfigFrames(self);  //Libera los frames de configuración
end;
procedure TConfig.FormShow(Sender: TObject);
begin
  Props2Win;   //load the properties in the frame
end;
procedure TConfig.cfgFilePropertiesChanges;
begin
  if OnPropertiesChanged<>nil then OnPropertiesChanged;
  fcPanComEd.ConfigEditor;  //to update your editor
end;
procedure TConfig.Initiate(hl0: TSynFacilComplet);
//Start the configuration form. It must be called before using the form
// and after having loaded all the frames.
begin
  //start Frames
  fcGeneral.Initiate(cfgFile);
  fcView.Initiate(cfgFile);
  fcPanCom.Initiate(cfgFile);
  fcPanComEd.Initiate(cfgFile, edTerm, clBlack);
  //reads parameters from the configuration file.
  IniFileRead;
end;
procedure TConfig.TreeView1SelectionChanged(Sender: TObject);
begin
  if TreeView1.Selected = nil then exit;
  ShowFrameOfNode(self, Treeview1.Selected, 145, 0);
end;

procedure TConfig.btnAcceptClick(Sender: TObject);
begin
  btnApplyClick(Self);
  if cfgFile.strErr<>'' then exit;  //there was an error
  self.Close;   //because it's modal
end;
procedure TConfig.btnApplyClick(Sender: TObject);
begin
  Win2Props;       //Write properties of the frames
  if cfgFile.strErr<>'' then begin
    MsgErr(cfgFile.strErr);
    exit;
  end;
  IniFileWrite;   //save properties on disk
  if edTerm<>nil then edTerm.Invalidate;//to refresh the changes
  if edPCom<>nil then edPCom.Invalidate; //to refresh the changes
end;
procedure TConfig.btnCancelClick(Sender: TObject);
begin
  self.Hide;
end;
procedure TConfig.doConfig(Id: string='');
//Show the form, so that you can configure the current session
var
  it: TTreeNode;
begin
  if Id<> '' then begin  { it asks to show a special Id
     hide others }
    it := TTreeNodeFromId(Id,TreeView1);
    if it <> nil then it.Selected:=true;
    TreeView1SelectionChanged(self);
  end else begin //show all
    for it in TreeView1.Items do begin
      it.Visible:=true;
    end;
  end;
  Showmodal;
end;

function TConfig.ContiainsPrompt(const strAct: string): integer;
{Check if a string contains the prompt. Verification is simple, compare only the beginning of the chain.
If the string contains the prompt, it returns the length of the prompt found, otherwise returns zero
It is used for syntax highlighting and screen handling.}
var
  l: Integer;
  p: SizeInt;
begin
   l := length(fcPanCom.Prompt);{ The prompt is defined by the configuration }
   if l=0 then exit(0);
   if copy(strAct,1,l) = fcPanCom.Prompt then begin
     Result := length(fcPanCom.Prompt);  //the size of the prompt
   end else begin
     Result := 0;
   end;
end;

procedure TConfig.Win2Props;
//Read the properties of the configuration window.
begin
  cfgFile.WindowToProperties;   // can generate error
end;
procedure TConfig.Props2Win;
//Displays the properties in the configuration window.
begin
  cfgFile.PropertiesToWindow;  // can generate error
end;
procedure TConfig.IniFileRead;
begin
  cfgFile.FileToProperties;
end;

procedure TConfig.IniFileWrite;
//Write the configuration file
begin
  cfgFile.PropertiesToFile;
end;

procedure TConfig.SetLanguage(lang: string);
//Translation routine
begin
  fcGeneral.SetLanguage(lang);
  fcView.SetLanguage(lang);
  fcPanCom.SetLanguage(lang);
  fcPanComEd.SetLanguage(lang);

  case lowerCase(lang) of
  'es': begin
//      TTreeNodeFromId('1',TreeView1).Text:='Conexión';
//      TTreeNodeFromId('1.1',TreeView1).Text:='General';
//      TTreeNodeFromId('1.2',TreeView1).Text:='Detec.de Prompt';
//      TTreeNodeFromId('1.3',TreeView1).Text:='Rutas/Archivos';
//      TTreeNodeFromId('2',TreeView1).Text:='Terminal';
//      TTreeNodeFromId('2.1',TreeView1).Text:='Pantalla';
//      TTreeNodeFromId('2.2',TreeView1).Text:='Editor';
//      TTreeNodeFromId('2.3',TreeView1).Text:='Comando Recurrente';
    end;
  'en': begin
//      TTreeNodeFromId('1',TreeView1).Text:='Connection';
//      TTreeNodeFromId('1.1',TreeView1).Text:='General';
//      TTreeNodeFromId('1.2',TreeView1).Text:='Prompt detection';
//      TTreeNodeFromId('1.3',TreeView1).Text:='Paths/Files';
//      TTreeNodeFromId('2',TreeView1).Text:='Terminal';
//      TTreeNodeFromId('2.1',TreeView1).Text:='Screen';
//      TTreeNodeFromId('2.2',TreeView1).Text:='Editor';
//      TTreeNodeFromId('2.3',TreeView1).Text:='Recurring command';
    end;
  end;
end;

end.

