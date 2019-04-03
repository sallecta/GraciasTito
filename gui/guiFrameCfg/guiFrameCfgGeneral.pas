unit guiFrameCfgGeneral;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, Menus,
  MiConfigBasic;

type
  TStyleToolbar = (stb_SmallIcon, stb_BigIcon);
  { TfraCfgGeneral }
  TfraCfgGeneral = class(TFrame)
    chkVerBarEst: TCheckBox;
    chkVerBarHer: TCheckBox;
    RadioGroup1: TRadioGroup;
  public
    ToolBar: boolean;
    StateToolbar: TStyleToolbar;
    StatusBar: boolean;
    VerInspVar: boolean;
    procedure Initiate(cfgFile: TMiConfigBasic); //Inicia el frame
    procedure SetLanguage(lang: string);
  end;

implementation
{$R *.lfm}

{ TfraCfgGeneral }
procedure TfraCfgGeneral.Initiate(cfgFile: TMiConfigBasic);
begin
  cfgFile.Asoc_Bol(self.Name + '/ToolBar', @ToolBar  ,  chkVerBarHer, true);
  cfgFile.Asoc_Enum(self.Name + '/StateStatusbar',@StateToolbar, SizeOf(TStyleToolbar), RadioGroup1, 1);
  cfgFile.Asoc_Bol(self.Name + '/StatusBar', @StatusBar  ,  chkVerBarEst, true);
end;

procedure TfraCfgGeneral.SetLanguage(lang: string);
begin

end;

end.

