unit guiFrameCfgPanCom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, MiConfigBasic;

type

  { TfraCfgPanCom }

  TfraCfgPanCom = class(TFrame)
    Label1: TLabel;
    txtPrompt: TEdit;
  public
    Prompt : string;
    procedure Initiate(cfgFile: TMiConfigBasic);
  end;

implementation

{$R *.lfm}

{ TfraCfgPanCom }

procedure TfraCfgPanCom.Initiate(cfgFile: TMiConfigBasic);
begin
  cfgFile.Asoc_Str(self.Name + '/Prompt', @Prompt, txtPrompt,  '>>');
end;

end.

