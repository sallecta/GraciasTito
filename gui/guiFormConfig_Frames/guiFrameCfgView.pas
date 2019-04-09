unit guiFrameCfgView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Spin, Menus,
  MiConfigBasic;

type

  { TfraCfgView }

  TfraCfgView = class(TFrame)
    chbShowAxes: TCheckBox;
    chkVerCoorPunt: TCheckBox;
    spnIncrWheel: TFloatSpinEdit;
    Label2: TLabel;
    spnAxeDist: TFloatSpinEdit;
    Label1: TLabel;
    procedure chkVerEjesChange(Sender: TObject);
  public
    VerEjesCoor: boolean;
    LongEjeCoor: Double;
    VerCoorPunt: boolean;
    incrWheel  : Double;
    procedure Initiate(cfgFile: TMiConfigBasic);
  end;

implementation
{$R *.lfm}

procedure TfraCfgView.chkVerEjesChange(Sender: TObject);
begin
  spnAxeDist.Enabled := chbShowAxes.Checked;
  label1.Enabled:= chbShowAxes.Checked;
end;

procedure TfraCfgView.Initiate(cfgFile: TMiConfigBasic);
begin
  cfgFile.Asoc_Bol(self.Name + '/VerEjesCoor', @VerEjesCoor , chbShowAxes, true);
  cfgFile.Asoc_Dbl(self.Name + '/LongEjeCoor', @LongEjeCoor , spnAxeDist, 100);
  cfgFile.Asoc_Bol(self.Name + '/VerCoorPunt', @VerCoorPunt,  chkVerCoorPunt, true);
  cfgFile.Asoc_Dbl(self.Name + '/incrWheel',   @incrWheel  , spnIncrWheel, 100);
  chkVerEjesChange(self);
end;

end.

