
unit guiFormViewProp;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, Spin, sketchDocument, guiFramePaintBox;
type

  { TFormPropView }

  TFormPropView = class(TForm)
    btnApply: TBitBtn;
    btnCancel: TBitBtn;
    btnAccept: TBitBtn;
    chbShowAxes: TCheckBox;
    chbShowRotPoint: TCheckBox;
    chbShowGrid: TCheckBox;
    Label1: TLabel;
    Panel1: TPanel;
    spnAxeDist: TSpinEdit;
    procedure btnAcceptClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
  private
    View: TfraPaintBox;
  public
    procedure Exec(argView: TfraPaintBox);
  end;

var
  FormPropView: TFormPropView;

implementation
{$R *.lfm}

{ TFormPropView }

procedure TFormPropView.btnAcceptClick(Sender: TObject);
begin
  btnApplyClick(self);
end;

procedure TFormPropView.btnApplyClick(Sender: TObject);
begin
  View.viewEdi.ShowAxes  := chbShowAxes.Checked;
  View.viewEdi.AxesDistance  := spnAxeDist.Value;
  View.viewEdi.ShowRotPoint := chbShowRotPoint.Checked;
  View.viewEdi.ShowGrid   := chbShowGrid.Checked;
  View.viewEdi.Refresh;
end;

procedure TFormPropView.Exec(argView: TfraPaintBox);
begin
  View := argView;
  chbShowAxes.Checked    := View.viewEdi.ShowAxes;
  spnAxeDist.Value      := View.viewEdi.AxesDistance;
  chbShowRotPoint.Checked := View.viewEdi.ShowRotPoint;
  chbShowGrid.Checked := View.viewEdi.ShowGrid;
  Showmodal;
end;

end.

