{Formulario para configurar ub objeto View }
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
    btnCancelar: TBitBtn;
    btnAceptar: TBitBtn;
    chkVerEjes: TCheckBox;
    chkVerPtoGiro: TCheckBox;
    chkVerCuadric: TCheckBox;
    Label1: TLabel;
    Panel1: TPanel;
    spnLongEje: TSpinEdit;
    procedure btnAceptarClick(Sender: TObject);
    procedure btnAplicarClick(Sender: TObject);
  private
    View: TfraPaintBox;
  public
    procedure Exec(vista0: TfraPaintBox);
  end;

var
  FormPropView: TFormPropView;

implementation
{$R *.lfm}

{ TFormPropView }

procedure TFormPropView.btnAceptarClick(Sender: TObject);
begin
  btnAplicarClick(self);
end;

procedure TFormPropView.btnAplicarClick(Sender: TObject);
begin
  View.viewEdi.VerEjesCoor  := chkVerEjes.Checked;
  View.viewEdi.LonEjesCoor  := spnLongEje.Value;
  View.viewEdi.VerPuntoGiro := chkVerPtoGiro.Checked;
  View.viewEdi.VerCuadric   := chkVerCuadric.Checked;
  View.viewEdi.Refresh;
end;

procedure TFormPropView.Exec(vista0: TfraPaintBox);
begin
  View := vista0;
  chkVerEjes.Checked    := View.viewEdi.VerEjesCoor;
  spnLongEje.Value      := View.viewEdi.LonEjesCoor;
  chkVerPtoGiro.Checked := View.viewEdi.VerPuntoGiro;
  chkVerCuadric.Checked := View.viewEdi.VerCuadric;
  Showmodal;
end;

end.

