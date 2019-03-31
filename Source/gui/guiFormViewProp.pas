{Formulario para configurar ub objeto Vista }
unit guiFormViewProp;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, Spin, sketchDocument, guiFramePaintBox;
type

  { TFormPropView }

  TFormPropView = class(TForm)
    btnAplicar: TBitBtn;
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
    vista: TfraVisorGraf;
  public
    procedure Exec(vista0: TfraVisorGraf);
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
  vista.visEdi.VerEjesCoor  := chkVerEjes.Checked;
  vista.visEdi.LonEjesCoor  := spnLongEje.Value;
  vista.visEdi.VerPuntoGiro := chkVerPtoGiro.Checked;
  vista.visEdi.VerCuadric   := chkVerCuadric.Checked;
  vista.visEdi.Refrescar;
end;

procedure TFormPropView.Exec(vista0: TfraVisorGraf);
begin
  vista := vista0;
  chkVerEjes.Checked    := vista.visEdi.VerEjesCoor;
  spnLongEje.Value      := vista.visEdi.LonEjesCoor;
  chkVerPtoGiro.Checked := vista.visEdi.VerPuntoGiro;
  chkVerCuadric.Checked := vista.visEdi.VerCuadric;
  Showmodal;
end;

end.

