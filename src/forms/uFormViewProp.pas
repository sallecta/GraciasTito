
unit uFormViewProp;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, Spin, sketchDocument, uFrameEditor,
  glob
  ;
type

  { TFormViewProp }

  TFormViewProp = class(TForm)
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
    procedure FormCreate(Sender: TObject);
  private
    View: TFramePaintBox;
  public
    procedure Exec(argView: TFramePaintBox);
  end;

var
  formViewProp: TFormViewProp;

implementation
{$R *.lfm}

{ TFormViewProp }

procedure TFormViewProp.btnAcceptClick(Sender: TObject);
begin
  btnApplyClick(self);
end;

procedure TFormViewProp.btnApplyClick(Sender: TObject);
begin
  View.Editor.ShowAxes  := chbShowAxes.Checked;
  View.Editor.AxesDistance  := spnAxeDist.Value;
  View.Editor.ShowRotPoint := chbShowRotPoint.Checked;
  View.Editor.ShowGrid   := chbShowGrid.Checked;
  View.Editor.Refresh;
end;

procedure TFormViewProp.FormCreate(Sender: TObject);
begin
  // captions
  Caption:=msg.get('view')+': '+msg.get('properties');
  chbShowAxes.caption:=msg.get('showAxes');
  Label1.caption:=msg.get('axesLength');
  chbShowRotPoint.caption:=msg.get('showRotationPoint');
  chbShowAxes.caption:=msg.get('showAxes');
  chbShowGrid.caption:=msg.get('showGrid');
  btnAccept.caption:=msg.get('ok');
  btnCancel.caption:=msg.get('cancel');
  btnApply.caption:=msg.get('apply');
end;

procedure TFormViewProp.Exec(argView: TFramePaintBox);
begin
  View := argView;
  chbShowAxes.Checked    := View.Editor.ShowAxes;
  spnAxeDist.Value      := View.Editor.AxesDistance;
  chbShowRotPoint.Checked := View.Editor.ShowRotPoint;
  chbShowGrid.Checked := View.Editor.ShowGrid;
  Showmodal;
end;

end.

