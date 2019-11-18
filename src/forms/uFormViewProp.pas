
unit uFormViewProp;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, Spin, uDoc, uFrameEditor
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
    frameEditor: TframeEditor;
  public
    procedure Exec(argView: TframeEditor);
  end;



implementation
uses glob;
{$R *.lfm}

{ TFormViewProp }

procedure TFormViewProp.btnAcceptClick(Sender: TObject);
begin
  btnApplyClick(self);
end;

procedure TFormViewProp.btnApplyClick(Sender: TObject);
begin
  frameEditor.docEditor.ShowAxes  := chbShowAxes.Checked;
  frameEditor.docEditor.AxesDistance  := spnAxeDist.Value;
  frameEditor.docEditor.ShowRotPoint := chbShowRotPoint.Checked;
  frameEditor.docEditor.ShowGrid   := chbShowGrid.Checked;
  frameEditor.docEditor.Refresh;
end;

procedure TFormViewProp.FormCreate(Sender: TObject);
begin
  // captions
  Caption:=msg.get('frameEditor')+': '+msg.get('properties');
  chbShowAxes.caption:=msg.get('showAxes');
  Label1.caption:=msg.get('axesLength');
  chbShowRotPoint.caption:=msg.get('showRotationPoint');
  chbShowAxes.caption:=msg.get('showAxes');
  chbShowGrid.caption:=msg.get('showGrid');
  btnAccept.caption:=msg.get('ok');
  btnCancel.caption:=msg.get('cancel');
  btnApply.caption:=msg.get('apply');
end;

procedure TFormViewProp.Exec(argView: TframeEditor);
begin
  frameEditor := argView;
  chbShowAxes.Checked    := frameEditor.docEditor.ShowAxes;
  spnAxeDist.Value      := frameEditor.docEditor.AxesDistance;
  chbShowRotPoint.Checked := frameEditor.docEditor.ShowRotPoint;
  chbShowGrid.Checked := frameEditor.docEditor.ShowGrid;
  Showmodal;
end;

end.

