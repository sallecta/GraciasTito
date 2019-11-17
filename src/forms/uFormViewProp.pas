
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
  frameEditor.Editor.ShowAxes  := chbShowAxes.Checked;
  frameEditor.Editor.AxesDistance  := spnAxeDist.Value;
  frameEditor.Editor.ShowRotPoint := chbShowRotPoint.Checked;
  frameEditor.Editor.ShowGrid   := chbShowGrid.Checked;
  frameEditor.Editor.Refresh;
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
  chbShowAxes.Checked    := frameEditor.Editor.ShowAxes;
  spnAxeDist.Value      := frameEditor.Editor.AxesDistance;
  chbShowRotPoint.Checked := frameEditor.Editor.ShowRotPoint;
  chbShowGrid.Checked := frameEditor.Editor.ShowGrid;
  Showmodal;
end;

end.

