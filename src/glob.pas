unit glob;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Forms,
  //local
  uToLang,
  //uDoc,
  uhDoc, uhPage, uhView, uhDraw;

const
  APP_NAME = 'gTitoCAD';
  {$I meta/APP_VERSION.pas}//APP_VERSION
  {$I glob/constants.inc.pas} // globabal constants

//type
  //tlang = tuToLang; //get toLang object avialable in this module



var
  msg: uToLang.tuToLang;
  Form1, formDocProps, formViewProp: TForm;
  hDoc: uhDoc.tuhDoc;
  hPage: uhPage.tuhPage;
  hView: uhView.tuhView;
  hDraw: uhDraw.tuhDraw;
  //doc: TObject;//TDoc;



implementation
uses  uForm1;

initialization
glob.msg := tuToLang.Create();
glob.msg.logMissing:=true;



end.
