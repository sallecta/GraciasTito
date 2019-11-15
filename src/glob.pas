unit glob;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Forms,
  //local
  uToLang, uhDoc, uhPage;

const
  APP_NAME = 'gTitoCAD';
  {$I meta/APP_VERSION.pas}//APP_VERSION
  {$I glob/constants.inc.pas} // globabal constants

//type
  //tlang = tuToLang; //get toLang object avialable in this module



var
  msg: uToLang.tuToLang;
  Form1: TForm;
  hDoc: uhDoc.tuhDoc;
  hPage: uhPage.tuhPage;



implementation
uses  uFormForm1;

initialization
glob.msg := tuToLang.Create();
glob.msg.logMissing:=true;



end.
