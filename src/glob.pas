unit glob;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils,
  toLang;

const
  APP_NAME = 'gTitoCAD';
  {$I meta/APP_VERSION.pas}//APP_VERSION

type
  TEvRefresh = procedure of object;
  tlang = TtoLang; //get toLang object avialable in this module

var
  msg: tlang;

procedure VerifyClose(Accepted, hasError: boolean; var CanClose: boolean);

implementation

procedure VerifyClose(Accepted, hasError: boolean; var CanClose: boolean);
{Verification routine for the OnCloseQuery () event. Decide whether or not to activate the flag
"CanClose", based on the values of "Accepted" and "hasError".}
begin
  if (Accepted and hasError) then
  begin
    //The ACCEPT button has been pressed. There may have been an error
    CanClose := False;  //so that it does not close
    Accepted := False;  //to be able to close then with the button [X] of the form
  end;
end;

end.
