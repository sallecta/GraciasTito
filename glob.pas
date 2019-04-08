unit glob;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils;

const
  APP_NAME = 'GraciasTito';
  {$I meta/APP_VERSION.pas}   //APP_VERSION

type
  TEvRefresh = procedure of object;

  procedure VerifyClose(Accepted, hasError: boolean; var CanClose: boolean);

implementation

procedure VerifyClose(Accepted, hasError: boolean; var CanClose: boolean);
{Verification routine for the OnCloseQuery () event. Decide whether or not to activate the flag
"CanClose", based on the values of "Accepted" and "hasError".}
begin
  if Accepted then begin
    //The ACCEPT button has been pressed. There may have been an error
    if hasError then begin
      //ACCEPT has been pressed but there are errors in the data
      CanClose := false;  //so that it does not close
      Accepted := false;  //to be able to close then with the button [X] of the form
    end else begin
      {
      There were no errors with ACCEPT
       It closes normally
      }
    end;
  end else begin
    {
    You have pressed CANCEL or the [X] button of the form
          It closes normally
    }
  end;
end;

end.

