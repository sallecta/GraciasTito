unit glob;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils;

const
  APP_NAME = 'GraciasTito';   //nombre de programa
  {$I meta/version.txt}   //versión del programa

type
  TEvRefresh = procedure of object;

  procedure VerifyClose(Accepted, ErrorData: boolean; var CanClose: boolean);

implementation

procedure VerifyClose(Accepted, ErrorData: boolean; var CanClose: boolean);
{Rutina de verificación para el evento OnCloseQuery(). Decide si activar o no la bandera
"CanClose", en base a los valores d "Accepted" y "ErrorData".}
begin
  if Accepted then begin
    //Se ha pulsado el botón ACEPTAR. Puede que haya habido error
    if ErrorData then begin
      //Se ha pulsado ACEPTAR pero hay errores, en los datos
      CanClose := false;  //para que no se cierre
      Accepted := false;  //para poder cerrar luego con el botón [X] del formulario
    end else begin
      //No hubo errores con ACEPTAR
      //Se cierra normalmente
    end;
  end else begin
    //Se ha pulsado CANCELAR o el botón [X] del formulario
    //Se cierra normalmente
  end;
end;

end.

