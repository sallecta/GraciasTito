unit toLang_defaults;

{$mode objfpc}

interface

procedure createDefaults(objinst: TObject);

implementation

uses uToLang;

procedure createDefaults(objinst: TObject);
begin
  tuToLang(objinst).Add('AppName', 'tolangAppName');
  tuToLang(objinst).Add('DocumentExplorer', 'Document Explorer');
  tuToLang(objinst).Add('DocumentModifiedSaveChanges',
      'Document modified. Dou you want to save changes?');//El presupuesto ha sido modificado, ¿Guardar cambios?
  tuToLang(objinst).Add('AskPageDelete', 'Delete page: '); //¿Eliminar página "%s"?
  tuToLang(objinst).Add('InvalidNumber', 'Invalid number'); //Número inválido.
  tuToLang(objinst).Add('ExpressionErrorDetails',
       'Expression error. Expected: ');// Error en expresión. Se esperaba
  tuToLang(objinst).Add('FailedStringToFloat: ',
       'Failed convert string to float: '); // Error en número:
  tuToLang(objinst).Add('EndOfStringNotFound',
        'End of string not found'); // No se encontro fin de cadena
  tuToLang(objinst).Add('CantDevideByZero.',
         'Can not devide by zero'); // No se puede dividir por cero.
  tuToLang(objinst).Add('CantEvaluate0of0.',
         'Can not Evaluate 0 ^ 0'); // No se puede Evaluate 0^0
  tuToLang(objinst).Add('NoOperatorRecognized',
         'No operator is recognized: '); // No se reconoce operador:
  tuToLang(objinst).Add('UnknownFuncOrVar',
         'Unknown function or variable: '); // Función o variable desconocida:
  tuToLang(objinst).Add('SyntaxError.',
         'SyntaxError'); // Error de sintaxis
  tuToLang(objinst).Add('CantDeleteLastPage',
         'Can not delete last page'); // No se pueden eliminar todas las páginas.
  tuToLang(objinst).Add('PageDoesNotExist',
         'Page does not exist: '); // No existe la página
  tuToLang(objinst).Add('documentObjects',
         'sketchCoreObjects'); // sketchCoreObjects Gráficos
  tuToLang(objinst).Add('defaultObjName',
         'Object'); // Objeto
  tuToLang(objinst).Add('stateNormal',
         'Normal'); // stateNormal
  tuToLang(objinst).Add('stateSelMultiple',
         'Multiple selection'); // Selecc. Múltiple
  tuToLang(objinst).Add('stateObjectsMoving.',
         'sketchCoreObjects moving'); // 'Moviendo sketchCoreObjects'
  tuToLang(objinst).Add('stateScreenScrolling.',
         'Screen scrolling'); // 'Desplaz. Pantalla'
  tuToLang(objinst).Add('stateScreenRotating',
         'Screen rotating'); // Rotando Pantalla
  tuToLang(objinst).Add('stateDimensioningObjects',
         'Dimensioning sketchCoreObjects'); // 'Dimension.sketchCoreObjects'
  tuToLang(objinst).Add('stateMouseZooming',
         'Mouse zooming'); // 'Zoom con ratón'
  tuToLang(objinst).Add('stateLineCreating',
         'Line creating'); // Modo línea
  tuToLang(objinst).Add('stateRectangleCreating',
         'Rectangle creating'); // Modo Rectángulo
  tuToLang(objinst).Add('stateUnknown',
         'Unknown state'); // << Desconocido >>;
  tuToLang(objinst).Add('CommandUnknown',
         'Uunknown command: '); // Comando desconocido:
  tuToLang(objinst).Add('enterStartingPoint',
         '>> Enter starting point:'); // >> Ingrese punto inicial:
  tuToLang(objinst).Add('commandPrompt',
         '>>'); // '>> Comando:'
  tuToLang(objinst).Add('errEnterStartPoint',
         'ERROR. Enter starting point:'); // ERROR: Ingrese punto inicial:
  tuToLang(objinst).Add('enterFollowingPoint',
         'Enter following point: '); // '>> Ingrese siguiente punto ([C]errar):'
  tuToLang(objinst).Add('enterNextPoint',
         'Enter next point'); // Ingrese siguiente punto ([C]errar):
  tuToLang(objinst).Add('file',
         '&File'); // &Archivo
 tuToLang(objinst).Add('newReservedDocument',
         '&New document');
  tuToLang(objinst).Add('newCustomDocument',
         'New custom &document');
  tuToLang(objinst).Add('open',
         '&Open'); // &Abrir
  tuToLang(objinst).Add('save',
         '&Save'); // G&uardar 
  tuToLang(objinst).Add('close',
         '&Close'); // Cerrar
  tuToLang(objinst).Add('exit',
         '&Exit'); // Salir
  tuToLang(objinst).Add('frameEditor',
         'Editor'); // &Show
  tuToLang(objinst).Add('newitem8',
         'New Item8'); // empty
  tuToLang(objinst).Add('document',
         'Document'); // &Documento
  tuToLang(objinst).Add('addPage',
         'Add Page'); // Agregar &Página
  tuToLang(objinst).Add('insertRectangle',
         'Insert rectangle'); // Insertar Rectángulo
  tuToLang(objinst).Add('insertPolyline',
         'Insert polyline'); // Insertar Po&lilinea
  tuToLang(objinst).Add('properties',
         'Properties'); // Propiedades
  tuToLang(objinst).Add('page',
         'Page'); // Pa&gina
  tuToLang(objinst).Add('rename',
         'Rename'); // Cambiar &Name
  tuToLang(objinst).Add('remove',
         'Remove'); // Eliminar
  tuToLang(objinst).Add('tools',
         'Tools'); // &Herramientas
  tuToLang(objinst).Add('config',
         'Configuration'); // Confi&guración
  tuToLang(objinst).Add('insert',
         'Insert'); // &Insertar
  tuToLang(objinst).Add('addLine',
         'Add line'); // Agregar &Línea
  tuToLang(objinst).Add('rotate',
         '&Rotate'); // &Rotar
  tuToLang(objinst).Add('ToolButton4',
         'ToolButton4'); // ToolButton4
  tuToLang(objinst).Add('ToolButton8',
         'ToolButton8'); // ToolButton8
  tuToLang(objinst).Add('viewSuperior',
         'frameEditor Superior'); // frameEditor S&uperior
  tuToLang(objinst).Add('acDocInsRectan',
         'acDocInsRectan'); // acDocInsRectan
  tuToLang(objinst).Add('editor',
         'Editor'); // Diseño
  tuToLang(objinst).Add('TabSheet2',
         'TabSheet2'); // TabSheet2
  tuToLang(objinst).Add('ok',
         'OK'); // empty
  tuToLang(objinst).Add('cancel',
         'Cancel'); // empty
  tuToLang(objinst).Add('apply',
         'Apply'); // empty
  tuToLang(objinst).Add('showToolbar',
         'Show toolbar'); // Show Barra de &Herramientas
  tuToLang(objinst).Add('icons',
         'Icons');
  tuToLang(objinst).Add('small',
         'Small');
  tuToLang(objinst).Add('big',
          'Big');
  tuToLang(objinst).Add('showStatusbar',
         'Show status bar'); // Show Barra de &state
  tuToLang(objinst).Add('showAxes',
         'Show axes'); // Show &Ejes Coordenados.
  tuToLang(objinst).Add('axesLength',
         'Axes length'); // Longitud de ejes:
  tuToLang(objinst).Add('showCoordPoint',
         'Show coordinate point'); // Mostrar Coord. del &Puntero en barra de state.
  tuToLang(objinst).Add('mouseWheelIncrement',
         'Mouse wheel increment'); // Incremento de ángulo con la rueda del Ratón:
  tuToLang(objinst).Add('prompt',
         'Prompt'); // Prompt:
  tuToLang(objinst).Add('general',
         'General'); // empty
  tuToLang(objinst).Add('frameEditor',
         'frameEditor'); // empty
  tuToLang(objinst).Add('commandPanel',
         'Command panel'); // empty
  tuToLang(objinst).Add('readPerspective',
         'Read Perspective'); // &Leer Perspectiva.
  tuToLang(objinst).Add('xpos',
         'X pos'); // empty
  tuToLang(objinst).Add('ypos',
         'Y pos'); // empty
  tuToLang(objinst).Add('clearPerspective',
         'Clear perspective'); // Lim&piar Perspectiva
  tuToLang(objinst).Add('xoffs',
         'X offset'); // X_Cam:
  tuToLang(objinst).Add('yoffs',
         'Y offset'); // Y_Cam:
  tuToLang(objinst).Add('setPerspective',
         'Set perspective'); // Fijar Perspectiva
  tuToLang(objinst).Add('alphaAngle',
         'Alpha angle'); // empty
  tuToLang(objinst).Add('fiAngle',
         'Fi angle'); // empty
  tuToLang(objinst).Add('zoom',
         'Zoom'); // empty
  tuToLang(objinst).Add('name',
         'Name'); // empty
  tuToLang(objinst).Add('notes',
         'Notes'); // Notas
  tuToLang(objinst).Add('author',
         'Author'); // Creado por:
  tuToLang(objinst).Add('showRotationPoint',
         'Show rotation point'); // Show Punto de &Giro
  tuToLang(objinst).Add('showGrid',
         'Show grid'); // Show &Cuadrícula
  tuToLang(objinst).Add('page',
         'Page'); // Página
  tuToLang(objinst).Add('objectExample',
         'Example object'); // empty
  tuToLang(objinst).Add('draw',
         'Draw'); // empty
  tuToLang(objinst).Add('drawPoly',
         'Draw poly line'); // empty
  tuToLang(objinst).Add('nodocuments',
         'No Documents'); // empty
  tuToLang(objinst).Add('empty',
         'empty'); // empty
  tuToLang(objinst).Add('empty',
         'empty'); // empty
  tuToLang(objinst).Add('empty',
         'empty'); // empty
  tuToLang(objinst).Add('empty',
         'empty'); // empty

end;

end.
