unit toLang_defaults;

{$mode objfpc}

interface

procedure createDefaults(objinst: TObject);

implementation

uses toLang;

procedure createDefaults(objinst: TObject);
begin
  TtoLang(objinst).Add('AppName', 'tolangAppName');
  TtoLang(objinst).Add('ProjectExplorer', 'Project Explorer');
  TtoLang(objinst).Add('ProjectModifiedSaveChanges',
      'Project modified. Dou you want to save changes?');//El presupuesto ha sido modificado, ¿Guardar cambios?
  TtoLang(objinst).Add('AskPageDelete', 'Delete page: '); //¿Eliminar página "%s"?
  TtoLang(objinst).Add('InvalidNumber', 'Invalid number'); //Número inválido.
  TtoLang(objinst).Add('ExpressionErrorDetails',
       'Expression error. Expected: ');// Error en expresión. Se esperaba
  TtoLang(objinst).Add('FailedStringToFloat: ',
       'Failed convert string to float: '); // Error en número:
  TtoLang(objinst).Add('EndOfStringNotFound',
        'End of string not found'); // No se encontro fin de cadena
  TtoLang(objinst).Add('CantDevideByZero.',
         'Can not devide by zero'); // No se puede dividir por cero.
  TtoLang(objinst).Add('CantEvaluate0of0.',
         'Can not Evaluate 0 ^ 0'); // No se puede Evaluate 0^0
  TtoLang(objinst).Add('NoOperatorRecognized',
         'No operator is recognized: '); // No se reconoce operador:
  TtoLang(objinst).Add('UnknownFuncOrVar',
         'Unknown function or variable: '); // Función o variable desconocida:
  TtoLang(objinst).Add('SyntaxError.',
         'SyntaxError'); // Error de sintaxis
  TtoLang(objinst).Add('CantDeleteLastPage',
         'Can not delete last page'); // No se pueden eliminar todas las páginas.
  TtoLang(objinst).Add('PageDoesNotExist',
         'Page does not exist: '); // No existe la página
  TtoLang(objinst).Add('ProjectDefaultName',
         'Project'); // Proyecto
  TtoLang(objinst).Add('documentObjects',
         'Objects'); // objects Gráficos
  TtoLang(objinst).Add('defaultObjName',
         'Object'); // Objeto
  TtoLang(objinst).Add('stateNormal',
         'Normal'); // stateNormal
  TtoLang(objinst).Add('stateSelMultiple',
         'Multiple selection'); // Selecc. Múltiple
  TtoLang(objinst).Add('stateObjectsMoving.',
         'Objects moving'); // 'Moviendo objects'
  TtoLang(objinst).Add('stateScreenScrolling.',
         'Screen scrolling'); // 'Desplaz. Pantalla'
  TtoLang(objinst).Add('stateScreenRotating',
         'Screen rotating'); // Rotando Pantalla
  TtoLang(objinst).Add('stateDimensioningObjects',
         'Dimensioning objects'); // 'Dimension.objects'
  TtoLang(objinst).Add('stateMouseZooming',
         'Mouse zooming'); // 'Zoom con ratón'
  TtoLang(objinst).Add('stateLineCreating',
         'Line creating'); // Modo línea
  TtoLang(objinst).Add('stateRectangleCreating',
         'Rectangle creating'); // Modo Rectángulo
  TtoLang(objinst).Add('stateUnknown',
         'Unknown state'); // << Desconocido >>;
  TtoLang(objinst).Add('CommandUnknown',
         'Uunknown command: '); // Comando desconocido:
  TtoLang(objinst).Add('enterStartingPoint',
         '>> Enter starting point:'); // >> Ingrese punto inicial:
  TtoLang(objinst).Add('commandPrompt',
         '>>'); // '>> Comando:'
  TtoLang(objinst).Add('errEnterStartPoint',
         'ERROR. Enter starting point:'); // ERROR: Ingrese punto inicial:
  TtoLang(objinst).Add('enterFollowingPoint',
         'Enter following point: '); // '>> Ingrese siguiente punto ([C]errar):'
  TtoLang(objinst).Add('enterNextPoint',
         'Enter next point'); // Ingrese siguiente punto ([C]errar):
  TtoLang(objinst).Add('file',
         '&File'); // &Archivo
  TtoLang(objinst).Add('newProject',
         '&New Project...'); // Nuevo &Proyecto...
  TtoLang(objinst).Add('open',
         '&Open'); // &Abrir
  TtoLang(objinst).Add('save',
         '&Save'); // G&uardar 
  TtoLang(objinst).Add('close',
         '&Close'); // Cerrar
  TtoLang(objinst).Add('exit',
         '&Exit'); // Salir
  TtoLang(objinst).Add('view',
         'View'); // &Show
  TtoLang(objinst).Add('newitem8',
         'New Item8'); // empty
  TtoLang(objinst).Add('project',
         'Project'); // &Projecto
  TtoLang(objinst).Add('addPage',
         'Add Page'); // Agregar &Página
  TtoLang(objinst).Add('insertRectangle',
         'Insert rectangle'); // Insertar Rectángulo
  TtoLang(objinst).Add('insertPolyline',
         'Insert polyline'); // Insertar Po&lilinea
  TtoLang(objinst).Add('properties',
         'Properties'); // Propiedades
  TtoLang(objinst).Add('page',
         'Page'); // Pa&gina
  TtoLang(objinst).Add('rename',
         'Rename'); // Cambiar &Name
  TtoLang(objinst).Add('remove',
         'Remove'); // Eliminar
  TtoLang(objinst).Add('tools',
         'Tools'); // &Herramientas
  TtoLang(objinst).Add('config',
         'Configuration'); // Confi&guración
  TtoLang(objinst).Add('insert',
         'Insert'); // &Insertar
  TtoLang(objinst).Add('addLine',
         'Add line'); // Agregar &Línea
  TtoLang(objinst).Add('rotate',
         '&Rotate'); // &Rotar
  TtoLang(objinst).Add('ToolButton4',
         'ToolButton4'); // ToolButton4
  TtoLang(objinst).Add('ToolButton8',
         'ToolButton8'); // ToolButton8
  TtoLang(objinst).Add('viewSuperior',
         'View Superior'); // View S&uperior
  TtoLang(objinst).Add('acProjInsRectan',
         'acProjInsRectan'); // acProjInsRectan
  TtoLang(objinst).Add('editor',
         'Editor'); // Diseño
  TtoLang(objinst).Add('TabSheet2',
         'TabSheet2'); // TabSheet2
  TtoLang(objinst).Add('ok',
         'OK'); // empty
  TtoLang(objinst).Add('cancel',
         'Cancel'); // empty
  TtoLang(objinst).Add('apply',
         'Apply'); // empty
  TtoLang(objinst).Add('showToolbar',
         'Show toolbar'); // Show Barra de &Herramientas
  TtoLang(objinst).Add('icons',
         'Icons');
  TtoLang(objinst).Add('small',
         'Small');
  TtoLang(objinst).Add('big',
          'Big');
  TtoLang(objinst).Add('showStatusbar',
         'Show status bar'); // Show Barra de &state
  TtoLang(objinst).Add('showAxes',
         'Show axes'); // Show &Ejes Coordenados.
  TtoLang(objinst).Add('axesLength',
         'Axes length'); // Longitud de ejes:
  TtoLang(objinst).Add('showCoordPoint',
         'Show coordinate point'); // Mostrar Coord. del &Puntero en barra de state.
  TtoLang(objinst).Add('mouseWheelIncrement',
         'Mouse wheel increment'); // Incremento de ángulo con la rueda del Ratón:
  TtoLang(objinst).Add('prompt',
         'Prompt'); // Prompt:
  TtoLang(objinst).Add('general',
         'General'); // empty
  TtoLang(objinst).Add('view',
         'View'); // empty
  TtoLang(objinst).Add('commandPanel',
         'Command panel'); // empty
  TtoLang(objinst).Add('readPerspective',
         'Read Perspective'); // &Leer Perspectiva.
  TtoLang(objinst).Add('xpos',
         'X pos'); // empty
  TtoLang(objinst).Add('ypos',
         'Y pos'); // empty
  TtoLang(objinst).Add('clearPerspective',
         'Clear perspective'); // Lim&piar Perspectiva
  TtoLang(objinst).Add('xoffs',
         'X offset'); // X_Cam:
  TtoLang(objinst).Add('yoffs',
         'Y offset'); // Y_Cam:
  TtoLang(objinst).Add('setPerspective',
         'Set perspective'); // Fijar Perspectiva
  TtoLang(objinst).Add('alphaAngle',
         'Alpha angle'); // empty
  TtoLang(objinst).Add('fiAngle',
         'Fi angle'); // empty
  TtoLang(objinst).Add('zoom',
         'Zoom'); // empty
  TtoLang(objinst).Add('name',
         'Name'); // empty
  TtoLang(objinst).Add('notes',
         'Notes'); // Notas
  TtoLang(objinst).Add('author',
         'Author'); // Creado por:
  TtoLang(objinst).Add('showRotationPoint',
         'Show rotation point'); // Show Punto de &Giro
  TtoLang(objinst).Add('showGrid',
         'Show grid'); // Show &Cuadrícula
  TtoLang(objinst).Add('page',
         'Page'); // Página
  TtoLang(objinst).Add('objectExample',
         'Example object'); // empty
  TtoLang(objinst).Add('empty',
         'empty'); // empty
  TtoLang(objinst).Add('empty',
         'empty'); // empty
  TtoLang(objinst).Add('empty',
         'empty'); // empty
  TtoLang(objinst).Add('empty',
         'empty'); // empty
  TtoLang(objinst).Add('empty',
         'empty'); // empty
  TtoLang(objinst).Add('empty',
         'empty'); // empty
  TtoLang(objinst).Add('empty',
         'empty'); // empty

end;

end.
