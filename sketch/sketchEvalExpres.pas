{
Unit that defines the TEvalExpres object, which allows to evaluate the result
of a Arithmetic expression in a chain of line.
Recognize the variables. These must be created with TEvalExpres.AisgVariable ().

                                                  By Tito Hinostroza 12/17/2016
}
unit sketchEvalExpres;
{$mode objfpc}{$H+}
interface
uses  Classes, SysUtils, math, Forms, LCLType;
Const
  CONTEXT_END = #0;    //Fin de contexto  End of context

Type
  //ObjType of operand
  TEvExOperand = (OP_UNKNOWN,    //unknown desconocido
               OP_NUM,    //number número
               OP_CHAIN);    //chain cadena

  //Category to classify the operands
  TEvExCategory = (
    CAT_NIL,
    CAT_CONST,
    CAT_VAR,
    CAT_FUNC,
    CAT_EXPR);  //expression

  { TEvExExpression }
  //ObjType expression. It is used for the management of arithmetic evaluation.
  TEvExExpression = object
    txt: String;
    operand: TEvExOperand;    //Type of data that returns the expression
    cat: TEvExCategory;   //Categoría de expresión
  private
    fTxt: String;    //text value of the expression
    fNum: Single;    //Numeric value of the expression
    procedure OperandSetTxt(argTxt: string);
    function VarReadAsText: string;
    procedure OperandSetNumber(argNumber: single);
    function OperandReadNumber: single;
  public
    chainError: string;
    property valTxt: string read VarReadAsText write OperandSetTxt;
    property valNum: single read OperandReadNumber write OperandSetNumber;
  End;

  { TContext }
  {Structure that defines an object context. It is used both to read the entry and to read
    write in the output.}
  TContext = class
    col      : Integer;
    line      : string;
    constructor Create;
    destructor Destroy; override;
    Function atBeginContext:Boolean;
    Function atEndContext:Boolean;
    Function ContextGetCurrChar:Char;
    Function GetCharAnMoveCursor:Char;
    Function ContextGetPrevChar: Char;
    Function ContextGetNextChar: Char;
    Function CapBlancos:Boolean;

    //Métodos de escritura
    procedure ContentGoToBeginning;
    procedure ContentGoToEnd;
  End;

  //Define a una variable.
  //Se define como registro clásico, para optimizar la velocidad.
  TEVar= record
    number: string[12];   //Name de la variable
    value: Double;      //Valor de la variable
  end;

  { TEvalExpres }
  {Objeto evaluador de expresiones.}
  TEvalExpres = class
  public
    Context : TContext;   //reference to the current input context
    vars : array of TEVar;  //Se puede hacer estático, si se quiere ganar velocidad
    nVars: integer;     //Número de variables
    //rutinas basicas de lectura
    Function ContextCurrPosGetChars(argNumOfChars:Integer): String;
    function Capture(argCap: String): Boolean;
    function ContextCurrCharIs(argChar: char): Boolean;
    //Rutinas avanzadas de lectura
    function ContextCurrCharIsNumber(var n:Single):boolean;
    function ContextIsString(var s: string):boolean;
    function ContextIsIdentifier(var s: string):boolean;
    function ContextGetOperator: String;           //coge operador
    function hierarchyOperator(oper: String): Integer;  //jerarquía de operador
    function Evaluate(Op1: TEvExExpression; opr: String; Op2: TEvExExpression): TEvExExpression;
    function GetOperand: TEvExExpression;
    function GetExpresion(hierarchy: Integer): TEvExExpression;
    function CogExpresionEnclosed: TEvExExpression;
    function AssignNumberToVar(const VarName: string; value: Double): integer;
    function EvaluateString(line: string): TEvExExpression;
  public  //Campos para manejo de error
    ErrorCol : integer;   //número de columna del error
    ErrorStr : string;    //cadena de error
    procedure GenError(argMsg: String; col: integer=-1);
  public  //Inicialización
    procedure Initiate(txt: string);   //Prepara la secuencia de preprocesamiento
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TEvExExpression }
procedure TEvExExpression.OperandSetTxt(argTxt: string);
//Set text value of an operand
begin
  operand := OP_CHAIN;  //se fija como cadeana, de otra forma no podría recibir este value
  fTxt := argTxt;
end;
function TEvExExpression.VarReadAsText: string;
//   Read the variable as text
begin
  if operand = OP_CHAIN then  //si ya es texto, la lectura es directa
    Result := ftxt
  else if operand = OP_NUM then //Si es numérico, se hace una transformación
    //siempre se podrá transformar
    Result := FloatToStr(fNum)   //pero no deja de ser numérico
  else
    Result := '';
end;
procedure TEvExExpression.OperandSetNumber(argNumber: single);
begin
  operand := OP_NUM;  //is set as a number, otherwise it could not receive this value
  fNum := argNumber;
end;
function TEvExExpression.OperandReadNumber: single;
begin
  if operand = OP_CHAIN then begin //if it is text, a transformation is made
    //it may not be able to transform
    if not TryStrToFloat(ftxt, Result) then
      chainError := 'Número inválido.';
  end else if operand = OP_NUM then //If it is already numeric, the reading is direct
    Result := fNum
  else
    Result := 0;
end;
{ TContext }
//********************************************************************************
//Funciones Básicas para administración de los Contextos
//********************************************************************************
constructor TContext.Create;
begin
  ContentGoToEnd;   //inicia fil y col
end;
destructor TContext.Destroy;
begin
  inherited Destroy;
end;
function TContext.atBeginContext: Boolean;
//Returns true if it is at the beginning of the Context (column 1)
begin
  Result := (col = 1);
end;
function TContext.atEndContext: Boolean;
//Returns true if the end of the current Context has been passed
begin
  Result := (col >= Length(line) + 1);
end;
function TContext.ContextGetCurrChar: Char;
{
Returns the current character
  If there is no text in the current Context or if the end of the
  text, returns CONTEXT_END.
}
begin
  if atEndContext Then exit(CONTEXT_END);
  Result := line[col];
end;
function TContext.GetCharAnMoveCursor: Char;
{
Read a character from the context and move the cursor one position.
  If there is no text in the current Context or if the end of the
  text, returns CONTEXT_END.
}
begin
  if atEndContext Then exit(CONTEXT_END);
  Result := line[col];
  inc(col);
end;
function TContext.ContextGetPrevChar: Char;
{
Take a look at the previous character of the Context
  If there is no previous character, it returns null character.
}
begin
  if atBeginContext Then exit(#0);
  Result := line[col-1];
end;
function TContext.ContextGetNextChar: Char;
{
Returns the next character to the current one.
If there is no next character, it returns null character.
}
begin
  if atEndContext Then exit(#0);
  Result := line[col+1];
end;
function TContext.CapBlancos: Boolean;
{
Take the initial targets of the input context.
  If it does not find any white at the beginning, it returns false
}
begin
  Result := False;
  while not atEndContext and (ContextGetCurrChar in [' ', #9]) do
    GetCharAnMoveCursor;
end;
procedure TContext.ContentGoToBeginning;
//Move the position to the beginning of the content.
begin
  col := 1;
end;
procedure TContext.ContentGoToEnd;
// Move the position to the end of the content.
begin
  col := length(line)+1;
end;

{ TEvalExpres }
function TEvalExpres.ContextCurrPosGetChars(argNumOfChars: Integer): String;
{
Returns the N characters from the current position of the current Context.
  If there is no text in the current Context or if the end of the
  text, returns CONTEXT_END.
}
begin
  if Context.atEndContext Then Exit(CONTEXT_END);
  Result := copy(Context.line, Context.col, argNumOfChars);
End;
function TEvalExpres.Capture(argCap: String): Boolean;
{Take the given string ignoring the initial targets.}
Var i:Integer;
begin
  Result := False;
  Context.CapBlancos;     //removes initial targets
  i := 1;
  while Not Context.atEndContext And (i <= Length(argCap)) do begin
    if Context.ContextGetCurrChar = argCap[i] then begin
      Context.GetCharAnMoveCursor;
      i := i + 1;
    end else begin
      exit;
    end;
  end;
  if i > Length(argCap) then begin   //found the whole chain
    Result := true;
  end;
End;
function TEvalExpres.ContextCurrCharIs(argChar: char): Boolean;
{Take the indicated character. If it does not find it, it generates an error and returns FALSE.}
begin
  if Context.ContextGetCurrChar=argChar then begin
    //Es el caracter buscado
    Context.GetCharAnMoveCursor;
    exit(true);
  end else begin
    GenError('Error en expresión. Se esperaba "'+ argChar +'"');
    exit(false);
  end;
end;
function TEvalExpres.ContextCurrCharIsNumber(var n: Single): boolean;
{Verify if what you are following is a number and if so, try to take it.
You can generate error when converting the number}
Var aChar:char;
    temp:String;
begin
    aChar := Context.ContextGetCurrChar;
    If Not (aChar in ['0'..'9','.','-']) Then
       exit(false);        //it's not number
    if (aChar in ['.','-']) and not (Context.ContextGetNextChar in ['0'..'9']) then
       exit(false);    //It's not valid
    temp := Context.GetCharAnMoveCursor;   //get first digit
    //search until you find the end of the identifier
    While Context.ContextGetCurrChar in ['0'..'9','.'] do begin
      aChar := Context.GetCharAnMoveCursor;     //take the character
      temp += aChar;
    end;
    //it reached the end of the number
    if not TryStrToFloat(temp, n) then begin
      GenError('Error en número: ' + temp, Context.col);
    end;
    Result := true;  //indicates that there was a number
end;
function TEvalExpres.ContextIsString(var s: string): boolean;
{Take a constant of Type string (between apostrophes) from the position where
the file is located, to the delimiter or end of line.
  If it does not find a string, it returns FALSE}
var
  aChar : char;
begin
  if Context.ContextGetCurrChar <> '''' Then   //first character not valid
    exit(false);        //it is not constant string
  Context.GetCharAnMoveCursor;     //take the character
  s := '';         //start to accumulate
  //search until find end of chain
  while not Context.atEndContext do begin
      aChar := Context.GetCharAnMoveCursor;
      If aChar = '''' Then begin  //end character
        exit(true);
      end Else begin
        s += aChar;
      End;
  end;
  //it came to the end of the file
  GenError('No se encontro fin de cadena');
  Result := true;    //indicates that string was found
end;
function TEvalExpres.ContextIsIdentifier(var s: string): boolean;
{Take an identifier, which must correspond to a variable.}
begin
  if not (Context.ContextGetCurrChar in ['a'..'z','A'..'Z']) then   //first character not valid
    exit(false);//it is not constant string
  s := '';         //start to accumulate
  //search until you find the end of the identifier
  while not Context.atEndContext and (Context.ContextGetCurrChar in ['a'..'z','A'..'Z']) do begin
    s += Context.GetCharAnMoveCursor;
  end;
  Result := true;    //indicates that identifier was found
end;
function TEvalExpres.ContextGetOperator: String;
{Take an operator in the position of the current context. If you do not find
  returns empty string and does not take characters, except leading spaces.}
begin
  ContextGetOperator := '';
  Context.CapBlancos;     //removes initial targets
  Case Context.ContextGetCurrChar of //complete with operator of more characters
  '+': begin
         Result := Context.GetCharAnMoveCursor;
        end;
  '-': begin
         Result := Context.GetCharAnMoveCursor;
      end;
  '*': begin
        Result := Context.GetCharAnMoveCursor;
      end;
  '/': begin
        Result := Context.GetCharAnMoveCursor;
      end;
  '\': begin
        Result := Context.GetCharAnMoveCursor;
      end;
  '%': begin
        Result := Context.GetCharAnMoveCursor;
      end;
  '^': begin
        Result := Context.GetCharAnMoveCursor;
      end;
  End;
End;
function TEvalExpres.hierarchyOperator(oper: String): Integer;
//Returns the hierarchy of a Show operator technical documentation.
begin
    Case oper of
    '+', '-'{, '|', '&'}: hierarchyOperator := 5;
    '*', '/', '\', '%': hierarchyOperator := 6;
    '^'{, '++', '--', '+=', '-=', '*=', '/='}: hierarchyOperator := 8;
    Else hierarchyOperator := 0;
    End;
End;
function TEvalExpres.Evaluate(Op1: TEvExExpression; opr: String; Op2: TEvExExpression): TEvExExpression;
// Returns the result and type of an operation
begin
    ErrorStr:='';
    Evaluate.cat := CAT_EXPR;
    Case opr of
    '': begin     //Without operator And it is assumed without Op2
          //there is nothing to do, it is already in the place
          Evaluate := Op1;
        end;
    '+': begin
          Evaluate.valNum := Op1.valNum + Op2.valNum;  //Force Evaluate.operand: = OP_NUM
         end;
    '-': begin
          Evaluate.valNum := Op1.valNum - Op2.valNum;
         end;
    '*': begin
          Evaluate.valNum := Op1.valNum * Op2.valNum;
         end;
    '/': begin
          if Op2.valNum = 0 Then
              GenError('No se puede dividir por cero.')
          else begin   //error
              Evaluate.valNum := Op1.valNum / Op2.valNum;
          End;
         end;
    '\': begin
          if Op2.valNum = 0 Then
              GenError('No se puede dividir por cero.')
          else begin   //error
              Evaluate.valNum := round(Op1.valNum) div round(Op2.valNum);
          end;
         end;
    '%': begin
          if Op2.valNum = 0 Then
              GenError('No se puede dividir por cero.')
          else begin   //error
              Evaluate.valNum := round(Op1.valNum) mod round(Op2.valNum);
          end;
         end;
    '^': begin
          if (Op2.valNum = 0) And (Op2.valNum = 0) Then
              GenError('No se puede Evaluate 0^0')
          else begin   //error
              Evaluate.valNum := power(Op1.valNum, Op2.valNum);
          end;
         end;
    else begin
        GenError('No se reconoce operador: ' + opr, Context.col);
        Exit;
         End;
    end;
    //Complete fields to evaluate
    Evaluate.txt := Op1.txt + opr + Op2.txt;   //expression text
End;
function TEvalExpres.GetOperand: TEvExExpression;
{Take an operand in the current position of the content. If not
the operand or is wrong, generates Error.}
var
  aChain : String;//bug not used
  num : single;
  expr : TEvExExpression;
  i: Integer;
begin
  Context.CapBlancos;
  if Context.atEndContext then begin
    Result.txt := '';
    exit;
  end;
  if ContextCurrCharIsNumber(num) then begin
    if ErrorStr<>'' then exit;
    Result.txt := '#';
    Result.valNum := num;
    Result.cat := CAT_CONST;
  end else if ContextIsIdentifier(aChain) then begin
    for i:=0 to nVars-1 do begin
      if vars[i].number = aChain then begin
        Result.txt := '$';   //indicates number
        Result.valNum := vars[i].value;
        Result.cat := CAT_VAR;
        exit;
      end;
    end;
    //It is not variable, search if it is a function
    case aChain of //bug aChain is emty
    'abs': begin
      expr := CogExpresionEnclosed;
      if ErrorStr<>'' then exit;
      Result.valNum := abs(expr.valNum);
      exit;  //exit without error
    end;
    'sgn': begin
      expr := CogExpresionEnclosed;
      if ErrorStr<>'' then exit;
      Result.valNum := Sign(expr.valNum);
      exit;  //exit without error
    end;
    'sgn2': begin
      expr := CogExpresionEnclosed;
      if ErrorStr<>'' then exit;
      if expr.valNum<0 then Result.valNum := 0
      else Result.valNum := expr.valNum;
      exit;  //exit without error
    end;
    'int': begin
      expr := CogExpresionEnclosed;
      if ErrorStr<>'' then exit;
      Result.valNum := Int(expr.valNum);
      exit;  //exit without error
    end;
    'sen': begin
      expr := CogExpresionEnclosed;
      if ErrorStr<>'' then exit;
      Result.valNum := sin(expr.valNum);
      exit;  //exit without error
    end;
    'cos': begin
      expr := CogExpresionEnclosed;
      if ErrorStr<>'' then exit;
      Result.valNum := cos(expr.valNum);
      exit;  //exit without error
    end;
    'tan': begin
      expr := CogExpresionEnclosed;
      if ErrorStr<>'' then exit;
      Result.valNum := tan(expr.valNum);
      exit;  //exit without error
    end;
    end;
    //It is not variable or function.
    GenError('Función o variable desconocida: '+aChain, Context.col);
    Result.txt := '';
  end else if ContextIsString(aChain) Then begin   //Constant chain
    Result.txt := 'A';   //indicates chain
    Result.valTxt := aChain;    //Fix Type to number
    Result.cat := CAT_CONST;
  end else If Context.ContextGetCurrChar = '(' Then begin
    Result := CogExpresionEnclosed;
    exit;
  end else begin
    Result.txt := '';
    exit;
  end;
end;
function TEvalExpres.GetExpresion(hierarchy: Integer): TEvExExpression;
{
Take a complete expression, in the current position of the content
  If you can not find an expression, it generates an error
}
var Op1, Op2 : TEvExExpression;
    opr, opr2 : String;
    hieOpr, hieOpr2: Integer;
    pos1, pos2 : integer;
begin
    Context.CapBlancos;  //quita blancos iniciales
    Op1 := GetOperand;  //error
    if ErrorStr<>'' then exit;
//    if Op1.txt = '' then begin
//      Result.cat := CAT_NIL;  //No se reconoce expresión
//      GenError('Se esperaba expresión.');
//      exit;
//    end;
    opr := ContextGetOperator;
    if opr = '' Then begin
      Result := Op1;
      Exit
    End;
    hieOpr := hierarchyOperator(opr);     //There is an operator, take your hierarchy
    //-------------------------- Delimited by hierarchy?---------------------
    if hieOpr <= hierarchy then begin  //es menor que la que sigue, expres.
      Result := Op1;  //solo devuelve el único operando que leyó
      Exit;
    End;
    while opr <> '' do begin
        pos1 := Context.col;    //Guarda por si lo necesita
        Op2 := GetOperand;
        if ErrorStr<>'' then exit;
//        If Op2.txt = '' Then   begin //error
//           GenError('Error en expresión. Se esperaba operando.');
//           exit;
//        end;
        pos2 := Context.col;    //Guarda por si lo necesita
        opr2 := ContextGetOperator;
        If opr2 <> '' Then begin  //Hay otro operador
            hieOpr2 := hierarchyOperator(opr2);
            //Delimited by operator hierarchy?
            If hieOpr2 <= hierarchy Then begin
                Context.col := pos2;   //before catching the operator
                Result := Evaluate(Op1, opr, Op2);
                Exit;
            End;
            If hieOpr2 > hieOpr Then begin    //and it is of higher hierarchy, it goes back
                Context.col:= pos1;        //  go back
                Op2 := GetExpresion(hieOpr);        //evaluate first
                opr2 := ContextGetOperator;    //update the next operator
            End;
        End;

        Op1 := Evaluate(Op1, opr, Op2);    //evaluate result
        if ErrorStr<>'' then exit;
        opr := opr2;
        hieOpr := hierarchyOperator(opr);    //Update previous operator
    end;
    Result := Op1;
    Result.cat := CAT_EXPR;
end;
function TEvalExpres.CogExpresionEnclosed: TEvExExpression;
{Take an expression that should be enclosed in parentheses. It can generate error}
begin
  if not ContextCurrCharIs('(') then exit;
  Result := GetExpresion(0);
  if ErrorStr<>'' then exit;
  Context.CapBlancos;
  if not ContextCurrCharIs(')') then exit;
end;
function TEvalExpres.AssignNumberToVar(const VarName: string; value: Double): integer;
{Assign a numerical value to a variable. If it does not exist, create it.
Returns the index of the variable in the array vasr [].}
var
  i: Integer;
begin
  //Search variable
  for i:=0 to nVars-1 do begin
    if vars[i].number = VarName then begin
      vars[i].value := value;
      exit(i);
    end;
  end;
  //It was not found, it must be CreateIt
  inc(nVars);
  setlength(vars, nVars);
  Result := nVars-1;
  vars[Result].number := VarName;
  vars[Result].value := value;
end;
function TEvalExpres.EvaluateString(line: string): TEvExExpression;
{Evaluate the expression that is contained in "line"}
begin
  ErrorStr:='';
  Initiate(line);
  Result := GetExpresion(0);
  if ErrorStr<>'' then exit;
  //Check if you finished processing the entire line
  if not Context.atEndContext then
    GenError('Error de sintaxis.');
end;
procedure TEvalExpres.GenError(argMsg: String; col: integer = -1);
begin
  ErrorStr := argMsg;
  if col = -1 then ErrorCol := Context.col
  else ErrorCol := col;
end;
constructor TEvalExpres.Create;
begin
  Context := TContext.Create;
  nVars := 0;
  setlength(vars, nVars);
end;
destructor TEvalExpres.Destroy;
begin
  Context.Destroy;
  inherited;
end;
procedure TEvalExpres.Initiate(txt: string);
begin
  Context.line := txt;
  Context.ContentGoToBeginning;
end;

end.

