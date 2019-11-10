{
Unit that defines the TEvalExpres object, which allows to evaluate the result
of a Arithmetic expression in a chain of line.
Recognize the variables. These must be created with TEvalExpres.AisgVariable ().

                                                  By Tito Hinostroza 12/17/2016
}
unit sketchEvalExpres;

{$mode objfpc}{$H+}
interface

uses  Classes, SysUtils, Math, Forms, LCLType, glob
  ;

const
  CONTEXT_END = #0;    //Fin de contexto  End of context

type
  //ObjType of operand
  TEvExOperand = (OP_UNKNOWN,    //unknown
    OP_NUM,    //number
    OP_CHAIN);    //chain

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
    txt: string;
    operand: TEvExOperand;    //Type of data that returns the expression
    cat: TEvExCategory;   //Categoría de expresión
  private
    fTxt: string;    //text value of the expression
    fNum: single;    //Numeric value of the expression
    procedure OperandSetTxt(argTxt: string);
    function VarReadAsText: string;
    procedure OperandSetNumber(argNumber: single);
    function OperandReadNumber: single;
  public
    chainError: string;
    property valTxt: string read VarReadAsText write OperandSetTxt;
    property valNum: single read OperandReadNumber write OperandSetNumber;
  end;

  { TContext }
  {Structure that defines an object context. It is used both to read the entry and to read
    write in the output.}
  TContext = class
    col: integer;
    line: string;
    constructor Create;
    destructor Destroy; override;
    function atBeginContext: boolean;
    function atEndContext: boolean;
    function ContextGetCurrChar: char;
    function GetCharAnMoveCursor: char;
    function ContextGetPrevChar: char;
    function ContextGetNextChar: char;
    function CapBlancos: boolean;
    procedure ContentGoToBeginning;
    procedure ContentGoToEnd;
  end;

  //Define a una variable.
  TEVar = record
    number: string[12];
    Value: double;
  end;

  { TEvalExpres }
  TEvalExpres = class
  public
    Context: TContext;   //reference to the current input context
    vars: array of TEVar;
    nVars: integer;
    function ContextCurrPosGetChars(argNumOfChars: integer): string;
    function Capture(argCap: string): boolean;
    function ContextCurrCharIs(argChar: char): boolean;
    function ContextCurrCharIsNumber(var n: single): boolean;
    function ContextIsString(var s: string): boolean;
    function ContextIsIdentifier(var s: string): boolean;
    function ContextGetOperator: string;
    function hierarchyOperator(oper: string): integer;
    function Evaluate(Op1: TEvExExpression; opr: string;
      Op2: TEvExExpression): TEvExExpression;
    function GetOperand: TEvExExpression;
    function GetExpresion(hierarchy: integer): TEvExExpression;
    function CogExpresionEnclosed: TEvExExpression;
    function AssignNumberToVar(const VarName: string; Value: double): integer;
    function EvaluateString(line: string): TEvExExpression;
  public
    ErrorCol: integer;
    ErrorStr: string;
    procedure GenError(argMsg: string; col: integer = -1);
  public
    procedure Initiate(txt: string);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TEvExExpression }
procedure TEvExExpression.OperandSetTxt(argTxt: string);
//Set text value of an operand
begin
  operand := OP_CHAIN;
  fTxt := argTxt;
end;

function TEvExExpression.VarReadAsText: string;
  //   Read the variable as text
begin
  if operand = OP_CHAIN then
    Result := ftxt
  else if operand = OP_NUM then
    Result := FloatToStr(fNum)
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
  if operand = OP_CHAIN then
  begin //if it is text, a transformation is made
    //it may not be able to transform
    if not TryStrToFloat(ftxt, Result) then
      chainError := msg.Get('InvalidNumber');
  end
  else if operand = OP_NUM then //If it is already numeric, the reading is direct
    Result := fNum
  else
    Result := 0;
end;

{ TContext }
constructor TContext.Create;
begin
  ContentGoToEnd;
end;

destructor TContext.Destroy;
begin
  inherited Destroy;
end;

function TContext.atBeginContext: boolean;
  //Returns true if it is at the beginning of the Context (column 1)
begin
  Result := (col = 1);
end;

function TContext.atEndContext: boolean;
  //Returns true if the end of the current Context has been passed
begin
  Result := (col >= Length(line) + 1);
end;

function TContext.ContextGetCurrChar: char;
{
Returns the current character
  If there is no text in the current Context or if the end of the
  text, returns CONTEXT_END.
}
begin
  if atEndContext then
    exit(CONTEXT_END);
  Result := line[col];
end;

function TContext.GetCharAnMoveCursor: char;
{
Read a character from the context and move the cursor one position.
  If there is no text in the current Context or if the end of the
  text, returns CONTEXT_END.
}
begin
  if atEndContext then
    exit(CONTEXT_END);
  Result := line[col];
  Inc(col);
end;

function TContext.ContextGetPrevChar: char;
{
Take a look at the previous character of the Context
  If there is no previous character, it returns null character.
}
begin
  if atBeginContext then
    exit(#0);
  Result := line[col - 1];
end;

function TContext.ContextGetNextChar: char;
{
Returns the next character to the current one.
If there is no next character, it returns null character.
}
begin
  if atEndContext then
    exit(#0);
  Result := line[col + 1];
end;

function TContext.CapBlancos: boolean;
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
  col := length(line) + 1;
end;

{ TEvalExpres }
function TEvalExpres.ContextCurrPosGetChars(argNumOfChars: integer): string;
{
Returns the N characters from the current position of the current Context.
  If there is no text in the current Context or if the end of the
  text, returns CONTEXT_END.
}
begin
  if Context.atEndContext then
    Exit(CONTEXT_END);
  Result := copy(Context.line, Context.col, argNumOfChars);
end;

function TEvalExpres.Capture(argCap: string): boolean;
  {Take the given string ignoring the initial targets.}
var
  i: integer;
begin
  Result := False;
  Context.CapBlancos;     //removes initial targets
  i := 1;
  while not Context.atEndContext and (i <= Length(argCap)) do
    if Context.ContextGetCurrChar = argCap[i] then
    begin
      Context.GetCharAnMoveCursor;
      i := i + 1;
    end
    else
      exit;
  if i > Length(argCap) then
    Result := True//found the whole chain
  ;
end;

function TEvalExpres.ContextCurrCharIs(argChar: char): boolean;
  {Take the indicated character. If it does not find it, it generates an error and returns FALSE.}
begin
  if Context.ContextGetCurrChar = argChar then
  begin
    //Es el caracter buscado
    Context.GetCharAnMoveCursor;
    exit(True);
  end
  else
  begin
    GenError(msg.Get('ExpressionErrorDetails') + argChar );
    exit(False);
  end;
end;

function TEvalExpres.ContextCurrCharIsNumber(var n: single): boolean;
{Verify if what you are following is a number and if so, try to take it.
You can generate error when converting the number}
var
  aChar: char;
  temp: string;
begin
  aChar := Context.ContextGetCurrChar;
  if not (aChar in ['0'..'9', '.', '-']) then
    exit(False);        //it's not number
  if (aChar in ['.', '-']) and not (Context.ContextGetNextChar in ['0'..'9']) then
    exit(False);    //It's not valid
  temp := Context.GetCharAnMoveCursor;   //get first digit
  //search until you find the end of the identifier
  while Context.ContextGetCurrChar in ['0'..'9', '.'] do
  begin
    aChar := Context.GetCharAnMoveCursor;     //take the character
    temp += aChar;
  end;
  //it reached the end of the number
  if not TryStrToFloat(temp, n) then
    GenError(msg.Get('FailedStringToFloat') + temp, Context.col);
  Result := True;  //indicates that there was a number
end;

function TEvalExpres.ContextIsString(var s: string): boolean;
{Take a constant of Type string (between apostrophes) from the position where
the file is located, to the delimiter or end of line.
  If it does not find a string, it returns FALSE}
var
  aChar: char;
begin
  if Context.ContextGetCurrChar <> '''' then   //first character not valid
    exit(False);        //it is not constant string
  Context.GetCharAnMoveCursor;     //take the character
  s := '';         //start to accumulate
  //search until find end of string
  while not Context.atEndContext do
  begin
    aChar := Context.GetCharAnMoveCursor;
    if aChar = '''' then
      exit(True)//end character
    else
      s += aChar;
  end;
  //it came to the end of the file
  GenError(msg.Get('EndOfStringNotFound'));
  Result := True;    //indicates that string was found
end;

function TEvalExpres.ContextIsIdentifier(var s: string): boolean;
  {Take an identifier, which must correspond to a variable.}
begin
  if not (Context.ContextGetCurrChar in ['a'..'z', 'A'..'Z']) then
    //first character not valid
    exit(False);//it is not constant string
  s := '';         //start to accumulate
  //search until you find the end of the identifier
  while not Context.atEndContext and (Context.ContextGetCurrChar in
      ['a'..'z', 'A'..'Z']) do
    s += Context.GetCharAnMoveCursor;
  Result := True;    //indicates that identifier was found
end;

function TEvalExpres.ContextGetOperator: string;
{Take an operator in the position of the current context. If you do not find
  returns empty string and does not take characters, except leading spaces.}
begin
  ContextGetOperator := '';
  Context.CapBlancos;     //removes initial targets
  case Context.ContextGetCurrChar of //complete with operator of more characters
    '+': Result := Context.GetCharAnMoveCursor;
    '-': Result := Context.GetCharAnMoveCursor;
    '*': Result := Context.GetCharAnMoveCursor;
    '/': Result := Context.GetCharAnMoveCursor;
    '\': Result := Context.GetCharAnMoveCursor;
    '%': Result := Context.GetCharAnMoveCursor;
    '^': Result := Context.GetCharAnMoveCursor;
  end;
end;

function TEvalExpres.hierarchyOperator(oper: string): integer;
  //Returns the hierarchy of a Show operator technical documentation.
begin
  case oper of
    '+', '-'{, '|', '&'}: hierarchyOperator := 5;
    '*', '/', '\', '%': hierarchyOperator := 6;
    '^'{, '++', '--', '+=', '-=', '*=', '/='}: hierarchyOperator := 8;
    else
      hierarchyOperator := 0;
  end;
end;

function TEvalExpres.Evaluate(Op1: TEvExExpression; opr: string;
  Op2: TEvExExpression): TEvExExpression;
  // Returns the result and type of an operation
begin
  ErrorStr := '';
  Evaluate.cat := CAT_EXPR;
  case opr of
    '': Evaluate := Op1;//Without operator And it is assumed without Op2
      //there is nothing to do, it is already in the place

    '+': Evaluate.valNum := Op1.valNum + Op2.valNum;//Force Evaluate.operand: = OP_NUM

    '-': Evaluate.valNum := Op1.valNum - Op2.valNum;
    '*': Evaluate.valNum := Op1.valNum * Op2.valNum;
    '/': if Op2.valNum = 0 then
        GenError(msg.Get('CantDevideByZero'))
      else
        Evaluate.valNum := Op1.valNum / Op2.valNum//error
      ;
    '\': if Op2.valNum = 0 then
        GenError(msg.Get('CantDevideByZero'))
      else
        Evaluate.valNum := round(Op1.valNum) div round(Op2.valNum)//error
      ;
    '%': if Op2.valNum = 0 then
        GenError(msg.Get('CantDevideByZero'))
      else
        Evaluate.valNum := round(Op1.valNum) mod round(Op2.valNum)//error
      ;
    '^': if (Op2.valNum = 0) and (Op2.valNum = 0) then
        GenError(msg.Get('CantEvaluate0of0'))
      else
        Evaluate.valNum := power(Op1.valNum, Op2.valNum)//error
      ;
    else
    begin
      GenError(msg.Get('NoOperatorRecognized') + opr, Context.col);
      Exit;
    end;
  end;
  //Complete fields to evaluate
  Evaluate.txt := Op1.txt + opr + Op2.txt;   //expression text
end;

function TEvalExpres.GetOperand: TEvExExpression;
{Take an operand in the current position of the content. If not
the operand or is wrong, generates Error.}
var
  aChain: string;//bug not used
  num: single;
  expr: TEvExExpression;
  i: integer;
begin
  Context.CapBlancos;
  if Context.atEndContext then
  begin
    Result.txt := '';
    exit;
  end;
  if ContextCurrCharIsNumber(num) then
  begin
    if ErrorStr <> '' then
      exit;
    Result.txt := '#';
    Result.valNum := num;
    Result.cat := CAT_CONST;
  end
  else if ContextIsIdentifier(aChain) then
  begin
    for i := 0 to nVars - 1 do
      if vars[i].number = aChain then
      begin
        Result.txt := '$';   //indicates number
        Result.valNum := vars[i].Value;
        Result.cat := CAT_VAR;
        exit;
      end;
    //It is not variable or function.
    GenError(msg.Get('UnknownFuncOrVar') + aChain, Context.col);
    Result.txt := '';
  end
  else if ContextIsString(aChain) then
  begin   //Constant chain
    Result.txt := 'A';   //indicates chain
    Result.valTxt := aChain;    //Fix Type to number
    Result.cat := CAT_CONST;
  end
  else if Context.ContextGetCurrChar = '(' then
  begin
    Result := CogExpresionEnclosed;
    exit;
  end
  else
  begin
    Result.txt := '';
    exit;
  end;
end;

function TEvalExpres.GetExpresion(hierarchy: integer): TEvExExpression;
{
Take a complete expression, in the current position of the content
  If you can not find an expression, it generates an error
}
var
  Op1, Op2: TEvExExpression;
  opr, opr2: string;
  hieOpr, hieOpr2: integer;
  pos1, pos2: integer;
begin
  Context.CapBlancos;
  Op1 := GetOperand;  //error
  if ErrorStr <> '' then
    exit;
  opr := ContextGetOperator;
  if opr = '' then
  begin
    Result := Op1;
    Exit;
  end;
  hieOpr := hierarchyOperator(opr);     //There is an operator, take your hierarchy
  if hieOpr <= hierarchy then
  begin
    Result := Op1;
    Exit;
  end;
  while opr <> '' do
  begin
    pos1 := Context.col;
    Op2 := GetOperand;
    if ErrorStr <> '' then
      exit;
    pos2 := Context.col;
    opr2 := ContextGetOperator;
    if opr2 <> '' then
    begin
      hieOpr2 := hierarchyOperator(opr2);
      if hieOpr2 <= hierarchy then
      begin
        Context.col := pos2;   //before catching the operator
        Result := Evaluate(Op1, opr, Op2);
        Exit;
      end;
      if hieOpr2 > hieOpr then
      begin    //and it is of higher hierarchy, it goes back
        Context.col := pos1;        //  go back
        Op2 := GetExpresion(hieOpr);        //evaluate first
        opr2 := ContextGetOperator;    //update the next operator
      end;
    end;

    Op1 := Evaluate(Op1, opr, Op2);    //evaluate result
    if ErrorStr <> '' then
      exit;
    opr := opr2;
    hieOpr := hierarchyOperator(opr);    //Update previous operator
  end;
  Result := Op1;
  Result.cat := CAT_EXPR;
end;

function TEvalExpres.CogExpresionEnclosed: TEvExExpression;
  {Take an expression that should be enclosed in parentheses. It can generate error}
begin
  if not ContextCurrCharIs('(') then
    exit;
  Result := GetExpresion(0);
  if ErrorStr <> '' then
    exit;
  Context.CapBlancos;
  if not ContextCurrCharIs(')') then
    exit;
end;

function TEvalExpres.AssignNumberToVar(const VarName: string; Value: double): integer;
{Assign a numerical value to a variable. If it does not exist, create it.
Returns the index of the variable in the array vasr [].}
var
  i: integer;
begin
  //Search variable
  for i := 0 to nVars - 1 do
    if vars[i].number = VarName then
    begin
      vars[i].Value := Value;
      exit(i);
    end;
  //It was not found, it must be CreateIt
  Inc(nVars);
  setlength(vars, nVars);
  Result := nVars - 1;
  vars[Result].number := VarName;
  vars[Result].Value := Value;
end;

function TEvalExpres.EvaluateString(line: string): TEvExExpression;
  {Evaluate the expression that is contained in "line"}
begin
  ErrorStr := '';
  Initiate(line);
  Result := GetExpresion(0);
  if ErrorStr <> '' then
    exit;
  //Check if you finished processing the entire line
  if not Context.atEndContext then
    GenError(msg.Get('SyntaxError'));
end;

procedure TEvalExpres.GenError(argMsg: string; col: integer = -1);
begin
  ErrorStr := argMsg;
  if col = -1 then
    ErrorCol := Context.col
  else
    ErrorCol := col;
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
