unit uToLang;

{$mode objfpc}

interface

uses
  Classes, SysUtils, IniFiles, toLang_defaults;

type
  
  { tuToLang }

  tuToLang = class
  private
    type TStrN = string[32];
    type TStrM = ansistring;
  var
    ArrKeys: array of TStrN;
    ArrMessages: array of TStrM;
    instance: string;
    class var filecreated: boolean;
  const
    created: boolean = False;

    function fromFile(fn: string): boolean;
    procedure wrMissing(msg: string);
  public
    constructor Create(target: string = ''; argInst: string = 'not set'); overload;
    function Get(Name: TStrN): TStrM;
    procedure Add(Name: TStrN; message: TStrM);
    procedure setMessages(target:string='');
    var logMissing: boolean;
  end;

implementation

function tuToLang.Get(Name: TStrN): TStrM;
var
  key: integer;
  found: boolean;
begin
  if not self.created then
    exit('');
  Result := '';
  found := False;
  key := 0;
  Name := LowerCase(Name);
  while key <= high(self.ArrKeys) do
  begin
    if ArrKeys[key] = Name then
    begin
      found := True;
      Break;
    end;
    key := key + 1;
  end; //while key <= high(self.ArrKeys)

  if found then
  begin
    if high(ArrMessages) >= key then
    begin
      if ArrMessages[key] <> '' then
        exit(ArrMessages[key])
      else
        exit(ArrKeys[key]);
    end;
  end//ArrKeys[key] = Name
  else
  begin
    If logMissing Then begin
       wrMissing(Name);
    end;
    exit('');
  end;

end;

procedure tuToLang.Add(Name: TStrN; message: TStrM);
var
  key: integer;
begin

  if not self.created then
    exit;
  key := 0;
  Name := LowerCase(Name);
  //if exists then nothing to do
  self.Get(Name);
  if self.Get(Name) <> '' then
    exit;
  SetLength(self.ArrKeys, Length(self.ArrKeys) + 1);
  SetLength(self.ArrMessages, Length(self.ArrKeys) + 1);
  key := high(self.ArrKeys);
  self.ArrKeys[key] := Name;
  self.ArrMessages[key] := message;
end;

function tuToLang.fromFile(fn: string): boolean;
var
  section, iniKey: string;
  INI_obj: TINIFile;
  Messages: TStringList;
  key: integer;
begin
  if not self.created then
    exit(False);
  if not FileExists(fn) then
  begin
    exit(False);
  end;
  INI_obj := TINIFile.Create(fn);
  Messages := TStringList.Create;
  section := 'Messages';
  try
    INI_obj.ReadSection(section, Messages);
    key := 0;
    if Messages.Count = 0 then
      exit(False);
    while key < Messages.Count do
    begin
      iniKey := Messages[key];
      self.Add(iniKey, INI_obj.ReadString(section, iniKey, ''));
      key := key + 1;
    end;
  finally
    INI_obj.Free;
    Messages.Free;
  end;
  exit(True);
end;

procedure tuToLang.wrMissing(msg: string);
Var f : text; fname: string;

begin
  fname := self.UnitName+'.'+self.ClassName+'_Get_missing.md';
  Assign (f,fname);
  msg := msg + ' (instanse '+self.instance+')';
  if not self.filecreated then begin
    Rewrite (f); { file is opened for write, and emptied }
    Writeln (f,'# Set "logMissing" to false to get rid of this file.');
    Writeln (f,'## Unit '+ self.UnitName+'. Class '+self.ClassName+'.');
    Writeln (f,'## Unable to find following:');
    Writeln (f,'* '+msg);
    close (f);
    self.filecreated := true;
  end
  else begin
    Append(f); { appending to file}
    Writeln (f,'* '+msg);
    close (f);
  end;
end;

procedure tuToLang.setMessages(target:string='');
begin
  if not self.created then
    exit;
  if target = '' then
  begin //add defaults
    toLang_defaults.createDefaults(self);
  end
  else
  begin
    target:=SetDirSeparators(target);
    if not self.fromFile(target + '.ini') then
      toLang_defaults.createDefaults(self);
  end;
end;

constructor tuToLang.Create(target: string = ''; argInst: string = 'not set');
begin
  inherited Create();
  SetLength(self.ArrKeys, 0);
  SetLength(self.ArrMessages, 0);
  self.created := True;
  self.setMessages(target);
  self.instance:=argInst;;
end;


initialization

end.
