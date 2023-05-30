unit FolderUnit;

interface

uses EntityUnit, FileUnit, StringBuilderUnit;

const
  MAX_FOLDER_SIZE = 50;

type
  FolderPtr = ^FolderObj;
  FolderObj = object(EntityObj)
  public
    constructor Init(name: string);
    destructor Done; virtual;

    procedure Add(entity: EntityPtr);
    function Remove(name: STRING): EntityPtr;
    procedure Delete(name: STRING);
    procedure Move(name: string; destination: FolderPtr);
    function Size: longInt;

    function AsString: string; virtual;
  private
    children: array[0..MAX_FOLDER_SIZE] of EntityPtr;
    count: integer;

    function FindEmptySlot: integer;
    function FindIndexByName(name: string): integer;
  end;

function NewFolder(name: string): FolderPtr;

implementation

function NewFolder(name: string): FolderPtr;
var
  f: FolderPtr;
begin
  New(f, Init(name));
  NewFolder := f;
end;

constructor FolderObj.Init(name: string);
begin
  count := 0;
  inherited Init(name, FolderType);
end;

destructor FolderObj.Done;
var
  i: integer;
begin
  inherited Done;
  for i := Low(children) to High(children) do
    if (children[i] <> nil) then
      Dispose(children[i], Done);
end;

procedure FolderObj.Add(entity: EntityPtr);
begin
  if(count = MAX_FOLDER_SIZE) then
  begin
    writeln('ERROR: Max. folder size reached!');
    Halt;
  end;

  children[FindEmptySlot] := entity;
  Inc(count);
end;

function FolderObj.Remove(name: STRING): EntityPtr;
var
  i: Integer;
begin
  i := FindIndexByName(name);
  if i >= 0 then
  begin
    Remove := children[i];
    children[i] := nil;
    Dec(count);
  end else
    Remove := nil;
end;

procedure FolderObj.Delete(name: STRING);
var
  i: Integer;
begin
  i := FindIndexByName(name);
  if i >= 0 then
  begin
    Dispose(children[i], Done);
    children[i] := nil;
    Dec(count);
  end;
end;

procedure FolderObj.Move(name: string; destination: FolderPtr);
var
  entity: EntityPtr;
begin
  entity := Remove(name);
  if entity <> nil then
    destination^.Add(entity);
end;
    
function FolderObj.Size: longInt;
var
  i, sum: longInt;
begin
  sum := 0;
  for i := Low(children) to High(children) do
    if (children[i] <> nil) then
      case children[i]^.entityType of
        FileType: sum := sum + FilePtr(children[i])^.size;
        FolderType: sum := sum + FolderPtr(children[i])^.Size;
      end;
  Size := sum;
end;

function FolderObj.FindEmptySlot: Integer;
var
  i: Integer;
begin
  for i := Low(children) to High(children) do
    if children[i] = nil then
    begin
      FindEmptySlot := i;
      Exit;
    end;
end;

function FolderObj.FindIndexByName(name: string): integer;
var
  i: Integer;
begin
  if(count = 0) then
  begin FindIndexByName := -1; Exit; end;

  for i := Low(children) to High(children) do
    if (children[i] <> nil) and (children[i]^.name = name) then
    begin
      FindIndexByName := i;
      Exit;
    end;
  FindIndexByName := -1;
end;

function FolderObj.AsString: string;
var
  i: integer;
  strBuilder: StringBuilderPtr;
begin
  strBuilder := NewStringBuilder;

  strBuilder^.AppendStr(inherited AsString);
  strBuilder^.AppendStr(', childrenAmount:');
  strBuilder^.AppendInt(count);
  strBuilder^.AppendStr(', size:');
  strBuilder^.AppendLongInt(Size);
  strBuilder^.AppendStr(', children:');

  for i := Low(children) to High(children) do
    if children[i] <> nil then
    begin
      strBuilder^.AppendLine;
      strBuilder^.AppendStr('  ');
      strBuilder^.AppendStr(children[i]^.asString);
    end;

  AsString := strBuilder^.AsString;
  Dispose(strBuilder, Done);
end;

end.
