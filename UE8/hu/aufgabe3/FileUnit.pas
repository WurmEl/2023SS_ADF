unit FileUnit;

interface

uses EntityUnit;

type
  FilePtr = ^FileObj;
  FileObj = object(EntityObj)
    size: longint;

    constructor Init(name: string; size: longInt);
    destructor Done; virtual;

    function AsString: string; virtual;
  end;

function NewFile(name: string; size: longInt): FilePtr;

implementation

function NewFile(name: string; size: longInt): FilePtr;
var
  f: FilePtr;
begin
  New(f, Init(name, size));
  NewFile := f;
end;

constructor FileObj.Init(name: string; size: longInt);
begin
  self.size := size;
  inherited Init(name, FileType);
end;

destructor FileObj.Done;
begin
  inherited Done;
end;

function FileObj.AsString: string;
var
  sizeStr: string;
begin
  Str(size, sizeStr);
  AsString := inherited + ' ,size: ' + sizeStr;
end;

end.
