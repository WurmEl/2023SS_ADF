unit EntityUnit;

interface

uses sysUtils;

type
  EntityType = (FileType, FolderType);

  EntityPtr = ^EntityObj;
  EntityObj = object
  public
    constructor Init(name: string; entityType: EntityType);
    destructor Done; virtual;

    function AsString: string; virtual;

  public
    name: string;
    entityType: EntityType;
    dateModified: TDateTime;
  end;

implementation

constructor EntityObj.Init(name: string; entityType: EntityType);
begin
  self.name := name;
  self.entityType := entityType;
  dateModified := Now;
end;

destructor EntityObj.Done;
begin
end;

function EntityObj.AsString: string;
var
  typeStr: string;
begin
  case entityType of
    FileType: typeStr := 'file';
    FolderType: typeStr := 'folder';
  else
    typeStr := 'undefiend';
  end;

  AsString := 'name: ' + name + ', type: ' + typeStr + ', dateModified: ' + DateTimeToStr(dateModified);
end;

end.
