unit studentUnit;

interface
uses personUnit;

type
  Student = ^StudentObj;
  StudentObj = object(PersonObj)
  private
    id: integer;

  public
    constructor init(name: string; age:integer; id:integer); overload;
    destructor done; virtual;

    procedure PartyHard;
    function SayHello: string; virtual;
  end;

implementation

constructor StudentObj.init(name: string; age:integer; id:integer); overload;
begin
  self.name := name;
  self.age := age;
  self.id := id;
end;

destructor StudentObj.done;
begin
  //empty
end;

function StudentObj.SayHello: string;
var
  idStr: string;
begin
  Str(id, idStr);
  SayHello := inherited SayHello + ' and my id is ' + idStr;
end;

procedure StudentObj.PartyHard;
begin
  writeln('uzuzuz');
end;

end.
