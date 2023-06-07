unit personUnit;

interface

type
  Person = ^PersonObj;
  PersonObj = object
  protected
    name: string;
    age: integer;

  public
    constructor init;
    constructor init(name: string); overload;
    constructor init(name: string; age:integer); overload;
    destructor done; virtual;

    function SayHello: string; virtual;
  end;

implementation

constructor PersonObj.init;
begin
  name := '';
  age := 0;
end;

constructor PersonObj.init(name: string); overload;
begin
  self.name := name;
  self.age := 0;
end;

constructor PersonObj.init(name: string; age: integer); overload;
begin
  self.name := name;
  self.age := age;
end;

destructor PersonObj.done;
begin
  // empty
end;

function PersonObj.SayHello: string;
var
  ageStr: string;
begin
  Str(age, ageStr);
  SayHello := 'Hello, my name is ' + name + ' and im ' + ageStr + ' years old';
end;

end.
