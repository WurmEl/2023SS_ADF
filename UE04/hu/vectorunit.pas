(* VectorUnit:                                        Elias Wurm, 2023-04-16 *)
(* ------                                                                    *)
(* provides a dynamic array implementation. It allows the creation,          *)
(* manipulation, and destruction of an array of integers. The array grows    *)
(* dynamically as elements are added to it, without the need for manual      *)
(* reallocation, by doubling the capacity whenever an overflow happens.      *)                                                             
(* ========================================================================= *)
unit VectorUnit;

interface

type
  Vector = Pointer;

procedure InitVector(var v: Vector);
procedure DisposeVector(var v: Vector);
procedure Add(var v: Vector; val: Integer);
procedure SetElementAt(var v: Vector; pos: Integer; val: Integer);
function ElementAt(v: Vector; pos: Integer): Integer;
procedure RemoveElementAt(var v: Vector; pos: Integer);
function Size(v: Vector): Integer;
function Capacity(v: Vector): Integer;

implementation

// using MaxInt for the intArray here so i dont have to disable rangecheck 
// error everytime i try to access the array if i used array[0..0] instead. 
// There is no allocation of more memory as a consequence because i manually  
// set the memory with capacity * SizeOf(Integer) so there shouldnt be a  
// problem, also i choose MaxInt because my count and capacity are int anyway
type
  IntArray = array[0..MaxInt] of Integer;
  PIntArray = ^IntArray;
  VecRec = record
    data: PIntArray;
    count: Integer;
    capacity: Integer;
  end;
  PVector = ^VecRec;

procedure InitVector(var v: Vector);
var
  pv: PVector;
begin
  new(pv);
  pv^.count := 0;
  pv^.capacity := 1;
  GetMem(pv^.data, pv^.capacity * SizeOf(Integer));
  if pv^.data = nil then
  begin
    WriteLn('Error: Heap overflow.');
    Halt(1);
  end;
  v := pv;
end;

procedure DisposeVector(var v: Vector);
var
  pv: PVector;
begin
  pv := PVector(v);
  FreeMem(pv^.data, pv^.capacity * SizeOf(Integer));
  Dispose(v);
end;

procedure GrowVector(var v: VecRec);
var
  newCapacity: longInt;
  newData: PIntArray;
  i: Integer;
begin
  newCapacity := v.capacity * 2;
  if newCapacity >= MaxInt then
  begin
    WriteLn('Error: Vector overflow.');
    Halt(1);
  end;
  GetMem(newData, newCapacity * SizeOf(Integer));
  for i := 0 to v.count - 1 do
    newData^[i] := v.data^[i];
  FreeMem(v.data, v.capacity * SizeOf(Integer));
  v.data := newData;
  v.capacity := newCapacity;
end;

procedure Add(var v: Vector; val: Integer);
var
  pv: PVector;
begin
  pv := PVector(v);
  if pv^.count = pv^.capacity then
    GrowVector(pv^);
  pv^.data^[pv^.count] := val;
  Inc(pv^.count);
end;

procedure SetElementAt(var v: Vector; pos: Integer; val: Integer);
var
  pv: PVector;
begin
  pv := PVector(v);
  if (pos < 0) or (pos >= pv^.count) then
  begin
    WriteLn('Error: Index out of range.');
    Halt(1);
  end;
  pv^.data^[pos] := val;
  
end;

function ElementAt(v: Vector; pos: Integer): Integer;
var
  pv: PVector;
begin
  pv := PVector(v);
  if (pos < 0) or (pos >= pv^.count) then
  begin
    WriteLn('Error: Index out of range.');
    Halt(1);
  end;
  ElementAt := pv^.data^[pos];
end;

procedure RemoveElementAt(var v: Vector; pos: Integer);
var
  i: Integer;
  pv: PVector;
begin
  pv := PVector(v);
  if (pos < 0) or (pos >= pv^.count) then
  begin
    WriteLn('Error: Index out of range.');
    Halt(1);
  end;
  for i := pos to pv^.count - 2 do
    pv^.data^[i] := pv^.data^[i + 1];
  Dec(pv^.count);
end;

function Size(v: Vector): Integer;
var
  pv: PVector;
begin
  pv := PVector(v);
  Size := pv^.count;
end;

function Capacity(v: Vector): Integer;
var
  pv: PVector;
begin
  pv := PVector(v);
  Capacity := pv^.capacity;
end;

end.
