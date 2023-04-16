(* Vector:                                            Elias Wurm, 2023-04-16 *)
(* ------                                                                    *)
(* provides a dynamic array implementation. It allows the creation,          *)
(* manipulation, and destruction of an array of integers. The array grows    *)
(* dynamically as elements are added to it, without the need for manual      *)
(* reallocation, by doubling the capacity whenever an overflow happens.      *)                                                             
(* ========================================================================= *)
unit Vector;

interface

const
  MAX_CAPACITY = MaxInt;

type
  PIntArray = ^IntArray;
  IntArray = array[0..MAX_CAPACITY] of Integer; // max array size is MaxInt (32767)
  Vec = record
    data: PIntArray;
    count: Integer;
    capacity: Integer;
  end;

procedure InitVector(var v: Vec);
procedure DisposeVector(var v: Vec);
procedure Add(var v: Vec; val: Integer);
procedure SetElementAt(var v: Vec; pos: Integer; val: Integer);
function ElementAt(v: Vec; pos: Integer): Integer;
procedure RemoveElementAt(var v: Vec; pos: Integer);
function Size(v: Vec): Integer;
function Capacity(v: Vec): Integer;

implementation

uses
  SysUtils;

procedure InitVector(var v: Vec);
begin
  v.count := 0;
  v.capacity := 1;
  GetMem(v.data, v.capacity * SizeOf(Integer));
  if v.data = nil then
  begin
    WriteLn('Error: Heap overflow.');
    Halt(1);
  end;
end;

procedure DisposeVector(var v: Vec);
begin
  FreeMem(v.data, v.capacity * SizeOf(Integer));
end;

procedure GrowVector(var v: vec);
var
  newCapacity: Integer;
  newData: PIntArray;
  i: Integer;
begin
  newCapacity := v.capacity * 2;
  if newCapacity > MAX_CAPACITY then
  begin
    WriteLn('Error: Capacity exceeds maximum value.');
    Halt(1);
  end;
  GetMem(newData, newCapacity * SizeOf(Integer));
  for i := 0 to v.count - 1 do
    newData^[i] := v.data^[i];
  FreeMem(v.data, v.capacity * SizeOf(Integer));
  v.data := newData;
  v.capacity := newCapacity;
end;

procedure Add(var v: Vec; val: Integer);
begin
  if v.count = v.capacity then
    GrowVector(v);
  v.data^[v.count] := val;
  Inc(v.count);
end;

procedure SetElementAt(var v: Vec; pos: Integer; val: Integer);
begin
  if (pos < 0) or (pos >= v.count) then
  begin
    WriteLn('Error: Index out of range.');
    Halt(1);
  end;
  v.data^[pos] := val;
end;

function ElementAt(v: Vec; pos: Integer): Integer;
begin
  if (pos < 0) or (pos >= v.count) then
  begin
    WriteLn('Error: Index out of range.');
    Halt(1);
  end;
  ElementAt := v.data^[pos];
end;

procedure RemoveElementAt(var v: Vec; pos: Integer);
var
  i: Integer;
begin
  if (pos < 0) or (pos >= v.count) then
  begin
    WriteLn('Error: Index out of range.');
    Halt(1);
  end;
  for i := pos to v.count - 2 do
    v.data^[i] := v.data^[i + 1];
  Dec(v.count);
end;

function Size(v: Vec): Integer;
begin
  Size := v.count;
end;

function Capacity(v: Vec): Integer;
begin
  Capacity := v.capacity;
end;

end.
