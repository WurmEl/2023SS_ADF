program VectorTests;

uses Vector;

var
  v: Vec;
  i: Integer;
begin
  // Initialize an empty vector
  InitVector(v);

  // Test adding elements
  WriteLn('Test adding elements:');
  writeln('0 elements - size: ', Size(v), ', capacity: ', Capacity(v));
  Add(v, 1);
  writeln('1 element - size: ', Size(v), ', capacity: ', Capacity(v));
  Add(v, 2);
  writeln('2 elements - size: ', Size(v), ', capacity: ', Capacity(v));
  Add(v, 3);
  writeln('3 elements - size: ', Size(v), ', capacity: ', Capacity(v));
  Add(v, 4);
  writeln('4 elements - size: ', Size(v), ', capacity: ', Capacity(v));
  Add(v, 5);
  writeln('5 elements - size: ', Size(v), ', capacity: ', Capacity(v));
  WriteLn;
  WriteLn;

  // Test getting and setting elements
  WriteLn('Test getting and setting elements:');
  WriteLn('Element at position 2: ', ElementAt(v, 2)); // should print 3
  SetElementAt(v, 2, 6);
  WriteLn('Element at position 2 after setting it to 6: ', ElementAt(v, 2)); // should print 6
  WriteLn;
  WriteLn;

  // Test removing elements
  WriteLn('Test removing elements:');
  WriteLn('Vector size before removing an element: ', Size(v)); // should print 5
  RemoveElementAt(v, 2);
  WriteLn('Vector size after removing an element: ', Size(v)); // should print 4
  WriteLn('Element at position 2 after removing element at position 2: ', ElementAt(v, 2)); // should print 4
  WriteLn;
  WriteLn;

  // Test disposing of vector
  DisposeVector(v);
end.
