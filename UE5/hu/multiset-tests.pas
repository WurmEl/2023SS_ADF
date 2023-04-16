program StrMSetTests;

uses Multiset;

var
  ms: StrMSet;
begin
  // Initialize an empty multiset
  InitStrMSet(ms);

  // Test inserting elements
  Insert(ms, 'apple');
  Insert(ms, 'banana');
  Insert(ms, 'apple');
  Insert(ms, 'cherry');
  Insert(ms, 'banana');
  Insert(ms, 'cherry');
  
  // Test counting elements
  WriteLn('Count of ''apple'': ', Count(ms, 'apple')); // should print 2
  WriteLn('Count of ''banana'': ', Count(ms, 'banana')); // should print 2
  WriteLn('Count of ''cherry'': ', Count(ms, 'cherry')); // should print 2
  WriteLn('Count of ''grape'': ', Count(ms, 'grape')); // should print 0
  
  // Test counting unique elements
  WriteLn('Number of unique elements: ', CountUnique(ms)); // should print 3
  
  // Test checking if elements are present
  WriteLn('Contains ''apple'': ', Contains(ms, 'apple')); // should print True
  WriteLn('Contains ''banana'': ', Contains(ms, 'banana')); // should print True
  WriteLn('Contains ''cherry'': ', Contains(ms, 'cherry')); // should print True
  WriteLn('Contains ''grape'': ', Contains(ms, 'grape')); // should print False
  
  // Test removing elements
  Remove(ms, 'apple');
  WriteLn('Count of ''apple'': ', Count(ms, 'apple')); // should print 1
  Remove(ms, 'banana');
  WriteLn('Count of ''banana'': ', Count(ms, 'banana')); // should print 1
  Remove(ms, 'cherry');
  WriteLn('Count of ''cherry'': ', Count(ms, 'cherry')); // should print 1
  
  // Test checking if multiset is empty
  WriteLn('Is empty: ', IsEmpty(ms)); // should print False
  
  // Test disposing of multiset
  DisposeStrMSet(ms);
  WriteLn('Is empty: ', IsEmpty(ms)); // should print True
end.
