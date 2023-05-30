program TestFS;

uses
  EntityUnit,
  FileUnit,
  FolderUnit;

var
  file1, file2, file3: FilePtr;
  folder1, folder2, folder3: FolderPtr;

begin
  // Create files
  file1 := NewFile('file1.txt', 100);
  file2 := NewFile('file2.txt', 200);
  file3 := NewFile('file3.txt', 300);

  // Create folders
  folder1 := NewFolder('folder1');
  folder2 := NewFolder('folder2');
  folder3 := NewFolder('folder3');

  // Add files to folder1
  folder1^.Add(file1);
  folder1^.Add(file2);

  // Add folder1 and file3 to folder2
  folder2^.Add(folder1);
  folder2^.Add(file3);

  // Add folder2 to folder3
  folder3^.Add(folder2);

  // Print the initial folder structure
  writeln('Initial Folder Structure:');
  writeln(folder3^.AsString); writeln;

  // Delete file2 from folder1
  folder1^.Delete('file2.txt');

  // Print the updated folder structure after removing file2
  writeln('Folder Structure after Removing file2:');
  writeln(folder3^.AsString); writeln;

  // Delete folder1 from folder2
  folder2^.Delete('folder1');

  // Print the updated folder structure after deleting folder1
  writeln('Folder Structure after Deleting folder1:');
  writeln(folder3^.AsString); writeln;

  // Delete file3 from folder2
  folder2^.Delete('file3.txt');

  // Print the updated folder structure after removing file3
  writeln('Folder Structure after Removing file3:');
  writeln(folder3^.AsString); writeln;


  // Create folder1 and file1 again and add file1 to folder1
  folder1 := NewFolder('folder1');
  file1 := NewFile('file1.txt', 100);
  folder1^.Add(file1);

  writeln('Folder Structure after create folder1 and file1 again and add file1 to folder1: ');
  writeln(folder1^.AsString); writeln;

  // Move file1 from folder1 to folder2
  folder1^.Move('file1.txt', folder2);

  // Print the updated folder structure after moving file1
  writeln('Folder Structure after Moving file1:');
  writeln(folder3^.AsString); writeln;
  writeln(folder1^.AsString); writeln;

  // Delete the folder and file objects
  Dispose(folder3, Done);
  Dispose(folder1, Done);
end.
