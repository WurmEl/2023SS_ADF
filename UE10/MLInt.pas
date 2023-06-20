(*MLStr:                                               MiniLib V.4, 2004
  -----
  Class MLInt, the MiniLib integer class.
========================================================================*)
UNIT MLInt;

INTERFACE

  USES
    MLObj;

  TYPE

(*=== class MLInt ===*)

    MLInteger = ^MLIntObj;
    MLIntObj = OBJECT(MLObjectObj)

      pasInt: INTEGER;

      CONSTRUCTOR Init(i: INTEGER);
      DESTRUCTOR Done; VIRTUAL;

(*--- overridden methods ---*)

      FUNCTION AsString: STRING; VIRTUAL;
      (*returns MLInt value as STRING*)

      FUNCTION IsEqualTo(o: MLObject): BOOLEAN; VIRTUAL;
      (*returns SELF.pasInt = o^.pasInt*)

      FUNCTION IsLessThan(o: MLObject): BOOLEAN; VIRTUAL;
      (*returns SELF.pasInt < o^.pasInt*)

(*--- new methods ---*)

      FUNCTION AsInteger: INTEGER;

    END; (*OBJECT*)


  FUNCTION NewMLInt(i: INTEGER): MLInteger;


(*======================================================================*)
IMPLEMENTATION

  USES
    MetaInfo;


  FUNCTION NewMLInt(i: INTEGER): MLInteger;
    VAR
      mli: MLInteger;
  BEGIN
    New(mli, Init(i));
    NewMLInt := mli;
  END; (*NewMLInt*)


  CONSTRUCTOR MLIntObj.Init(i: INTEGER);
  BEGIN
    INHERITED Init;
    Register('MLInt', 'MLObject');
    pasInt := i;
  END; (*MLIntObj.Init*)

  DESTRUCTOR MLIntObj.Done;
  BEGIN
    INHERITED Done;
  END; (*MLIntObj.Done*)


(*--- overridden methods ---*)

  FUNCTION MLIntObj.AsString: STRING;
  var
    intStr: STRING;
  BEGIN
    Str(pasInt, intStr);
    AsString := intStr;
  END; (*MLIntObj.AsString*)
    
  FUNCTION MLIntObj.IsEqualTo(o: MLObject): BOOLEAN;
  BEGIN
    IF o^.IsA('MLInt') THEN
      IsEqualTo := SELF.pasInt = MLInteger(o)^.pasInt
    ELSE BEGIN
      Error('invalid IsEqualTo comparison of MLInt to ' + o^.Class);
      IsEqualTo := FALSE;
    END; (*ELSE*)
  END; (*MLIntObj.IsEqualTo*)

  FUNCTION MLIntObj.IsLessThan(o: MLObject): BOOLEAN;
  BEGIN
    IF o^.IsA('MLInt') THEN
      IsLessThan := SELF.pasInt < MLInteger(o)^.pasInt
    ELSE BEGIN
      Error('invalid IsLessThan comparison of MLInt to ' + o^.Class);
      IsLessThan := FALSE;
    END; (*ELSE*)
  END; (*MLIntObj.IsLessThan*)


(*--- new methods ---*)

  FUNCTION MLIntObj.AsInteger: INTEGER;
  BEGIN
    AsInteger := pasInt;
  END;

END. (*MLStr*)


 

