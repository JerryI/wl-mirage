<<"SymbolicC`";

MakeCL /: MakeCL[IR`Function[type_, name_String, args_List, body_]] := CFunction[MakeCL[type], name, {# // Type // MakeCL, # // IR} &/@ args, MakeCL[body]]

MakeCL /: MakeCL[a_String] := a 

MakeCL /: MakeCL[IR`Type`Real] := "float"
MakeCL /: MakeCL[IR`Type`Complex] := "cfloat"

(* standard types *)
MakeCL /: MakeCL[IR`Define[IR`Entity[name_String, _, IR`Type`Real]]] := CDeclare["float", name]
MakeCL /: MakeCL[IR`Define[IR`Entity[name_String, _, IR`Type`Complex]]] := CDeclare["cfloat", name]

MakeCL /: MakeCL[IR`Define[IR`Entity[name_String, _, IR`Type`Integer]]] := CDeclare["int", name]

MakeCL /: MakeCL[IR`Define[IR`Entity[name_String, _, IR`Type`Complex`Re]]] := CDeclare["float", name]
MakeCL /: MakeCL[IR`Define[IR`Entity[name_String, _, IR`Type`Complex`Im]]] := CDeclare["float", name]

(* blocks *)

MakeCL /: MakeCL[IR`Block[l_List]] := CBlock[ MakeCL[#] &/@ l ]

MakeCL /: MakeCL[IR`List[l_List]] := CStatement[MakeCL[# // Flatten]] &/@ Flatten[l] // Flatten

MakeCL /: MakeCL[IR`Entity[data_, _, _]] := MakeCL[data]
MakeCL /: MakeCL[IR`Entity[IR`Constant, value_, IR`Type`Real]] := value

(* complex constants *)

MakeCL /: MakeCL[IR`Entity[IR`Constant, value_, IR`Type`Complex`Re]] := Re[value]
MakeCL /: MakeCL[IR`Entity[IR`Constant, value_, IR`Type`Complex`Im]] := Im[value]

MakeCL /: MakeCL[IR`Entity[IR`Constant, value_, IR`Type`Integer]] := value

(* REALS: ASSIGMENT to VARIABLES *)
MakeCL /: MakeCL[IR`Assign[IR`Entity[varA_String, _, IR`Type`Real], IR`Entity[varB_String, _, IR`Type`Real]]] := CAssign[varA, varB]

(* REALS: ASSIGMENT to Real operators????? *)
MakeCL /: MakeCL[IR`Assign[IR`Entity[varA_String, _, IR`Type`Real], IR`Entity[varB_, _, IR`Type`Real]]] := CAssign[varA, IR`Entity[varB, Null, IR`Type`Real] // MakeCL]

(* REALS: ASSIGMENT to CONSTANTS *)
MakeCL /: MakeCL[IR`Assign[IR`Entity[varA_String, _, IR`Type`Real], IR`Entity[IR`Constant, value_, IR`Type`Real]]] := CAssign[varA, value]

(* Integer: ASSIGMENT to CONSTANTS *)
MakeCL /: MakeCL[IR`Assign[IR`Entity[varA_String, _, IR`Type`Integer], IR`Entity[IR`Constant, value_, IR`Type`Integer]]] := CAssign[varA, value]


(* Reduced Complex: ASSIGMENT to CONSTANTS *)
MakeCL /: MakeCL[IR`Assign[IR`Entity[varA_String, _, IR`Type`Complex`Re], IR`Entity[IR`Constant, value_, IR`Type`Complex`Re]]] := CAssign[varA, value // Re]
MakeCL /: MakeCL[IR`Assign[IR`Entity[varA_String, _, IR`Type`Complex`Im], IR`Entity[IR`Constant, value_, IR`Type`Complex`Im]]] := CAssign[varA, value // Im]

(* Full Complex: ASSIGMENT to CONSTANTS *)
MakeCL /: MakeCL[IR`Assign[IR`Entity[varA_String, _, IR`Type`Complex], IR`Entity[varB_String, _, IR`Type`Complex]]] := {CStatement[CAssign[CMember[varA, "re"], CMember[varB, "re"]]], CStatement[CAssign[CMember[varA, "im"], CMember[varB, "im"]]]}

(* Full Complex: ASSIGMENT to VARIABLES *)
MakeCL /: MakeCL[IR`Assign[IR`Entity[varA_String, _, IR`Type`Complex], IR`Entity[anyB_, valueB_, IR`Type`Complex]]] := With[{},
    Echo[IR`Entity[anyB, valueB, IR`Type`Complex`Re]];
    
    CStatement /@ {
       CAssign[CMember[varA, "re"], IR`Entity[anyB, valueB, IR`Type`Complex`Re] // MakeCL],
       CAssign[CMember[varA, "im"], IR`Entity[anyB, valueB, IR`Type`Complex`Im] // MakeCL]
    }
]


(* Reduced Complex: ASSIGMENT to Variables *)
MakeCL /: MakeCL[IR`Assign[IR`Entity[varA_String, _, IR`Type`Complex`Re], IR`Entity[varB_String, _, IR`Type`Complex`Re]]] := With[{},
  CAssign[varA, varB]
]
MakeCL /: MakeCL[IR`Assign[IR`Entity[varA_String, _, IR`Type`Complex`Im], IR`Entity[varB_String, _, IR`Type`Complex`Im]]] := With[{},
  CAssign[varA, varB]
]


(* Reduced Complex: ASSIGMENT to Constants *)
MakeCL /: MakeCL[IR`Assign[IR`Entity[varA_String, _, IR`Type`Complex`Re], IR`Entity[IR`Constant, varB, IR`Type`Complex`Re]]] := With[{},
  CAssign[varA, varB // Re]
]
MakeCL /: MakeCL[IR`Assign[IR`Entity[varA_String, _, IR`Type`Complex`Im], IR`Entity[IR`Constant, varB, IR`Type`Complex`Im]]] := With[{},
  CAssign[varA, varB // Im]
]


(* Reduced Complex: ASSIGMENT to Operators??? *)
MakeCL /: MakeCL[IR`Assign[IR`Entity[varA_String, _, IR`Type`Complex`Re], IR`Entity[varB_, _, IR`Type`Complex`Re]]] := With[{},
  CAssign[varA, IR`Entity[varB, Null, IR`Type`Complex`Re] // MakeCL]
]

MakeCL /: MakeCL[IR`Assign[IR`Entity[varA_String, _, IR`Type`Complex`Im], IR`Entity[varB_, _, IR`Type`Complex`Im]]] := With[{},
  CAssign[varA, IR`Entity[varB, Null, IR`Type`Complex`Im] // MakeCL]
]


(* MISC Operations *)
MakeCL /: MakeCL[IR`Return[IR`Entity[name_, __]]] := CReturn[name]
MakeCL /: MakeCL[IR`While[cond_, body_]] := CWhile[MakeCL[cond], MakeCL[body]]

(* REAL: Operations 1x *)
MakeCL /: MakeCL[IR`Entity[
  IR`Operator[op_, IR`Entity[data_String, value_, IR`Type`Real]],
  _, IR`Type`Real]] := COperator[op, {data}]

(* REAL: Operations 2x *)
MakeCL /: MakeCL[IR`Entity[
  IR`Operator[op_, IR`Entity[data_String, value_, IR`Type`Real], IR`Entity[data2_String, value2_, IR`Type`Real]],
  _, IR`Type`Real]] := COperator[op, {data, data2}]  

(* REAL: Operations 2x = BOOL *)
MakeCL /: MakeCL[IR`Entity[
  IR`Operator[op_, IR`Entity[data_String, value_, IR`Type`Real], IR`Entity[data2_String, value2_, IR`Type`Real]],
  _, IR`Type`Bool]] := COperator[op, {data, data2}]    

(* REAL: Operations 2x constant *)
MakeCL /: MakeCL[IR`Entity[
  IR`Operator[op_, IR`Entity[data_String, value_, IR`Type`Real], IR`Entity[IR`Constant, value2_, IR`Type`Real]],
  _, IR`Type`Real]] := COperator[op, {data, value2}]

(* REAL: Operations 2x constant boolean result *)
MakeCL /: MakeCL[IR`Entity[
  IR`Operator[op_, IR`Entity[data_String, value_, IR`Type`Real], IR`Entity[IR`Constant, value2_, IR`Type`Real]],
  _, IR`Type`Bool]] := COperator[op, {data, value2}]   


(* Re[Complex] x REAL = Re[Complex]: Operations 2x *)
MakeCL /: MakeCL[IR`Entity[
  IR`Operator[op_, IR`Entity[data_String, value_, IR`Type`Complex`Re], IR`Entity[data2_String, value2_, IR`Type`Real]],
  _, IR`Type`Complex`Re]] := COperator[op, {data, data2}] 

(* Re[Complex] x REAL = Im[Complex]: Operations 2x *)
MakeCL /: MakeCL[IR`Entity[
  IR`Operator[op_, IR`Entity[data_String, value_, IR`Type`Complex`Re], IR`Entity[data2_String, value2_, IR`Type`Real]],
  _, IR`Type`Complex`Im]] := 0.   


(* Re[Complex] x INTEGER = Re[Complex]: Operations 2x *)
MakeCL /: MakeCL[IR`Entity[
  IR`Operator[op_, IR`Entity[data_String, value_, IR`Type`Complex`Re], IR`Entity[data2_String, value2_, IR`Type`Integer]],
  _, IR`Type`Complex`Re]] := COperator[op, {data, CCast["float", data2]}] 

MakeCL /: MakeCL[IR`Entity[
  IR`Operator[op_, IR`Entity[data2_String, value2_, IR`Type`Integer], IR`Entity[data_String, value_, IR`Type`Complex`Re]],
  _, IR`Type`Complex`Re]] := COperator[op, {CCast["float", data2], data}]   

(* Im[Complex] x INTEGER = Im[Complex]: Operations 2x *)
MakeCL /: MakeCL[IR`Entity[
  IR`Operator[op_, IR`Entity[data_String, value_, IR`Type`Complex`Im], IR`Entity[data2_String, value2_, IR`Type`Integer]],
  _, IR`Type`Complex`Im]] := COperator[op, {data, CCast["float", data2]}]     

MakeCL /: MakeCL[IR`Entity[
  IR`Operator[op_, IR`Entity[data2_String, value2_, IR`Type`Integer], IR`Entity[data_String, value_, IR`Type`Complex`Im]],
  _, IR`Type`Complex`Im]] := COperator[op, {CCast["float", data2], data}]      



(* Re[Complex] x INTEGER = Im[Complex]: 0 Operations 2x *)
MakeCL /: MakeCL[IR`Entity[
  IR`Operator[op_, IR`Entity[data_String, value_, IR`Type`Complex`Re], IR`Entity[data2_String, value2_, IR`Type`Integer]],
  _, IR`Type`Complex`Im]] := COperator[op, {data, CCast["float", data2]}] 

MakeCL /: MakeCL[IR`Entity[
  IR`Operator[op_, IR`Entity[data2_String, value2_, IR`Type`Integer], IR`Entity[data_String, value_, IR`Type`Complex`Re]],
  _, IR`Type`Complex`Im]] := COperator[op, {CCast["float", data2], data}]   

(* Im[Complex] x INTEGER = Re[Complex]: 0 Operations 2x *)
MakeCL /: MakeCL[IR`Entity[
  IR`Operator[op_, IR`Entity[data_String, value_, IR`Type`Complex`Im], IR`Entity[data2_String, value2_, IR`Type`Integer]],
  _, IR`Type`Complex`Re]] := 0.

MakeCL /: MakeCL[IR`Entity[
  IR`Operator[op_, IR`Entity[data2_String, value2_, IR`Type`Integer], IR`Entity[data_String, value_, IR`Type`Complex`Im]],
  _, IR`Type`Complex`Re]] := 0.  




(* Im[Complex] x REAL = Re[Complex]: Operations 2x *)
MakeCL /: MakeCL[IR`Entity[
  IR`Operator[op_, IR`Entity[data_String, value_, IR`Type`Complex`Im], IR`Entity[data2_String, value2_, IR`Type`Real]],
  _, IR`Type`Complex`Re]] := 0.

(* Im[Complex] x REAL = Im[Complex]: Operations 2x *)
MakeCL /: MakeCL[IR`Entity[
  IR`Operator[op_, IR`Entity[data_String, value_, IR`Type`Complex`Im], IR`Entity[data2_String, value2_, IR`Type`Real]],
  _, IR`Type`Complex`Im]] := COperator[op, {data, data2}] 


(* Complex x REAL = Re[Complex]: Operations 2x *)
MakeCL /: MakeCL[IR`Entity[
  IR`Operator[op_, IR`Entity[data_String, value_, IR`Type`Complex], IR`Entity[data2_String, value2_, IR`Type`Real]],
  _, IR`Type`Complex`Re]] := COperator[op, {CMember[data, "re"], data2}]

(* Complex x REAL = Im[Complex]: Operations 2x *)
MakeCL /: MakeCL[IR`Entity[
  IR`Operator[op_, IR`Entity[data_String, value_, IR`Type`Complex], IR`Entity[data2_String, value2_, IR`Type`Real]],
  _, IR`Type`Complex`Im]] := COperator[op, {CMember[data, "im"], data2}]



(* Complex x Complex = Re[Complex]: Operations 2x *)
MakeCL /: MakeCL[IR`Entity[
  IR`Operator[op_, IR`Entity[data_String, value_, IR`Type`Complex], IR`Entity[data2_String, value2_, IR`Type`Complex]],
  _, IR`Type`Complex`Re]] := COperator[op, {CMember[data, "re"], CMember[data2, "re"]}]

(* Complex x Complex = Im[Complex]: Operations 2x *)
MakeCL /: MakeCL[IR`Entity[
  IR`Operator[op_, IR`Entity[data_String, value_, IR`Type`Complex], IR`Entity[data2_String, value2_, IR`Type`Complex]],
  _, IR`Type`Complex`Im]] := COperator[op, {CMember[data, "im"], CMember[data2, "im"]}]  


(* Full Complex: Math Operations : Complex -> Re *)
MakeCL /: MakeCL[IR`Entity[
  IR`Operator[Exp, IR`Entity[data_String, value_, IR`Type`Complex]],
  _, IR`Type`Complex`Re]] := COperator[Times, {CStandardMathOperator[Cos, {CMember[data, "im"]}], CStandardMathOperator[Exp, {CMember[data, "re"]}]}]


(* Full Complex: Math Operations : Complex -> Im *)
MakeCL /: MakeCL[IR`Entity[
  IR`Operator[Exp, IR`Entity[data_String, value_, IR`Type`Complex]],
  _, IR`Type`Complex`Im]] := COperator[Times, {CStandardMathOperator[Sin, {CMember[data, "im"]}], CStandardMathOperator[Exp, {CMember[data, "re"]}]}]


(* Real: Math Operations  *)
MakeCL /: MakeCL[IR`Entity[
  IR`Operator[Exp, IR`Entity[data_String, value_, IR`Type`Real]],
  _, IR`Type`Real]] := CStandardMathOperator[Exp, {data}]


(*
MakeCL /: MakeCL[IR`Entity[IR`Operator[op_, IR`Entity[varA_, valueA_, IR`Type`Complex`Re], IR`Entity[varB_, valueB_, IR`Type`Complex]]], valA_, IR`Type`Complex`Re] := "FUCK"
MakeCL /: MakeCL[IR`Entity[IR`Operator[op_, IR`Entity[varA_, valueA_, IR`Type`Complex`Im], IR`Entity[varB_, valueB_, IR`Type`Complex]]], valB_, IR`Type`Complex`Im] := "FUCKy"
*)

(* shit does not work properly *)


(* cast from int to float *)
