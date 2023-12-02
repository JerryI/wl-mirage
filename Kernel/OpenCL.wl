BeginPackage["JerryI`Mirage`OpenCL`", {"JerryI`Mirage`IR`", "SymbolicC`"}]


MakeCL /: MakeCL[IR`Function[type_, name_String, args_List, body_]] := CFunction[MakeCL[type], name, {# // Type // MakeCL, # // IR} &/@ args, MakeCL[body]]

MakeCL /: MakeCL[a_String] := a 

MakeCL /: MakeCL[IR`Type`Real] := "float"
MakeCL /: MakeCL[IR`Define[IR`Entity[name_String, _, IR`Type`Real]]] := CDeclare["float", name]

MakeCL /: MakeCL[IR`Block[l_List]] := CBlock[ MakeCL[#] &/@ l ]

MakeCL /: MakeCL[IR`List[l_List]] := CStatement[MakeCL[#]] &/@ l 

MakeCL /: MakeCL[IR`Entity[data_, _, _]] := MakeCL[data]
MakeCL /: MakeCL[IR`Entity[IR`Constant, value_, IR`Type`Real]] := value

MakeCL /: MakeCL[IR`Entity[IR`Constant, value_, IR`Type`Integer]] := value

(* examples of typed operators *)
MakeCL /: MakeCL[IR`Assign[IR`Entity[varA_String, _, IR`Type`Real], varB_]] := CAssign[varA, MakeCL[varB]]
MakeCL /: MakeCL[IR`Assign[IR`Entity[varA_String, _, IR`Type`Real], IR`Entity[IR`Constant, value_, IR`Type`Real]]] := CAssign[varA, value]
MakeCL /: MakeCL[IR`Assign[IR`Entity[varA_String, _, IR`Type`Real], IR`Entity[varB_String, _, IR`Type`Real]]] := CAssign[varA, varB]

MakeCL /: MakeCL[IR`Return[IR`Entity[name_, __]]] := CReturn[name]
MakeCL /: MakeCL[IR`While[cond_, body_]] := CWhile[MakeCL[cond], MakeCL[body]]

MakeCL /: MakeCL[IR`Operator[op_, var_]] := COperator[op, var // MakeCL]
MakeCL /: MakeCL[IR`Operator[op_, varA_, varB_]] := COperator[op, {varA // MakeCL, varB // MakeCL}]

(* cast from int to float *)
MakeCL /: MakeCL[IR`Operator[Times, IR`Entity[firstA__, IR`Type`Real], IR`Entity[firstB__, IR`Type`Integer]]] := COperator[Times, {IR`Entity[firstA, IR`Type`Real] // MakeCL, CCast["float", IR`Entity[firstB, IR`Type`Integer] // MakeCL] // CParentheses} ]
MakeCL /: MakeCL[IR`Operator[Times, IR`Entity[firstA__, IR`Type`Integer], IR`Entity[firstB__, IR`Type`Real]]] := COperator[Times, {CCast["float", IR`Entity[firstA, IR`Type`Integer] // MakeCL] // CParentheses, IR`Entity[firstB, IR`Type`Real] // MakeCL}]



Mirage`OpenCL`Translate[f_] := Module[{}, 
    f // IR // MakeCL // ToCCodeString
]


Mirage`OpenCL`Map[f_Function, list_List, OptionsPattern[]] := Module[{probe},
    If[OptionValue["Probe"] === Null,
        Print["Probing arguments are not specified, assuming Reals..."];
        probe = {3.14};
    ,
        probe = OptionValue["Probe"]
    ];

    (*... convertion happen*)
]

Options[Mirage`OpenCL`Map] = {"Probe"->Null}

Begin["`Private`"]

End[]
EndPackage[]