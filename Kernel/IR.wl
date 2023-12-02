BeginPackage["JerryI`Mirage`IR`"]




(* utils *)
MapHeld[f_, list_] := Table[Extract[Unevaluated[list], i, f], {i, 1, Length[Unevaluated[list]]}]
SetAttributes[MapHeld, HoldAll]

(*** LEXER ***)
(* autodetects types of variables by computing the tree *)

(* it has a few atoms to represent the code *)
(* -------------------------------------------- *)
(*  IR`Entity[data_, computedValue_, type_]         - the smallets and simples atom that represent a typed object *)

(*  IR`Operator[operator_, arguments__IR`Entity]    - represents an operator or function that takes IR`Entities as arguments *)
(*  IR`Operator[operator_, arguments__IR`Entity]    - represents an operator or function that takes IR`Entities as arguments *)
(*  IR`Function[type_, name_String, arguments_List, body_]    - represents an normal C-like function *)

(*  IR`Block[list_IR`Entity]                        - block of statements/ commands *)
(*  IR`Define[var_IR`Entity]                        - declare a variable *)
(*  IR`Assign[a_IR`Entity, b_IR`Entity]             - assign entity a to b *)


(*  IR`Return[a_IR`Entity]                          - C-like return  *)

(*  IR`Type`Real                                    - real type *)
(*  IR`Type`Integer                                 - integer type *)
(*  whatever type .... *)


ClearAll[Lexer]
SetAttributes[Lexer, HoldAll]

(* random names generator *)
Lexer`internalIndex = 0;
Lexer`RandomName[] := (Lexer`internalIndex++; RandomWord[]<>ToString[Lexer`internalIndex]);
Lexer`RandomName[s_String] := (Lexer`internalIndex++; s<>ToString[Lexer`internalIndex]);

Lexer`Name[s_String] := s
Lexer`Name[s_Symbol] := ToString[s]
SetAttributes[Lexer`Name, HoldFirst]

(* type, data and value extrators *)
IR`Entity /: Value[IR`Entity[data_, value_, type_]] := value
IR`Entity /: IR[IR`Entity[data_, value_, type_]] := data 
IR`Entity /: Type[IR`Entity[data_, value_, type_]] := type 

(* type detection *)
Lexer`Type[_Real] := IR`Type`Real 
Lexer`Type[_Integer] := IR`Type`Integer

(* constants *)
Lexer[a_Real, env_] := IR`Entity[IR`Constant, a, Lexer`Type[a]]
Lexer[a_Integer, env_] := IR`Entity[IR`Constant, a, Lexer`Type[a]]

(* anything which is unknown to the system *)
Lexer[any_, env_] := IR`Entity[IR`Unknown[any], Undefined, Undefined] 
SetAttributes[IR`Unknown, HoldFirst]

(* take a symbol from the stack *)
Lexer[a_Symbol, env_] := env[Lexer`Name[a]]

(* ... *)
Lexer[IR`Entity[a__], env_] := IR`Entity[a]


(* convert an anonimous function *)
Lexer[Function[args_, body_], env_] := Module[{},
  With[{return = Lexer`RandomName[], arguments = MapHeld[Function[v, With[{name = Lexer`Name[v]}, 
                                                                            env[name] = IR`Entity[
                                                                              name, 
                                                                              env[name], 
                                                                              Lexer`Type[env[name]]
                                                                            ];
                                                                            env[name]
                                                                          ], HoldFirst], args]},
    With[{result = Lexer[Set[return, body], env]},   
      IR`Entity[
        IR`Function[result // Type, "kernel", arguments, IR`Block[{
          result,
          IR`Return[env[Lexer`Name[return]]]
        } // Flatten]],

        Undefined,
        Undefined
      ]
    ]
  ]
]

Lexer[Plus[a_, b_], env_] := With[{},
  With[{aa = Lexer[a, env], bb = Lexer[b, env]},
    With[{val = Value[aa] + Value[bb]},
      IR`Entity[IR`Operator[Plus, aa, bb], val, Lexer`Type[val]]
    ]
  ]
]

Lexer[Set[a_, b_], env_] := Module[{},
  With[{result = Lexer[b, env], name = Lexer`Name[a]},
    If[KeyExistsQ[env, name],
      env[name] = Lexer`modifyVariable[name, result];
      IR`Entity[IR`Assign[env[name], result], result // Value, result // Type]
    ,
      IR`Entity[IR`List[{
        env[name] = Lexer`modifyVariable[name, result];
        IR`Define[env[name]],
        IR`Assign[env[name], result]
      }], result // Value, result // Type]
    ]
  ]
]

Lexer[Times[a_, b_], env_] := With[{},
  With[{aa = Lexer[a, env], bb = Lexer[b, env]},
    With[{val = Value[aa] Value[bb]},
      IR`Entity[IR`Operator[Times, aa, bb], val, Lexer`Type[val]]
    ]
  ]
]

(* helper to update the value of stored variable *)
Lexer`modifyVariable[name_String, IR`Entity[data_, value_, type_]] := IR`Entity[name, value, type]; 


(* !!! just as an example complex structures *)
Lexer[Set[a_, With[args_, body_]], env_] := Module[{},
  With[{scoped = MapHeld[Function[v, Lexer[v, env], HoldFirst], args]},
    With[{r = Lexer[Set[a, body], env]},
      IR`Entity[IR`List[{scoped, r} // Flatten], r // Value, r // Type]
    ]
  ]
]

Lexer[Set[a_, Sum[expr_, {var_, from_, to_, st_}]], env_] := Module[{},
  With[{sum = Lexer`RandomName["sum"], init = Lexer[Set[var, from], env], till = Lexer[to, env]},
    With[{test = Lexer[Set[sum, expr], env]},
      IR`Entity[IR`List[{
        IR`Define[IR`Entity[sum, test // Value, test // Type]],
        init,
        IR`While[IR`Operator[LessEqual, Lexer[var, env], till],
          IR`Block[{
            Lexer[Set[sum, expr], env],
            Lexer[Set[var, var + st], env]
          } // Flatten]
        ],
        With[{s = env[sum]}, Lexer[Set[a, s], env]]
      } // Flatten], 

          test // Value,
          test // Type 
      ]
    ]
  ]
] 

Lexer[op_[x_], env_] := With[{val = Lexer[x, env]},
  IR`Entity[IR`Operator[op, val], op[val // Value], val // Type]
]

(* !!! just as an example *)
Lexer[Sin[x_], env_] := With[{val = Lexer[x, env]},
  IR`Entity[IR`Operator[Sin, val], Sin[val // Value], val // Type]
]





Mirage`IR`Translate[f_Function, args_List] := Module[{env},
    env = Association[args];
    Lexer[f, env]
]

Begin["`Private`"]

End[]
EndPackage[]