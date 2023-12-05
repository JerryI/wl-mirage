BeginPackage["JerryI`Mirage`IR`"]

(* utils *)
MapHeld[f_, list_] := Table[Extract[Unevaluated[list], i, f], {i, 1, Length[Unevaluated[list]]}]
SetAttributes[MapHeld, HoldAll]

ClearAll[Lexer]
SetAttributes[Lexer, HoldAll]

(* random names generator *)
Lexer`internalIndex = 0;
Lexer`RandomName[] := (Lexer`internalIndex++; RandomWord[]<>ToString[Lexer`internalIndex]);
Lexer`RandomName[s_String] := (Lexer`internalIndex++; s<>ToString[Lexer`internalIndex]);

Lexer`allocateRegister[env_] := Lexer`RandomName["reg"]

Lexer`Name[s_String] := s
Lexer`Name[s_Symbol] := ToString[s]
SetAttributes[Lexer`Name, HoldFirst]

(* type, data and value extrators *)
IR`Entity /: Value[IR`Entity[data_, value_, type_]] := value
IR`Entity /: IR[IR`Entity[data_, value_, type_]] := data 
IR`Entity /: Type[IR`Entity[data_, value_, type_]] := type 

(* type detection *)
Lexer`Type[_Real] := IR`Type`Real 
Lexer`Type[i_Complex] := If[Re[i] === 0, IR`Type`Complex`Re, If[Im[i] === 0, IR`Type`Complex`Im, IR`Type`Complex] ] 
Lexer`Type[I] := IR`Type`Complex`Im 
Lexer`Type[_Integer] := IR`Type`Integer


(* constants *)
Lexer[a_Real, env_] := IR`Entity[IR`Constant, a, Lexer`Type[a]]
Lexer[a_Integer, env_] := IR`Entity[IR`Constant, a, Lexer`Type[a]]
Lexer[a_Complex, env_] := IR`Entity[IR`Constant, a, Lexer`Type[a]]

(* anything which is unknown to the system *)
Lexer[any_, env_] := IR`Entity[IR`Unknown[any], Undefined, Undefined] 
SetAttributes[IR`Unknown, HoldFirst]

(* take a symbol from the stack *)
Lexer[a_Symbol, env_] := env[Lexer`Name[a]]

Lexer[I, env_] := IR`Entity[IR`Constant, I, IR`Type`Complex`Im] 

(* ... *)
(*Lexer[IR`Entity[a__], env_] := IR`Entity[a]*)


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

results = {};

(* convert 4 args to 2 args operators like Times, Plus ... *)
Lexer[Set[var_, op_[a_, b_, c_, d_]], env_] := Lexer[Set[var, op[op[a,b],op[c,d]]], env]

(* convert 3 args to 2 args operators like Times, Plus ... *)
Lexer[Set[var_, op_[a_, b_, c_]], env_] := Lexer[Set[var, op[op[a,b],c]], env]

(* any nested operators are decomposed into a multiple single steps using registers *)
Lexer[Set[var_, op_[a_]], env_] := With[{iregister = Lexer`allocateRegister[env], oregister = Lexer`allocateRegister[env]},
  
  (* set the input register to the a-variable *)
  With[{input = Lexer[Set[iregister, a], env]},
  
    (* estimate the value of the operator *)
    With[{value = op[input // Value]},
  
      (* make an ir's representation of an operator *)
      With[{output = IR`Entity[IR`Operator[op, IR`Entity[iregister, input // Value, input // Type]], value, Lexer`Type[value]]},

        (* as set of operations *)
        IR`Entity[IR`List[{
          (* define input registers and assing them to the data *)
          input,

          (* write data from an operator to the output register *)
          Lexer`setVariable[oregister, output, env],

          (* write data from an ouput register to var *)
          Lexer`setVariable[Lexer`Name[var], env[oregister], env]
        }], output // Value, output // Type]
      ]
    ]
  ]
]

(* the same, but for two variables operator *)
Lexer[Set[var_, op_[a_, b_]], env_] := With[{iaregister = Lexer`allocateRegister[env],ibregister = Lexer`allocateRegister[env], oregister = Lexer`allocateRegister[env]},

  With[{ainput = Lexer[Set[iaregister, a], env], binput = Lexer[Set[ibregister, b], env]},
  
    With[{value = op[ainput // Value, binput // Value]},
    
      With[{output = IR`Entity[IR`Operator[op, IR`Entity[iaregister, ainput // Value, ainput // Type], IR`Entity[ibregister, binput // Value, binput // Type]], value, Lexer`Type[value]]},
      
        IR`Entity[IR`List[{
        
          ainput,
          binput,
          
          Lexer`setVariable[oregister, output, env],
          
          Lexer`setVariable[Lexer`Name[var], env[oregister], env]
          
        }], output // Value, output // Type]
      ]
    ]
  ]
]


Lexer[Set[a_, With[args_, body_]], env_] := Module[{},

  With[{scoped = MapHeld[Function[v, Lexer[v, env], HoldFirst], args]},
    test = {env, scoped};
    With[{r = Lexer[Set[a, body], env]},
    
      IR`Entity[IR`List[{
      
        scoped, r
        
      } // Flatten], r // Value, r // Type]
      
    ]
  ]
]

Lexer[Set[a_, Sum[expr_, {var_, from_, to_, step_:1}]], env_] := Module[{},
With[{iregister = Lexer`allocateRegister[env], fregister = Lexer`allocateRegister[env], tregister = Lexer`allocateRegister[env], sregister = Lexer`allocateRegister[env], sumregister = Lexer`allocateRegister[env]},

  With[{fvar = Lexer[Set[fregister, from], env], tvar = Lexer[Set[tregister, to], env], svar = Lexer[Set[sregister, step], env]},
  
    With[{init = Lexer`setVariable[Lexer`Name[var], env[fregister], env]},

      With[{body = Lexer[Set[iregister, expr], env]},
      

        With[{sum = With[{val = env[iregister] // Value}, Lexer`setVariable[sumregister, env[iregister], env]]},

          With[{increment = Lexer[Set[var, var + step], env], sset = Lexer`setVariable[Lexer`Name[a], env[sumregister], env]},

            
            IR`Entity[
              IR`List[{
                fvar,
                tvar,
                svar,

                init,
                sum,

                IR`While[IR`Entity[IR`Operator[LessEqual, env[Lexer`Name[var]], env[tregister]], True, IR`Type`Bool],
                  IR`Block[{
                    increment,
                    body,
                    IR`Assign[env[sumregister], IR`Entity[IR`Operator[Plus, env[sumregister], env[iregister]], env[sumregister] // Value, env[sumregister] // Type]]
                  }]
                ],

                sset
                
              }], sset // Value, sset // Type
            ]
            
          
          ]
        
        ]
      ]
      
    ]
  ]
]]


(* variable to variable assigments *)

Lexer[Set[a_Symbol, b_Symbol], env_] := Module[{},
  With[{result = Lexer[b, env], name = Lexer`Name[a]},
    Lexer`setVariable[name, result, env]
  ]
]

Lexer[Set[a_String, b_String], env_] := Module[{},
  With[{result = Lexer[b, env], name = Lexer`Name[a]},
    Lexer`setVariable[name, result, env]
  ]
]

Lexer[Set[a_String, b_Symbol], env_] := Module[{},
  With[{result = Lexer[b, env], name = Lexer`Name[a]},
    Lexer`setVariable[name, result, env]
  ]
]

Lexer[Set[a_String, b_], env_] := Module[{},
  With[{result = Lexer[b, env], name = Lexer`Name[a]},
    Lexer`setVariable[name, result, env]
  ]
]

Lexer[Set[a_Symbol, b_], env_] := Module[{},
  With[{result = Lexer[b, env], name = Lexer`Name[a]},
    Lexer`setVariable[name, result, env]
  ]
]


(* helper function *)
Lexer`setVariable[name_String, result_, env_] := With[{},
  (* check if it is already defined *)
  If[KeyExistsQ[env, name],
    
    (* update the stored value *)
    env[name] = IR`Entity[name, result // Value, result // Type];
      
    (* return the assigment *)
    IR`Entity[IR`Assign[env[name], result], result // Value, result // Type]
      
    ,
    
    IR`Entity[IR`List[{
      (* put the stored value *)
      env[name] = IR`Entity[name, result // Value, result // Type];

      (* export Definition *)
      IR`Define[env[name]],
        
      (* export assigment *)
      IR`Assign[env[name], result]
        
    }], result // Value, result // Type]
  ]
]

SetAttributes[Lexer`setVariable, HoldRest]

Mirage`IR`Translate[f_Function, args_List] := Module[{env},
    env = Association[args];
    Lexer[f, env]
]

Begin["`Private`"]

End[]
EndPackage[]