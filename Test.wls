SetDirectory[FileNameJoin[{If[StringQ[NotebookDirectory[]], NotebookDirectory[], DirectoryName[$InputFileName]]}]]
Get[FileNameJoin[{"Kernel", "IR.wl"}]]
Get[FileNameJoin[{"Kernel", "OpenCL.wl"}]]

(* example function *)
f = Function[{x,t}, Sum[Exp[I(w x - w t)], {w,0.0,10.0,0.01}] ];

ir = Mirage`IR`Translate[f, {"x"->1.0, "t"->3.6}];

Print["--- Intermediate Representation ---"];
ir // Print;

Print["--- C code ---"];
Mirage`OpenCL`Translate[ir] // Print;