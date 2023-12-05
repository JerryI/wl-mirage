SetDirectory[FileNameJoin[{If[StringQ[NotebookDirectory[]], NotebookDirectory[], DirectoryName[$InputFileName]]}]]

Get["IR.wl"];
Get["CL.wl"];

Mirage`IR`Translate[
    
  Function[{x,t}, Sum[Exp[I w x - I w t], {w,0.1,10., 0.1} ]]

, {"x"->0.1, "t"->2.}] // IR // MakeCL // ToCCodeString // Print