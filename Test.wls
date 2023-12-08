SetDirectory[FileNameJoin[{If[StringQ[NotebookDirectory[]], NotebookDirectory[], DirectoryName[$InputFileName]]}]]

Get["IR.wl"];
Get["CL.wl"];

f =   Function[{x,t}, 
  
  With[{r = 0.8, d = 0.3}, Sum[

  Exp[- I w t + I w x]
  
, {w, 5,10+5, 0.1}]]];

Mirage`IR`Translate[
    
 f

, {"x"->0.1, "t"->2.}] // IR // MakeCL // ToCCodeString // Print