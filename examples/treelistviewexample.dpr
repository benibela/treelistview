program treelistviewexample;
uses  forms,
  treelistviewexampleu in 'treelistviewexampleu.pas';
 begin
  Application.Initialize;
  Application.CreateForm(texampleform, form);
  
  //this is a very stupid/clever workaround but at least it works (and the same
  //unit can be used in Lazarus and Delphi)
  while running do Application.ProcessMessages;

end.


                                    
