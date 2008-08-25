program treelistviewexample;
uses  forms,
  treelistviewexampleu in 'treelistviewexampleu.pas';
 begin
  Application.Initialize;
  Application.CreateForm(texampleform, form);
  
  //this is a very stupid/clever workaround but at least it works
  while running do Application.ProcessMessages;

end.


                                    
