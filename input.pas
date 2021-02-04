program 
VAR 
  iNdx, iA, iB, iC : integer;
  sName, sArni : string;
  bStatus : boolean

BEGIN
  {iA := 10 + ' AAA'; }

  iNdx := 10;
  while (iNdx >= 0) do begin
    writeln(iNdx); iNdx := iNdx-1
  end;

  write('(((-4+9)*9/5+1)*10+20)/6 ='); writeln((((-4+9)*9/NOT 5+1)*10+20)/6); 

  write('Enter your mark from 1 to 5: '); read(iA);
  if (iA > 3) then writeln('Oh, you are good student)') else 
  if (iA <=2) then writeln('Oh, no!') else writeln('You can be better, bro.');

  write('Enter your name: ');
  read(sName);
  sArni := 'I''ll be back!'; writeln(sName+', '+sArni)

End. 