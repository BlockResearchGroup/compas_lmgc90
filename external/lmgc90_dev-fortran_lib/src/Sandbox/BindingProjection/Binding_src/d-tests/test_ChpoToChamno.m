mex -fortran -llapack ChpoToChamnog.F ChpoToChamno.f ddreal8toint.f ddinttoreal8.f zdanul.f place2.f

path(path,'matlabEF')
path(path,'matlabUtils')

mode1 = 'COPL';
idim = 2;

file1 = 'test_ChpoToChamno.avs';
xcrit1 = 1.e-6;
[xcoor1,ListMesh1,ListChpo1,ListnMesh1,ListChml1,ListCara1,error1] = ...
  ReadMergeAVS2(xcrit1,file1,1);
xcoor1 = xcoor1(:,[1:idim]);

mail1 = ListMesh1{1};
chpo1 = ListChpo1{1};
nmail1 = ListnMesh1{1};

clear ListMesh1 ListChpo1 ListnMesh1 ListChml1 ListCara1

% Avec matlab
[chamno1,intg1] = ChpoToChamno3(chpo1,nmail1,mail1);
XVAL2 = chamno1{1}{1}.XVAL';

% Avec mex file
YVAL = chpo1{1}{1}.XVAL;
LPT = nmail1{1}.MAIL';
MAIL = mail1{1}.MAIL;
XVAL = ChpoToChamnog(YVAL,LPT,MAIL);

err1 = max(max(abs(XVAL - XVAL2))) / max(max(abs(XVAL)));
if (err1 > 1.e-5)
  err1
  error('PB1')
end

disp('TEST PASSE AVEC SUCCES')
quit
