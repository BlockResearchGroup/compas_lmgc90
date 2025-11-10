path(path,'matlabEF')
path(path,'matlabUtils')
mode1 = 'COPL';
idim = 2;

% Recuperation des donnees
% """"""""""""""""""""""""
disp('Recuperation des donnees')
file1 = 'testdd3.avs';
xcrit1 = -1.e-5;
[xcoor1,ListMesh1,ListChpo1,ListnMesh1,ListChml1, ...
  ListCara1,error1] = ReadMergeAVS2(xcrit1,file1,2);
xcoor2 = xcoor1;
xcoor1 = xcoor1(:,1:idim);

mail1 = ListMesh1{1};
chpo1 = ListChpo1{1};
nmail1 = ListnMesh1{1};
mail2 = ListMesh1{2};

clear ListMesh1 ListChpo1 ListnMesh1 ListChml1 ListCara1;

YVAL = chpo1{1}{1}.XVAL;
LPT  = nmail1{1}.MAIL';
MAIL = mail1{1}.MAIL;
XVAL = ChpoToChamnog(YVAL,LPT,MAIL);

XCOOR = xcoor1;
CTYPE = 'TRI3';
XPREC = 1.e-6;
OPTIO = 'SIMPLEX';
nmail2 = ChangeMesh2(mail2,'POI1');
numer2 = nmail2{1}.MAIL';
XCOORC = XCOOR(numer2,:);
[ELEM,ALPHA] = NodesInMeshg(XCOOR,MAIL,CTYPE,XPREC,XCOORC,OPTIO)

YVAL1 = ProjectChamnog(ELEM,ALPHA,XVAL);

[chpo2,nmail2] = VectToChpo2(YVAL1,numer2,[1:length(numer2)]',[{'SCAL'}]);

[chamno1,intg1] = ChpoToChamno3(chpo1,nmail1,mail1);
file1 = 'testdd3a.pos';
fid=fopen(file1,'w');
  error1 = WriteChamnoGMSH(xcoor2,mail1,chamno1,fid)
fclose(fid);
[chamno2,intg2] = ChpoToChamno3(chpo2,nmail2,mail2);
file1 = 'testdd3b.pos';
fid=fopen(file1,'w');
  error1 = WriteChamnoGMSH(xcoor2,mail2,chamno2,fid)
fclose(fid);





