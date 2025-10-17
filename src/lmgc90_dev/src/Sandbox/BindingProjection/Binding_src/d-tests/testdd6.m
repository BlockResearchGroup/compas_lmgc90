% On teste les elements de structure (TRI3 en 3D)
path(path,'matlabEF')
path(path,'matlabUtils')
idim = 3;

% Noeuds
xcoor1 = zeros(0,3);
% grille cartesienne
n1 = 21;
ino = 0;
for iz = 1 : n1
  for iy = 1 : n1
    for ix = 1 : n1
      ino = ino + 1;
      xcoor1(ino,:) = (1./(n1-1.))*[(ix-1) (iy-1) (iz-1)] - 0.5;
    end
  end
end
% element TRI3
ino = ino + 3
xcoor1 = [xcoor1
         -0.5 -0.5 0.
          0.5 -0.5 0.
         -0.5  0.5 0.];
% On tourne le tout !
theta = pi/4.;
phi = pi/4.;
Q = [cos(phi)*cos(theta) -sin(theta) -sin(phi)*cos(theta)
     cos(phi)*sin(theta) cos(theta)  -sin(phi)*sin(theta)
     sin(phi)            0.           cos(phi)];
toto = Q*xcoor1';
xcoor1 = toto';

% Test TRI3
YVAL = [1. 2. 3.]';
LPT  = [ino-2 ino-1 ino];
MAIL = [ino-2 ino-1 ino];
XVAL = ChpoToChamnog(YVAL,LPT,MAIL);
clear mail1; mail1{1} = struct('TYPE','TRI3','MAIL',MAIL);
clear nmail1; nmail1{1} = struct('TYPE','POI1','MAIL',LPT');
clear chpo1 chpoe1; 
chpoe1{1} = struct('COMP','SCAL','UNIT','','XVAL',YVAL);
chpo1{1} = chpoe1;

% Test SEG2
YVAL2 = [10. 11.]';
LPT2  = [ino-1 ino];
MAIL2 = [ino-1 ino];
XVAL2 = ChpoToChamnog(YVAL2,LPT2,MAIL2);
clear mail2; mail2{1} = struct('TYPE','SEG2','MAIL',MAIL2);
clear nmail2; nmail2{1} = struct('TYPE','POI1','MAIL',LPT2');
clear chpo2 chpoe2; 
chpoe2{1} = struct('COMP','SCAL','UNIT','','XVAL',YVAL2);
chpo2{1} = chpoe2;

XCOOR = xcoor1;
XCOORC = XCOOR(1:end,:);

CTYPE = 'TRI3';
XPREC = 1.e-6;
% XPREC = 1.e-1;
% XPREC = 2.e-1;
OPTIO = 'SIMPLEX';
[ELEM,ALPHA] = NodesInMeshg(XCOOR,MAIL,CTYPE,XPREC,XCOORC,OPTIO);
ZVAL = ProjectChamnog(ELEM,ALPHA,XVAL);

CTYPE2 = 'SEG2';
XPREC2 = 1.e-6;
OPTIO2 = 'SIMPLEX';
[ELEM2,ALPHA2] = NodesInMeshg(XCOOR,MAIL2,CTYPE2,XPREC2,XCOORC,OPTIO2);
ZVAL2 = ProjectChamnog(ELEM2,ALPHA2,XVAL2);

numer2 = [1:ino];

[chpop,nmailp] = VectToChpo2(ZVAL,numer2,[1:length(numer2)]',[{'SCAL'}]);
[chpop2,nmailp2] = VectToChpo2(ZVAL2,numer2,[1:length(numer2)]',[{'SCAL'}]);

file1 = 'testdd6a3.pos';
fid=fopen(file1,'w');
  [chamno1,intg1] = ChpoToChamno3(chpo1,nmail1,mail1);
  error1 = WriteChamnoGMSH(xcoor1,mail1,chamno1,fid)
fclose(fid);
file1 = 'testdd6b3.pos';
fid=fopen(file1,'w');
  error1 = WriteChpoGMSH(xcoor1,nmailp,chpop,fid,'SCAL',[{'SCAL'}])
fclose(fid);

file1 = 'testdd6a2.pos';
fid=fopen(file1,'w');
  [chamno2,intg2] = ChpoToChamno3(chpo2,nmail2,mail2);
  error1 = WriteChamnoGMSH(xcoor1,mail2,chamno2,fid)
fclose(fid);
file1 = 'testdd6b2.pos';
fid=fopen(file1,'w');
  error1 = WriteChpoGMSH(xcoor1,nmailp2,chpop2,fid,'SCAL',[{'SCAL'}])
fclose(fid);

%file1 = 'testdd6.msh';
%clear nmailtot1; nmailtot1{1} = struct('TYPE','POI1','MAIL',[1:ino]');
%fid=fopen(file1,'w');
%  clear ListMesh1; ListMesh1{1} = nmailtot1;
%  error1 = WriteMeshGMSH(xcoor1,ListMesh1,fid)
%fclose(fid);





