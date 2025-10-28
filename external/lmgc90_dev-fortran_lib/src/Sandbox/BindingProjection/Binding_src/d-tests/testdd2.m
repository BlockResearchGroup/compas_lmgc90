%mex -fortran -llapack ChpoToChamnog.F ChpoToChamno.f ddreal8toint.f ddinttoreal8.f zdanul.f place2.f

XCOOR = [0. 0.
         1. 0.
         0. 1.];
MAIL = [1 2 3];
XCOORC = XCOOR;
LPT = [1 2 3];
YVAL = [1.
        2.
        3.];
XVAL = ChpoToChamnog(YVAL,LPT,MAIL)

%mex -fortran -llapack NodesInMeshg.F NodesInMesh.f NodesInElement.f NodesInSimplex.f ddreal8toint.f ddinttoreal8.f ianul.f zdanul.f traces.f place2.f /usr/lib/libscalapack.a 

CTYPE = 'TRI3';
XPREC = 1.e-6;
OPTIO = 'SIMPLEX';
[ELEM,ALPHA] = NodesInMeshg(XCOOR,MAIL,CTYPE,XPREC,XCOORC,OPTIO)

%mex -fortran -llapack ProjectChamnog.F ProjectChamno.f ddreal8toint.f ddinttoreal8.f zdanul.f traces.f place2.f

YVAL1 = ProjectChamnog(ELEM,ALPHA,XVAL)
