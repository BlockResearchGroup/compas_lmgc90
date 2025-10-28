#include "medpre.h"
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

/****************************************************/
/*                                                  */
/*      MED to lmgc90 Pre data structures           */
/*                                                  */
/****************************************************/

/* 
 * info for one group of elements, aka grel
 * (from a MED family with negative nmber)
 *
 * Dans MED, les familles forment une partition 
 * de l'ensemble des elements d'un maillage (noeud,
 * face, arete, maille). Chacun de ces elements
 * apartient a une et une seule famille (au moins 
 * la famille 0).
 *
 * numero d'une famille med
 *  numero >0 = famille de noeuds
 *  numero <0 = famille d'elements
 *  numero == 0: famille obligatoire (contient tout)
 */
struct grelInfo_t
{
  /* index of this grel (med index in the list of families) */
  int       idx;

 /* name of this grel */
  char      name[MED_NAME_SIZE+1];

  /* index dans les tableaux geonelt, geotype et geoconn pour ce grel */
  int       geoIdx;

  /* number of this grel (elts of this MED family have this number) */
  med_int   number;

  /* taille du grel, i.e. nelements (on supose un seul type d'element par famille) */
  int       size;

  /* indexes des elements du grel */
  int      *ids;

  /* nombre de groupes dans la famille MED */
  med_int   nGroup;

 /* liste des groupes dans la famille MED */
  char     *groupName;
};

void grelInfo_t_delete(struct grelInfo_t *grelInfo)
{
  if (!grelInfo) {return;}
  if (grelInfo->nGroup)
    {
      free(grelInfo->groupName);
      grelInfo->nGroup=0;
    }
  if (grelInfo->ids){free(grelInfo->ids);}
  grelInfo->size=0;
}

struct grelInfo_t *grelInfo_t_new(int ngrels)
{
  int i;
  struct grelInfo_t *greli;
  if (ngrels<=0) {return NULL;}
  greli = (struct grelInfo_t *)malloc(sizeof(struct grelInfo_t)*ngrels);
  if (!greli) {return greli;}
  for (i=0; i<ngrels;i++)
    {
      greli[i].nGroup=0;
      greli[i].groupName=NULL;
      greli[i].size=0;
      greli[i].ids=NULL;
    }
  return greli;
}

/* 
 * info for one group of nodes, aka grno
 * (from a MED family with positive nmber)
 *
 */
struct grnoInfo_t
{
  char     name[MED_LNAME_SIZE+1];
  med_int  number;
  int      nNodes;
  int     *nodeIds;
};
void grnoInfo_t_delete(struct grnoInfo_t *grnoInfo)
{
  if (!grnoInfo) {return;}
  if (grnoInfo->nodeIds)
    {
      free(grnoInfo->nodeIds);
      grnoInfo->nodeIds=NULL;
      grnoInfo->nNodes=0;
    }
}

struct grnoInfo_t *grnoInfo_t_new(int nGr)
{
  struct grnoInfo_t *grni;
  int i;
  grni= (struct grnoInfo_t*)malloc(sizeof(struct grnoInfo_t)*nGr);
  for (i=0;i<nGr;i++)
    {
      grni[i].nNodes=0;
      grni[i].nodeIds=NULL;
    }
  return grni;
}
/*
 * the opaque handle: info for one mesh in MED file
 */
typedef struct
{
  med_idt fid;
  int     nMesh;
  char    meshname[MED_NAME_SIZE+1];
  char    meshdescription[MED_COMMENT_SIZE+1];
  med_int spacedim;
  med_int meshdim;

  /* nombre total d'elements */
  int     neltot;

  /* description des types de mailles */
  int                 geontypes; /* number of geometrical types in the mesh */
  int                *geonelt;   /* number of elt of each geometrical type */
  med_geometry_type  *geotype;   /* geometrical type */
  med_int           **geoconn;   /* connectivity for elts of type geotype */

  /* liste des familles d'elements. */
  int                  nGrels;
  struct grelInfo_t   *grelInfo;

  /* liste des familles de noeuds */
  int                  nGrnos;
  struct grnoInfo_t   *grnoInfo;
  med_int             *famForNode;

} handle_t;

/*
 * handle constructor
 */
void handle_free(handle_t *h)
{
  if (!h) {return;}
  if (h->geonelt) {free(h->geonelt);}
  if (h->geotype) {free(h->geotype);}
  if (h->geoconn)
    {
      int i;
      for (i=0; i<h->geontypes;i++) { if (h->geoconn[i]) {free(h->geoconn[i]);} }
      free(h->geoconn);
    }
  if (h->famForNode) {free(h->famForNode);}
  if (h->nGrels)
    {
      int i;
      for (i=0; i<h->nGrels;i++) { grelInfo_t_delete(&h->grelInfo[i]); }
    }
  free(h->grelInfo);
  h->nGrels=0;
  if (h->nGrnos)
    {
      int i;
      for (i=0; i<h->nGrnos;i++) { grnoInfo_t_delete(&h->grnoInfo[i]); }
    }
  free(h->grnoInfo);
  h->nGrnos=0;
}

/*
 * handle destructor
 */
handle_t *handle_t_new(void)
{
  handle_t *h = (handle_t*) malloc(sizeof(*h));
  if (!h) {/* ERREUR */ return NULL;}
  h->neltot=0;
  h->geontypes=0;
  h->geonelt=NULL;
  h->geotype=NULL;
  h->geoconn=NULL;
  h->nGrels=0;
  h->nGrnos=0;
  h->famForNode=NULL;
  h->grelInfo=NULL;
  return h;
}

#define HANDLE_T(h,v)  handle_t *(h) = (handle_t *) (v)
#define NEW_HANDLE(h)  handle_t *(h) = handle_t_new()
#define FREE_HANDLE(h) handle_free(h)
#define MEDPRE_ASSERT(p) if (!(p)) {fprintf(stderr,"ASSERTION FAILED %s:%d\n",__FILE__,__LINE__);}

/****************************************************/
/*                                                  */
/*           MED to lmgc90 Pre logging              */
/*                                                  */
/****************************************************/

/* log level of library */
static int medpre_loglevel = 1;

/* types of log entries */
#define MEDPRE_TYPE_MESSAGE 0
#define MEDPRE_TYPE_ERREUR  1
#define MEDPRE_TYPE_DEBUG   2

static const char *medpre_header[3]= {"MEDPRE MESSAGE","MEDPRE ERREUR","MEDPRE DEBUG"};

/*
 * utility for logging
 */
void medpre_printf(int lvl, int type, const char*file,int line,const char *fmt, ...) 
{
  va_list argptr;
  if (lvl > medpre_loglevel) {return;}

  fprintf(stdout,"%s %s[%d]: ", medpre_header[type],file,line);

  va_start(argptr,fmt);
  vfprintf(stdout, fmt, argptr );
  va_end(argptr);
}

#define MEDPRE_ERREUR(lvl,fmt,...)  medpre_printf(lvl,MEDPRE_TYPE_ERREUR,__FILE__,__LINE__,fmt,##__VA_ARGS__)
#define MEDPRE_MESSAGE(lvl,fmt,...) medpre_printf(lvl,MEDPRE_TYPE_MESSAGE,__FILE__,__LINE__,fmt,##__VA_ARGS__)
#define MEDPRE_DEBUG(lvl,fmt,...)   medpre_printf(lvl,MEDPRE_TYPE_DEBUG,__FILE__,__LINE__,fmt,##__VA_ARGS__)

/****************************************************/
/*                                                  */
/*        MED to lmgc90 Pre private API             */
/*                                                  */
/****************************************************/

/*
 * for element number eltIndex, retrieve 
 * geometrical type, i.e. the index in handle->geoXXX arrays
 */
int _eltGetGeoIdx(handle_t *handle, int eltIndex)
{
  int geoIdx=0;
  int cumul=handle->geonelt[geoIdx];
  while (eltIndex>=cumul) {geoIdx++; cumul+=handle->geonelt[geoIdx];}
  return geoIdx;
}

/*
 * for element of global index eltIndex and geo index geo Idx,
 * retrieve element "local" index in his type
 */
int _eltGetIdx(handle_t *handle, int eltIndex, int geoIdx)
{
  while (geoIdx) {eltIndex = eltIndex - handle->geonelt[--geoIdx];}
  return eltIndex;
}

/*
 * maps: MED types to lmgc90 types
 */
char  *lmgc90Type0D[2]  = {"","P1xxx"};
char  *lmgc90Type1D[4]  = {"","","S2xxx","S3xxx"};
char  *lmgc90Type2D[9]  = {"","","","T3xxx","Q4xxx","","T6xxx","","Q8xxx"};
char  *lmgc90Type3D[11] = {"","","","","TE4xx","","PRI6x","","H8xxx","","TE10x"};

char  **lmgc90Types[4] ={
  lmgc90Type0D,
  lmgc90Type1D,
  lmgc90Type2D,
  lmgc90Type3D
};

/*
 * for MED type "type", retrieve lmgc90 type of element as a string
 */
char *_mapMEDgeomTypes(int type)
{
  int nnodes = type%100;
  int dim    = (type - nnodes)/100;
  return lmgc90Types[dim][nnodes];
}

/*
 * load the family number for all nodes (exactly one per node)
 */
void _loadNodeFamilies(void *_handle)
{
  HANDLE_T(handle,_handle);
  int nnodes,err,i;

  /* number of nodes */
  nnodes = medpre_GetNumberOfNodes(_handle);

  /* allocate space for family numbers */
  handle->famForNode = (med_int*)malloc(sizeof(*handle->famForNode)*nnodes);
  if (handle->famForNode==NULL)
    {
      /* ERREUR*/
      MEDPRE_ERREUR(1,"could not allocate\n");
      return;
    }

  /* load all family numbers */
  err=MEDmeshEntityFamilyNumberRd(handle->fid,handle->meshname, MED_NO_DT, MED_NO_IT,
                                  MED_NODE,MED_NONE,handle->famForNode);
  if (err< 0)
    {
     for (i=0; i<nnodes; i++) *(handle->famForNode+i) = 0;
    }

#ifdef DEBUG
  printf("Numeros de famille pour les noeuds:\n");
  for (i=0; i<nnodes; i++) printf("%d - ", *(handle->famForNode+i));
#endif
}

/*
 * load all element info:
 * geom types, number of elts by type, connectivities
 */
med_err _loadAllElements(void *_handle)
{
  HANDLE_T(handle,_handle);
  int itgeo,err,ntot=0;
  med_bool coordinatechangement;
  med_bool geotransformation;
  char geotypename[MED_NAME_SIZE+1];

  /* get number of geometrical types */
  handle->geontypes = MEDmeshnEntity(handle->fid,handle->meshname, MED_NO_DT, MED_NO_IT, MED_CELL,MED_GEO_ALL,
                                     MED_CONNECTIVITY,
                                     MED_NODAL,
                                     &coordinatechangement,&geotransformation);
  if (handle->geontypes<0)
    {
      /* ERREUR */
      MEDPRE_ERREUR(1,"Could not read mesh geometric types from fid %d, meshname <%s>\n",handle->fid,handle->meshname);
      return -1;
    }

  /* allocate space for geonelt[], geontype[], geoconn[] */
  handle->geonelt=(int*)malloc(sizeof(*(handle->geonelt))*handle->geontypes);
  if (!handle->geonelt)
    {
      /* ERREUR */
      MEDPRE_ERREUR(1,"Could not allocate\n");
      return -1;
    }
  handle->geotype=(int*)malloc(sizeof(*(handle->geotype))*handle->geontypes);
  if (!handle->geotype)
    {
      /* ERREUR */
      MEDPRE_ERREUR(1,"Could not allocate\n");
      return -1;
    }
  handle->geoconn=(med_int**)malloc(sizeof(*(handle->geoconn))*handle->geontypes);
  if (!handle->geoconn)
    {
      /* ERREUR */
      MEDPRE_ERREUR(1,"Could not allocate\n");
      return -1;
    }
  for (itgeo=0; itgeo<handle->geontypes; itgeo++) {handle->geoconn[itgeo]=NULL;}

  /* load for each type */
  for (itgeo=0; itgeo<handle->geontypes; itgeo++)
    {
      /* get geometry type */
      err = MEDmeshEntityInfo(handle->fid,handle->meshname, MED_NO_DT, MED_NO_IT,MED_CELL,itgeo+1,
                              geotypename,&handle->geotype[itgeo]);
      if (err<0)
        {
          /* ERREUR */
          MEDPRE_ERREUR(1,"Could not retrieve info for %s\n",_mapMEDgeomTypes(handle->geotype[itgeo]));
          return -1;
        }

      /* how many cells of this type */
      handle->geonelt[itgeo] = MEDmeshnEntity(handle->fid,handle->meshname, MED_NO_DT, MED_NO_IT, MED_CELL,
                                              handle->geotype[itgeo],
                                              MED_CONNECTIVITY, MED_NODAL,&coordinatechangement,
                                              &geotransformation);
      if (handle->geonelt[itgeo]<0)
        {
          /* ERREUR */
          MEDPRE_ERREUR(1,"Could not retrieve ncells of type %s\n",_mapMEDgeomTypes(handle->geotype[itgeo]));
          return -1;
        }
      
      ntot += handle->geonelt[itgeo];
    }
  handle->neltot=ntot;
  return 0;
}

/*
 * load the family index for all elements of type itgeo
 * returned array must be freed by caller
 */
med_int *_loadEltFamilies(void *_handle, int itgeo)
{
  HANDLE_T(handle,_handle);
  int      nelt,err,i;
  med_int *familynumbers;

  /* number of elts of this type */
  nelt=handle->geonelt[itgeo];

  /* allocate storage */
  familynumbers = (med_int *) malloc(sizeof(*familynumbers)*nelt);
  if (familynumbers==NULL)
    {
      /* ERREUR*/
      MEDPRE_ERREUR(1,"could not allocate\n");
      return NULL;
    }

  /* load the numbers for this type */
  err=MEDmeshEntityFamilyNumberRd(handle->fid,handle->meshname, MED_NO_DT, MED_NO_IT,
                                  MED_CELL,handle->geotype[itgeo],familynumbers );

  /* if error, nullify */
  if (err < 0) {for (i=0; i<nelt; i++) *(familynumbers+i) = 0;}

#ifdef DEBUG
  printf("\nNumeros de famille pour les %d cellules de type %s:\n",nelt,_mapMEDgeomTypes(handle->geotype[itgeo]));
  printf("size of med_int = %ld\n",sizeof(*familynumbers));
  for (i=0; i<nelt; i++) printf("%d - ", *(familynumbers+i));
  printf("\n");
#endif

  return familynumbers;
}

/*
 * load all family info, for all families
 */
void _loadFamiliesInfo(void *_handle)
{
  HANDLE_T(handle,_handle);
  int eltFam,nodFam;
  med_int famit,nfamilies;
  int err,i,cumul;
  int itgeo;
  struct grelInfo_t *grelInfo;

  /* les infos pour un famille MED sont */
  med_int nGroup;
  med_int number;
  char    name[MED_NAME_SIZE+1];
  char   *groupName;

  /* nombre total de familles dans le maillage */
  nfamilies = MEDnFamily(handle->fid,handle->meshname);
  if (nfamilies<0)
    {
      /* ERREUR */
      MEDPRE_ERREUR(1,"could not retrieve number of families in med file\n");
      return;
    }

  /* on  fait un premier tour pour compter les familles de noeuds,
   * et les familles d'elements */
  handle->nGrels = 0;
  handle->nGrnos = 0;
  for (famit=1; famit<=nfamilies; famit++)
    {
      /* nombre de groupes */
      nGroup = MEDnFamilyGroup(handle->fid,handle->meshname,famit);
      if (nGroup<0)
        {
          /* ERREUR */
          MEDPRE_ERREUR(1,"ERREUR sur le nombre de groupes lors de l'acces a la famille %d\n",famit);
          continue;
        }
      groupName = (char*)malloc(sizeof(char)*MED_LNAME_SIZE*nGroup+1);
      if (!groupName)
	{
	  /* ERREUR */
	  MEDPRE_ERREUR(1,"pas assez d'espace memoire pour %d groupes\n",nGroup);
	  continue;
	}
      /* le reste des infos */
      err=MEDfamilyInfo(handle->fid,handle->meshname,famit,name,&number,groupName);
      if (err<0)
        {
          /* ERREUR */
          MEDPRE_ERREUR(1,"ERREUR lors de l'acces a la famille %d\n",famit);
          continue;
        }
      /* la famille de numero 0 ne compte pas*/
      if ((int)number>(int)0)
	{
	  /* c'est une famille de neuds */
	  handle->nGrnos++;
	}
      else if ((int)number<(int)0)
	{
	  /* c'est une famille d'elements */
	  handle->nGrels++;
	}
      free(groupName);
    }

#ifdef DEBUG
  printf("Maillage a %d familles d'elements\n",handle->nGrels);
  printf("        et %d familles de noeuds \n",handle->nGrnos);
#endif

  /* espace de stockage pour les infos des familles d'elements */
  if (handle->nGrels==0)
    {
      handle->grelInfo=NULL;
    }
  else
    {
      handle->grelInfo=grelInfo_t_new(handle->nGrels);
    }
  /* espace de stockage pour les infos des familles de noeuds */
  if (handle->nGrnos==0)
    {
      handle->grnoInfo=NULL;
    }
  else
    {
      handle->grnoInfo=grnoInfo_t_new(handle->nGrnos);
    }

  /* pour chaque famille, on lit les groupes et les infos */
  eltFam=0; nodFam=0;
  for (famit=1; famit<=nfamilies; famit++)
    {
      /* nombre de groupes */
      nGroup = MEDnFamilyGroup(handle->fid,handle->meshname,famit);
      if (nGroup<0)
        {
          /* ERREUR */
          MEDPRE_ERREUR(1,"ERREUR sur le nombre de groupes lors de l'acces a la famille %d\n",famit);
          continue;
        }
      groupName = (char*)malloc(sizeof(char)*MED_LNAME_SIZE*nGroup+1);
      if (!groupName)
	{
	  /* ERREUR */
	  MEDPRE_ERREUR(1,"pas assez d'espace memoire pour %d groupes\n",nGroup);
	  continue;
	}
      /* infos de la famille */
      err=MEDfamilyInfo(handle->fid,handle->meshname,famit,name,&number,groupName);
      if (err<0)
        {
          /* ERREUR */
          MEDPRE_ERREUR(1,"ERREUR lors de l'acces a la famille %d\n",famit);
          continue;
        }
      /* la famille de numero 0 ne compte pas*/
      if ((int)number>(int)0)
	{
	  /* c'est une famille de noeuds */
	  struct grnoInfo_t *grnoInfo=handle->grnoInfo+nodFam;
	  nodFam++;
	  grnoInfo->number    = number;
	  strncpy(grnoInfo->name,groupName,sizeof(char)*(MED_LNAME_SIZE+1));
	}
      else if ((int)number<(int)0)
	{
	  /* c'est une famille d'elements */
	  struct grelInfo_t *grelInfo=handle->grelInfo+eltFam;

	  grelInfo->idx       = famit;
	  grelInfo->nGroup    = nGroup;
	  grelInfo->groupName = groupName;
	  grelInfo->number    = number;
	  strncpy(grelInfo->name,name,sizeof(char)*(MED_NAME_SIZE+1));
#ifdef DEBUG
      printf("    famille d'elets %d (medfam %d)\n",eltFam,grelInfo->idx);
      printf("                   s appelle    <%s>\n",grelInfo->name);
      printf("                   number    =  %d\n",grelInfo->number);
      printf("                   ngroup    =  %d\n",grelInfo->nGroup);
      printf("                   groupname =  <%s>\n",grelInfo->groupName);
#endif
      eltFam++;
	}
      else if ((int)number==0) {free(groupName);}
      /* not implemented: plusieurs groupes (plusieurs types de maille) par famille */
      /* if (familyInfo->nGroup>1) {MEDPRE_ERREUR(1,"ERROR more than one group per family in MED file\n");return;} */
    }

  /* On lit le numero de famille pour tous les noeuds */
  _loadNodeFamilies(_handle);

  /* on lit le numero de famille pour les elements (arete, face, maille) */
  for (itgeo=0;itgeo<handle->geontypes;itgeo++)
    {
      int      elt;
      med_int *family;
      int      nelt,check;

      /* on met dans un espace temporaire */
      family=_loadEltFamilies(_handle,itgeo);

      nelt=handle->geonelt[itgeo];
      for (elt=0;elt<nelt;elt++)
        {
          med_int fam=family[elt];
          famit=0;
          if (!fam) {continue;}
          grelInfo = handle->grelInfo;
          /* on cherche la famille de l'element */
          while ( (famit<handle->nGrels) && (fam!=grelInfo->number) )
            {
	      grelInfo++;
              famit++;
            }
          /* si on ne la trouve pas, on a un probleme */
          if (famit==(med_int)handle->nGrels)
            {
              /* ERREUR */
              MEDPRE_ERREUR(1,"cannot find family %d in file\n",fam);
              grelInfo->size=0;
              continue;
            }
          /* on l'a trouvee, on met a jour les champs
           * taille de famille, et type geometrique. */
          grelInfo->geoIdx=itgeo;
          grelInfo->size++;
          MEDPRE_ASSERT(grelInfo->ids==NULL);
        }

      /* on suppose un seul type d'element par famille, 
         donc dans family, on a des familles complètes,
         on les remplit maintenant */

      /* on alloue les listes d'identifiants dans les grelInfo_t,
         et on rempli chaque famille en parcourant a chaque fois tous les elets, 
         c'est plus long mais tellement plus simple */
      famit=0;
      for (famit=0;famit<handle->nGrels;famit++)
        {
          grelInfo=handle->grelInfo+famit;

          if (grelInfo->size==0) {continue;}
          if (grelInfo->ids) {continue;}
          if (grelInfo->number==0) {continue;} // cette famille est vide (normalmeenton l a deja eliminee) */
          if (grelInfo->number>0) {continue;} // on ne traite pas les familles de noeuds pour l'instant

#ifdef DEBUG
          printf("Chargement de la famille <%s> de %d elements\n",grelInfo->groupName,grelInfo->size);
#endif
          /* on alloue la famille car size!=0 et ids==NULL */
          grelInfo->ids=(int *)malloc(sizeof(*grelInfo->ids)*grelInfo->size);
          if (!grelInfo->ids)
            {
              /* ERREUR */
              MEDPRE_ERREUR(1,"could no allocate list of %d ids\n",grelInfo->size);
              grelInfo->size=0;
              continue;
            }

          /* on rempli les listes d identifiants pour cette famille
           * en parcourant tous les elements */
          nelt=handle->geonelt[grelInfo->geoIdx];
	  cumul=0;
	  i=0;
	  while (i<grelInfo->geoIdx) { cumul+=handle->geonelt[i++];}
          check=grelInfo->size;
          grelInfo->size=0;
          for (elt=0;elt<nelt;elt++)
            {
              if (family[elt]==grelInfo->number)
                {
                  grelInfo->ids[grelInfo->size++]=cumul+elt;
                }
            }
          MEDPRE_ASSERT(grelInfo->size==check);

          /* famille suivante */
        }
        free(family);
    }
}

/****************************************************/
/*                                                  */
/*             MED to lmgc90 Pre API                */
/*                                                  */
/****************************************************/


void *medpre_LoadMeshFile(const char* filename)
{
  med_sorting_type sortingtype; /*MED_SORT_DTIT,MED_SORT_ITDT,MED_SORT_UNDEF */
  med_int          nstep;
  med_axis_type    axistype;    /* MED_CARTESIAN,MED_CYLINDRICAL,MED_SPHERICAL,MED_UNDEF_AXIS_TYPE */
  char            *axisname;
  char            *unitname;
  char             dtunit[MED_SNAME_SIZE+1]; /* */
  med_mesh_type    meshtype;    /* MED_UNSTRUCTURED_MESH,MED_STRUCTURED_MESH,MED_UNDEF_MESH_TYPE */

  NEW_HANDLE(handle);

  /* open file */
  handle->fid=MEDfileOpen(filename,MED_ACC_RDONLY);
  if (handle->fid<0)
    {
      /* ERREUR */
      MEDPRE_ERREUR(1,"Could not load file %s\n",filename);
      return NULL;
    }

  /* check that there is only one mesh in the file */
  handle->nMesh=MEDnMesh(handle->fid);
  MEDPRE_ASSERT(handle->nMesh==1);

  /* load mesh info */
  /* memory allocation as if dimension==3 */
  if ((axisname  = (char*) malloc(MED_SNAME_SIZE*3+1)) == NULL) {
    /* ERREUR */
    MEDPRE_ERREUR(1,"Could not allocate\n");
    return NULL;
  }
  if ((unitname  = (char*) malloc(MED_SNAME_SIZE*3+1)) == NULL) {
    /* ERREUR */
    MEDPRE_ERREUR(1,"Could not allocate\n");
    return NULL;
  }

  if (MEDmeshInfo(handle->fid, 1, handle->meshname, &handle->spacedim,
                  &handle->meshdim, &meshtype, handle->meshdescription, 
                  dtunit, &sortingtype, &nstep, &axistype, axisname, unitname) < 0) {
    /* ERREUR */
    MEDPRE_ERREUR(1,"Could not retrieve mesh info\n");
    free(axisname); free(unitname);
    return NULL;
  }

  free(axisname); free(unitname);

  /* load element infos */
  if (_loadAllElements(handle))
    {
      MEDPRE_ERREUR(1,"Could not load element info\n");
      FREE_HANDLE(handle);
      return NULL;
    }

  return (void*)handle;
}
void medpre_Close(void *_handle)
{
  int err;
  HANDLE_T(handle,_handle);
  MEDPRE_ASSERT(handle);
  err = MEDfileClose(handle->fid);
  FREE_HANDLE(handle);
}

void medpre_PrintMeshInfo(void *_handle)
{
  HANDLE_T(handle,_handle);
  if (handle->meshdescription) {printf("     Comment: %s\n",handle->meshdescription);}
}

int medpre_GetSpaceDimension(void *_handle)
{
  HANDLE_T(handle,_handle);
  return (int)handle->spacedim;
}
int medpre_GetMeshDimension(void *_handle)
{
  HANDLE_T(handle,_handle);
  return (int)handle->meshdim;
}

int medpre_GetNumberOfNodes(void *_handle)
{
  HANDLE_T(handle,_handle);
  med_int  nnodes;
  med_bool coordinatechangement;
  med_bool geotransformation;
  /* read how many nodes in the mesh */
  if ((nnodes = MEDmeshnEntity(handle->fid, handle->meshname,
                               MED_NO_DT, MED_NO_IT, MED_NODE, MED_NO_GEOTYPE,
                               MED_COORDINATE, MED_NO_CMODE,&coordinatechangement,
                               &geotransformation)) < 0) {
    /* ERREUR */
    return -1;
  }
  return (int) nnodes;
}

void medpre_GetAllCoordinates(void *_handle, double **_coords, int *_size)
{
  HANDLE_T(handle,_handle);
  int nnodes;
  nnodes = medpre_GetNumberOfNodes(_handle);

  MEDPRE_ASSERT(sizeof(double)==sizeof(med_float));

  *_size = (int) (nnodes*handle->meshdim);
  *_coords=(double *)malloc(sizeof(double)*(*_size));

  if (MEDmeshNodeCoordinateRd(handle->fid,handle->meshname, MED_NO_DT, MED_NO_IT, MED_FULL_INTERLACE,
                              (*_coords)) < 0) {
    /* ERREUR */
    MEDPRE_ERREUR(1,"Could not read mesh coordinates\n");
    return;
  }
  return;
}

int medpre_GetNumberOfElements(void *_handle)
{
  HANDLE_T(handle,_handle);
  /* return total number of elements */
  return handle->neltot;
}

void medpre_GetElementNodes(void *_handle, int eltIndex, int **_conn, int *_size)
{
  HANDLE_T(handle,_handle);

  int i,start,err;
  int geoIdx=0;
  int nodespercell;
  med_geometry_type geotype;

  *_conn=NULL;
  *_size=0;
  
  geoIdx       = _eltGetGeoIdx(handle,eltIndex);
  eltIndex     = _eltGetIdx(handle,eltIndex,geoIdx);
#ifdef DEBUG
  printf("elt Index = %d: ",eltIndex);
  printf("    geoidx=%d  et  eltIndex=%d\n",geoIdx,eltIndex);
#endif

  /* get geometry type */
  geotype=handle->geotype[geoIdx];

  /* number of cells per elt in this category */
  nodespercell = geotype%100;

  /* load only once */
  if (!handle->geoconn[geoIdx])
    {
      /* allocate space in handle for connectivity arrays */
      handle->geoconn[geoIdx] = (med_int *) malloc(sizeof(med_int)*handle->geonelt[geoIdx]*nodespercell);
      if (!handle->geoconn[geoIdx])
        {
          /* ERREUR */
          MEDPRE_ERREUR(1,"could not allocate\n");
          return;
        }
      
      /* read cells connectivity in the mesh */
      err = MEDmeshElementConnectivityRd(handle->fid,handle->meshname, MED_NO_DT, MED_NO_IT, MED_CELL,
                                         geotype, MED_NODAL, MED_FULL_INTERLACE, handle->geoconn[geoIdx]);
      if (err<0)
        {
          /* ERREUR */
          MEDPRE_ERREUR(1,"could not read connectivity for %s\n",_mapMEDgeomTypes(handle->geotype[geoIdx]));
          return;
        }
    }

  /* retrieve connectivity for eltIndex */
  *_size=nodespercell;
  *_conn=(int *)malloc(sizeof(int)*nodespercell);
  start = eltIndex*nodespercell;
  for (i=0; i<nodespercell; i++)
    {
      /* python wants 0-offset indices, hence -1 */
      (*_conn)[i] = (int)handle->geoconn[geoIdx][start+i] - 1;
    }

  return;
}

char *medpre_GetElementGeomType(void *_handle, int eltIndex)
{
  int geoIdx = 0;
  HANDLE_T(handle,_handle);
  geoIdx   = _eltGetGeoIdx(handle,eltIndex);
  /* mapping  */
  return _mapMEDgeomTypes(handle->geotype[geoIdx]);
}

int  medpre_GetElementNbNodes(void *_handle, int eltIndex)
{
  int geoIdx = 0;
  HANDLE_T(handle,_handle);
  geoIdx = _eltGetGeoIdx(handle,eltIndex);
  return handle->geotype[geoIdx]%100;
}

/* En realite, on veut le nombre de familles */
int medpre_GetNbEltGroups(void *_handle)
{
  HANDLE_T(handle,_handle);
  if (!handle->nGrels) {_loadFamiliesInfo(handle);}

  return handle->nGrels;
}

char *medpre_GetEltGroupType(void *_handle, int groupIdx)
{
  HANDLE_T(handle,_handle);
  struct grelInfo_t *grelInfo;

  if (!handle->nGrels) {_loadFamiliesInfo(handle);}
  grelInfo=handle->grelInfo+groupIdx;
  return _mapMEDgeomTypes(handle->geotype[grelInfo->geoIdx]);
}

int  medpre_GetEltGroupSize(void *_handle,int groupIdx)
{
  HANDLE_T(handle,_handle);
  struct grelInfo_t *grelInfo;

  if (!handle->nGrels) {_loadFamiliesInfo(handle);}
  grelInfo=handle->grelInfo+groupIdx;
  return grelInfo->size;
}

void medpre_GetEltGroupListOfIds(void *_handle, int groupIdx, int **_ids, int *_size)
{
  HANDLE_T(handle,_handle);
  int i;
  struct grelInfo_t *grelInfo;
  if (!handle->nGrels) {_loadFamiliesInfo(handle);}

  grelInfo=handle->grelInfo+groupIdx;
  *_size=grelInfo->size;
  if (*_size==0) {*_ids=NULL;return;}
  *_ids=(int *)malloc(sizeof(**_ids)*(*_size));
  if (!*_ids)
    {
      /* ERREUR */
      MEDPRE_ERREUR(1,"could not allocate list of %d ids\n",*_size);
      *_size=0;
      return;
    }
  for (i=0; i<*_size;i++) {(*_ids)[i] = grelInfo->ids[i];}
  return;
}

char *medpre_GetEltGroupName(void *_handle,int groupIdx)
{
  HANDLE_T(handle,_handle);
  struct grelInfo_t *grelInfo;
  if (!handle->nGrels) {_loadFamiliesInfo(handle);}

  grelInfo=handle->grelInfo+groupIdx;
  return grelInfo->groupName;
}

/*
 * separate API for groups of node
 */ 
int medpre_GetNbNodeGroups(void *_handle)
{
  HANDLE_T(handle,_handle);
  if (!handle) {return -1;}
  return handle->nGrnos;
}

int  medpre_GetNodeGroupSize(void *_handle,int groupIdx)
{
  HANDLE_T(handle,_handle);
  struct grnoInfo_t *info;
  int node,nnotot;
  int nnodes=0;
  if (!handle) {return -1;}
  if (groupIdx>=handle->nGrnos)
    {
      /* ERREUR */
      MEDPRE_ERREUR(1,"Wrong index %d for node group index, max is %d\n",groupIdx,handle->nGrnos-1);
      return -1;
    }
  /* on parcourt les numeros de familles, et on compte */
  nnotot = medpre_GetNumberOfNodes(_handle);
  info = handle->grnoInfo+groupIdx;
  for (node=0; node<nnotot;node++)
    {
      med_int fam=handle->famForNode[node];
      if (fam==(med_int)(info->number)) {nnodes++;}
    }
  return nnodes;
}
char *medpre_GetNodeGroupName(void *_handle,int groupIdx)
{
  HANDLE_T(handle,_handle);
  if (!handle) {return NULL;}
  if (groupIdx>=handle->nGrnos)
    {
      /* ERREUR */
      MEDPRE_ERREUR(1,"Wrong index %d for node group index, max is %d\n",groupIdx,handle->nGrnos-1);
    }
  return handle->grnoInfo[groupIdx].name;
}
void medpre_GetNodeGroupListOfIds(void *_handle, int groupIdx, int **_ids, int *_size)
{
  HANDLE_T(handle,_handle);
  struct grnoInfo_t *info;
  int node,grnode,nnotot;
  if (!handle) {return;}
  if (groupIdx>=handle->nGrnos)
    {
      /* ERREUR */
      MEDPRE_ERREUR(1,"Wrong index %d for node group index, max is %d\n",groupIdx,handle->nGrnos-1);
    }

  /* on alloue et rempli la liste des identifiants, a la demande */
  nnotot = medpre_GetNumberOfNodes(_handle);
  info=handle->grnoInfo+groupIdx;
  *_size = medpre_GetNodeGroupSize(_handle,groupIdx);
  *_ids  = (int *) malloc(sizeof(**_ids)*(*_size));
  if (!*_ids)
    {
      /* ERREUR */
      MEDPRE_ERREUR(1,"Could not allocate %d integers for list of ids of group %d",(*_size),groupIdx);
      *_size=0;
      return;
    }
  /* on parcourt les numeros de famille */
  grnode=0;
  for (node=0; node<nnotot;node++)
    {
      med_int fam=handle->famForNode[node];
      if (fam==(med_int)(info->number))
	{
	  *_ids[grnode++]=node;
	}
    }
  return;
}
