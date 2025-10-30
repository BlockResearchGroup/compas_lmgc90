#include <med.h>
#define MESGERR 1
#include <med_utils.h>

void *medpre_LoadMeshFile(const char* filename);
void medpre_Close(void *_handle);

void medpre_PrintMeshInfo(void *_handle);

int medpre_GetSpaceDimension(void *_handle);
int medpre_GetMeshDimension(void *_handle);

int medpre_GetNumberOfNodes(void *_handle);
int medpre_GetNumberOfElements(void *_handle);

void medpre_GetAllCoordinates(void *_handle, double **_coords, int *_size);

/* return value: an LMGC90 geom type P1xxx, S2xxx, S3xxx, etc. */
char*medpre_GetElementGeomType(void *_handle, int eltIndex);

int  medpre_GetElementNbNodes(void *_handle, int eltIndex);
void medpre_GetElementNodes(void *_handle, int eltIndex, int **_conn, int *_size);

int   medpre_GetNbEltGroups(void *_handle);
char *medpre_GetEltGroupType(void *_handle, int groupIdx);
char *medpre_GetEltGroupName(void *_handle,int groupIdx);
int   medpre_GetEltGroupSize(void *_handle,int groupIdx);
void  medpre_GetEltGroupListOfIds(void *_handle, int groupIdx, int **_ids, int *_size);

int   medpre_GetNbNodeGroups(void *_handle);
int   medpre_GetNodeGroupSize(void *_handle,int groupIdx);
char *medpre_GetNodeGroupName(void *_handle,int groupIdx);
void  medpre_GetNodeGroupListOfIds(void *_handle, int groupIdx, int **_ids, int *_size);

