
#include "clipper.hpp"
#include <cmath>
#include <cstdlib>
#include <cstdio>

using namespace ClipperLib;

extern "C" void clipper_intersection(double *p1, int *n1, int size_n1, double *p2, int *n2, int size_n2, double shrink1, double shrink2, double delta, double **p3, int **n3, int & size_n3, double ** area)
{
    int    i, j, ii, jj, jsum, nn;
    Path   path1, path2;
    Paths  poly1, poly2, solution;

    Clipper       clipper;
    ClipperOffset offset ;

    // to map from double to int
    double scale = std::pow(double(10.), double(10.));

    // first polygon, composed of several poltyopes
    jsum = 0;
    for (i = 0; i < size_n1; i++)
    {
        for (j = 0; j < (int)(n1[i]); j++)
        {
            jj = jsum + j;
            path1.push_back( IntPoint( (cInt)(p1[2*jj] * scale), (cInt)(p1[2*jj+1] * scale)   ) );
        };
        jsum += n1[i];
    }
    offset.Clear();
    offset.AddPath(path1, jtMiter, etClosedPolygon);
    offset.Execute(poly1, -1.*scale*shrink1);

    // second polygon, composed of several poltyopes
    jsum = 0;
    for (i = 0; i < size_n2; i++)
    {
        for (j = 0; j < (int)(n2[i]); j++)
        {
            jj = jsum + j;
            path2.push_back( IntPoint( (cInt)(p2[2*jj] * scale), (cInt)(p2[2*jj+1] * scale)   ) );
        };
        jsum += n2[i];
    }
    offset.Clear();
    offset.AddPath(path2, jtMiter, etClosedPolygon);
    offset.Execute(poly2, -1.*scale*shrink2);

    // calcul de l'intersection
    clipper.Clear();
    clipper.AddPaths(poly1, ptSubject, true);
    clipper.AddPaths(poly2, ptClip   , true);
    clipper.Execute(ctIntersection, solution, pftEvenOdd, pftEvenOdd);

    // delta : parametre de simplification
    if ( delta > 0. )
    {
        delta = delta * scale;
        CleanPolygons(solution, delta);
    };

    double sum_area = 0;
    int    sum_n3   = 0;
    size_n3  = solution.size();
    if (solution.size() > 0)
    {
        //*n3      = new int [ size_n3 ];
        //*area    = new double [ size_n3 ];
        *n3      = (int*) malloc(sizeof(int)*size_n3);
        *area    = (double*) malloc(sizeof(double)*size_n3);

        for (i = 0; i < solution.size(); ++i)
        {
            (*n3)[i]   = solution[i].size();
            (*area)[i] = Area(solution[i]) / (scale*scale);

            sum_n3   += (*n3)[i];
            sum_area += (*area)[i];
        };

        if (sum_area > 0.)
        {
            //*p3 = new double [ 2*sum_n3 ];
            *p3 = (double*) malloc(sizeof(double)*2*sum_n3);

            ii = 0;
            for (i = 0; i < solution.size(); ++i)
            {
                nn = solution[i].size();
                for (j = 0; j < nn; ++j)
                {
                    (*p3)[2*ii]   = solution[i][j].X / scale;
                    (*p3)[2*ii+1] = solution[i][j].Y / scale;
                    ++ii;
                };
            };
        };
    };
}

extern "C" void clipper_simplification(double *pin, int nin, double delta, double **pout, int & nout)
{
    int    i;
    Path   path_in, path_out;

    Clipper       clipper;
    ClipperOffset offset ;

    // to map from double to int
    double scale = std::pow(double(10.), double(10.));
    for (i = 0; i < nin; i++)
    {
        path_in.push_back(IntPoint((cInt)(pin[2*i] * scale),(cInt)(pin[2*i+1] * scale)));
    };

    // delta : parametre de simplification
    delta = delta * scale;
    CleanPolygon(path_in, path_out, delta);

    // le polygone peut etre oriente dans le mauvais sens et avoir une surface negative
    nout = 0;
    if ( Area(path_out) != double(0.) )
    {
        nout  = path_out.size();
        //*pout = new double [ 2*nout ];
        *pout = (double*) malloc(sizeof(double)*2*nout);
        
        for (i = 0; i < nout; ++i)
        {
            (*pout)[2*i]   = path_out[i].X / scale;
            (*pout)[2*i+1] = path_out[i].Y / scale;
            //fprintf(stdout,"out %2i - x = %f  -  y = %f\n",i+1,(*pout)[2*i],(*pout)[2*i+1]);
        };
    };
}

extern "C" void clipper_pointinpolygon(double px, double py, double *pin, int *nin, int size_nin, int &result)
{
    int      i, j, ii, jj, nn, jsum;
    IntPoint pt;
    Path     path;
    int      check;

    // to map from double to int
    double scale = std::pow(double(10.), double(10.));

    // a priori, the point is outside
    result = -1;
    // point to check
    pt = IntPoint( (cInt)(px*scale), (cInt)(py*scale) );
    // polygon, composed of several poltyopes
    jsum = 0;
    for (i = 0; i < size_nin; i++)
    {
        path.clear();
        for (j = 0; j < (int)(nin[i]); j++)
        {
            jj = jsum + j;
            path.push_back( IntPoint( (cInt)(pin[2*jj] *scale), (cInt)(pin[2*jj+1]) *scale ));
        };
        jsum += nin[i];
    }

    result = PointInPolygon(pt, path);
    if ( result < 1 )
    {
      result = -result - 1;
    }
}
