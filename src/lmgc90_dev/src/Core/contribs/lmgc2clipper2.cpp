
#include "clipper2/clipper.h"
#include <cmath>
#include <cstdlib>
#include <cstdio>
#include <iostream>

using namespace Clipper2Lib;

extern "C" void clipper_intersection(double *p1, int *n1, int size_n1, double *p2, int *n2, int size_n2, double shrink1, double shrink2, double delta, double **p3, int **n3, int &size_n3, double **area)
{
    int     i, j, ii, jj, nn, jsum;
    PathD   path;
    PathsD  paths1, paths2;
    PathsD  poly1 , poly2, solution;
    
    // first polygon, composed of several poltyopes
    jsum = 0;
    for (i = 0; i < size_n1; i++)
    {
        path.clear();
        for (j = 0; j < (int)(n1[i]); j++)
        {
            jj = jsum + j;
            path.push_back( PointD( (double)(p1[2*jj]), (double)(p1[2*jj+1])   ) );
        };
        paths1.push_back(path);
        jsum += n1[i];
    }
    poly1 = InflatePaths(paths1, -1.*shrink1, JoinType::Miter, EndType::Polygon, 2.0, 8);

    // second polygon, composed of several poltyopes
    jsum = 0;
    for (i = 0; i < size_n2; i++)
    {
        path.clear();
        for (j = 0; j < (int)(n2[i]); j++)
        {
            jj = jsum + j;
            path.push_back( PointD( (double)(p2[2*jj]), (double)(p2[2*jj+1])   ) );
        };
        paths2.push_back(path);
        jsum += n2[i];
    }
    poly2 = InflatePaths(paths2, -1.*shrink2, JoinType::Miter, EndType::Polygon, 2.0, 8);

    // calcul de l'intersection
    solution = Intersect(poly1, poly2, FillRule::NonZero, 8);

    // delta : parametre de simplification
    delta    = std::max(delta, 1e-8);
    solution = SimplifyPaths(solution, delta);

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
            (*area)[i] = Area(solution[i]);

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
                    (*p3)[2*ii]   = solution[i][j].x;
                    (*p3)[2*ii+1] = solution[i][j].y;
                    ++ii;
                };
            };
        };
    };
}

extern "C" void clipper_simplification(double *pin, int nin, double delta, double **pout, int &nout)
{
    int     i;
    PathD   path;
    PathsD  paths;
    
    for (i = 0; i < nin; i++)
    {
        path.push_back( PointD( (double)(pin[2*i]), (double)(pin[2*i+1]) ) );
    };
    paths.push_back(path);

    // delta : parametre de simplification
    delta = std::max(delta, 1e-8);
    paths = SimplifyPaths(paths, delta);

    // a priori, la solution de contient qu'un polytope : paths[0]
    nout = 0;
    if ( paths.size() == 1 )
    {
        nout  = paths[0].size();
        //*pout = new double [ 2*nout ];
        *pout = (double*) malloc(sizeof(double)*2*nout);

        for (i = 0; i < nout; ++i)
        {
            (*pout)[2*i]   = paths[0][i].x;
            (*pout)[2*i+1] = paths[0][i].y;
        };
    };
}

extern "C" void clipper_pointinpolygon(double px, double py, double *pin, int *nin, int size_nin, int &result)
{
    int     i, j, ii, jj, nn, jsum;
    PointD  pt;
    PathD   path;
    PointInPolygonResult  check;

    // a priori, the point is outside
    result = -1;
    // point to check
    pt = PointD( (double)(px), (double)(py) );
    // polygon, composed of several poltyopes
    jsum = 0;
    for (i = 0; i < size_nin; i++)
    {
        path.clear();
        for (j = 0; j < (int)(nin[i]); j++)
        {
            jj = jsum + j;
            path.push_back( PointD( (double)(pin[2*jj]), (double)(pin[2*jj+1])   ) );
        };
        jsum += nin[i];
        check = PointInPolygon(pt, path);
        if (check == Clipper2Lib::PointInPolygonResult::IsOn)
        {
            result  =  0;
            break;
        };
        if (check == Clipper2Lib::PointInPolygonResult::IsInside)
        {
            result *= -1;
        };
    };
}
