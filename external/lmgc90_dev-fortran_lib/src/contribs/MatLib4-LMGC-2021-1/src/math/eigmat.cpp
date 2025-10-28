/*
 *  $Id: eigmat.cpp 143 2014-04-18 07:12:34Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2014, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "eigmat.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

// std C library
#include <cmath>
#include <cstring>
// local
#ifdef MATLIB_USE_LAPACK
#include "lapack.h"
#endif
#include "MathUtils.h"


/**
 * Compute eigenvalues + left and right eigenvectors (3x3 matrix).
 */
int MATLIB_NAMESPACE eigenLR3(const double A[],double eigVal[],
                              double eigVecL[][3],double eigVecR[][3]) {

  int i,j,k,ij;
  double g[9],gInv[9];

  // compute eigenvalues and right eigenvectors
  int test = eigen3(A,eigVal,eigVecR);
  if (!test) return 0;

  // compute left eigenvectors
  for (i=0, ij=0; i < 3; i++)
    for (j=0; j < 3; j++, ij++) {
      g[ij] = 0.e0;
      for (k=0; k < 3; k++) g[ij] += eigVecR[k][i]*eigVecR[k][j];
    }
  invmat3(g,gInv);
  for (k=0; k < 3; k++)
    for (i=0, ij=0; i < 3; i++) {
      double val = 0.e0;
      for (j=0; j < 3; j++, ij++) val += gInv[ij]*eigVecR[k][j];
      eigVecL[k][i] = val;
    }

  return 1;
}


/**
 * Compute eigenvalues + left and right eigenvectors (2x2 matrix).
 */
int MATLIB_NAMESPACE eigenLR2(const double A[],double eigVal[],
                              double eigVecL[][2],double eigVecR[][2]) {

  int i,j,k,ij;
  double g[4],gInv[4];

  // compute eigenvalues and right eigenvectors
  int test = eigen2(A,eigVal,eigVecR);
  if (!test) return 0;

  // compute left eigenvectors
  for (i=0, ij=0; i < 2; i++)
    for (j=0; j < 2; j++, ij++) {
      g[ij] = 0.e0;
      for (k=0; k < 2; k++) g[ij] += eigVecR[k][i]*eigVecR[k][j];
    }
  invmat2(g,gInv);
  for (k=0; k < 2; k++)
    for (i=0, ij=0; i < 2; i++) {
      double val = 0.e0;
      for (j=0; j < 2; j++, ij++) val += gInv[ij]*eigVecR[k][j];
      eigVecL[k][i] = val;
    }

  return 1;
}

/**
 * Compute eigenvalues + right eigenvectors.
 */
int MATLIB_NAMESPACE eigen3(const double A[],double eigVal[],
                            double eigVec[][3]) {
  double M[9],wi[3],V[9];

  // copy A into M
  std::memcpy(M,A,9*sizeof(double));

  // solve eigenproblem
  int test;
#ifndef MATLIB_USE_LAPACK
  int work1[3];
  double work2[3];
  test = eigen(3,3,M,eigVal,wi,V,work1,work2);
#else
  char no='N',yes='V';
  LAPACK_INTEGER size=3,wsz=50,ierr;
  LAPACK_DOUBLE dummy[9],work[50];
  // left eigenvectors of transposed matrix
  // (this is how M looks with fortran storage)
  FORTRAN(dgeev)(&yes,&no,&size,M,&size,eigVal,wi,V,&size,dummy,&size,
                 work,&wsz,&ierr);
  test = !ierr;
#endif
  if (!test) return test;

  // check for complex eigenvalues
  if ((wi[0] != 0.e0) || (wi[1] != 0.e0) || (wi[2] != 0.e0)) return 0;

  // get eigenvectors
  double *p=V;
  for (int i=0; i < 3; i++, p+=3)
    std::memcpy(eigVec[i],p,3*sizeof(double));

  return 1;
}

/**
 * Compute eigenvalues + right eigenvectors (2x2 matrix).
 */
int MATLIB_NAMESPACE eigen2(const double A[],double eigVal[],
                            double eigVec[][2]) {
  double M[4],wi[2],V[4];

  // copy A into M
  std::memcpy(M,A,4*sizeof(double));

  // solve eigenproblem
  int test;
#ifndef MATLIB_USE_LAPACK
  int work1[2];
  double work2[2];
  test = eigen(2,2,M,eigVal,wi,V,work1,work2);
#else
  char no='N',yes='V';
  LAPACK_INTEGER size=2,wsz=50,ierr;
  LAPACK_DOUBLE dummy[4],work[50];
  // left eigenvectors of transposed matrix
  // (this is how M looks with fortran storage)
  FORTRAN(dgeev)(&yes,&no,&size,M,&size,eigVal,wi,V,&size,dummy,&size,
                 work,&wsz,&ierr);
  test = !ierr;
#endif
  if (!test) return test;

  // check for complex eigenvalues
  if ((wi[0] != 0.e0) || (wi[1] != 0.e0)) return 0;

  // get eigenvectors
  double *p=V;
  for (int i=0; i < 2; i++, p+=2)
    std::memcpy(eigVec[i],p,2*sizeof(double));

  return 1;
}

/**
 * Solve eigenproblem for a real general matrix (based on lapack)
 */
void balanc(int,int,double*,int&,int&,double[]);
void balbak(int,int,int,int,double[],int,double*);
void cdiv(double,double,double,double,double&,double&);
void elmhes(int,int,int,int,double*,int[]);
void eltran(int,int,int,int,double*,int[],double*);
int hqr(int,int,int,int,double*,double[],double[],double*);

int MATLIB_NAMESPACE eigen(int nm,int n,double *A,double *wr,double *wi,
                           double *v,int *work1,double *work2) {
  int is1,is2,ierr;

  balanc(nm,n,A,is1,is2,work2);
  elmhes(nm,n,is1,is2,A,work1);
  eltran(nm,n,is1,is2,A,work1,v);
  ierr = hqr(nm,nm,is1,is2,A,wr,wi,v);
  if (ierr) return 0;
  balbak(nm,n,is1,is2,work2,n,v);

  return 1;
}

/**
 * Balancing operations.
 */
void swap(double*,int,double*,int,int);

void balanc(int nm,int n,double *A,int& low,int& high,double *scale) {

  static const double PREC = 1.e-16;
  static const double RADIX = 16.e0;

  double b2 = RADIX*RADIX;
  int i,j,ij,ji,k=0,l=n-1;
  bool last = false;
  
  // search for rows isolating an eigenvalue and push them down
 ROWS:
    // check rows
    for (i=l; i >= 0; i--) {
      for (j=0, ij=i*nm; j <= l; j++, ij++) {
        if (i == j) continue;
        if (A[ij] != 0.e0) break;
      }
      if (j > l) {
        scale[l] = i;
        if (i != l) { // exchange columns and rows
          swap(A+i,nm,A+l,nm,l+1);
          swap(A+i*nm+k,1,A+l*nm+k,1,n-k);
        }
        if (l == 0) goto END;
        l--;
        goto ROWS;
      }
    }

  // search for columns isolating an eigenvalue and push them left
 COLUMNS:
    // check column
    for (j=k; j <= l; j++) {
      for (i=k, ij=k*nm+j; i <= l; i++, ij+=nm) {
        if (i == j) continue;
        if (A[ij] != 0.e0) break;
      }
      if (i > l) { // exchange columns and rows
        scale[k] = j;
        if (j != k) {
          swap(A+j,nm,A+k,nm,l+1);
          swap(A+j*nm+k,1,A+k*nm+k,1,n-k);
        }
        k++;
        goto COLUMNS;
      }
    }

  // balance the submatrix in rows/columns k to l
  for (i=k; i <= l; i++) scale[i] = 1.e0;

  // iterative loop for norm reduction
  while (!last) {
    last = true;

    for (i=k; i <= l; i++) {

      // calculate row and column norm
      double c=0.e0,r=0.e0;
      for (j=k, ij=i*nm+k, ji=k*nm+i; j <=l ; j++, ij++, ji+=nm) {
        if (i == j) continue;
        c += std::fabs(A[ji]);
        r += std::fabs(A[ij]);
      }

      // if both are non-zero
      double g,f,s;
      if ((c > PREC) && (r > PREC)) {

        g = r/RADIX;
        f = 1.e0;
        s = c+r;
        while (c < g) {
          f *= RADIX;
          c *= b2;
        }

        g = r*RADIX;
        while (c > g) {
          f /= RADIX;
          c /= b2;
        }

        // balance
        if ((c+r)/f < 0.95*s) {
          g = 1.e0/f;
          scale[i] *= f;
          last = false;

          // apply similarity transformation
          for (j=k, ij=i*nm+k; j < n; j++, ij++) A[ij] *= g;
          for (j=0, ji=i  ; j <= l; j++, ji+=nm) A[ji] *= f;
        }
      }
    }
  }

 END:
   low = k;
   high = l;
}

void balbak(int nm,int n,int low,int high,double scale[],int m,double *v) {

  int i,j,k,ii,ij;

  // quick return
  if (m == 0) return;

  // step 1
  if (low != high) {
    for (i=low; i <= high; i++) {
      double s = scale[i];
      for (j=0,ij=i; j < m; j++, ij+=nm) v[ij] *= s;
    }
  }

  // step 2
  for (ii=0; ii < n; ii++) {
    if (ii >= low && ii <= high) continue;
    if (ii < low)
      i = low-ii;
    else
      i = ii;

    k = (int) scale[i];
    if (k == i) continue;
    swap(v+i,nm,v+k,nm,m);
  }
}

void swap(double *a,int inca,double *b,int incb,int n) {
  double tmp;
  for (int i=0; i < n; i++, a+=inca, b+=incb) {
    tmp = (*a);
    (*a) = (*b);
    (*b) = tmp;
  }
}

/**
 * Transformation of a general matrix to upper Hessenberg form.
 */
void elmhes(int nm,int n,int low,int high,double *A,int work[]) {

  static const double PREC = 1.e-16;

  int i,j,k,l,m,ij,ji,im1,jm1,jm,mj;

  k = low+1;
  l = high-1;
  if (l < k) return;

  for (m=k; m <= l; m++) {
    int m1 = m-1;
    double x = 0.e0;

    // find the pivot
    i = m;
    for (j=m, jm1=m*nm+m-1; j <= high; j++, jm1+=nm) {
      if (std::fabs(A[jm1]) < std::fabs(x)) continue;
      x = A[jm1];
      i = j;
    }
    work[m] = i;

    // interchange rows and columns
    if (i != m) {
      swap(A+i*nm+m1,1,A+m*nm+m1,1,n-m1);
      swap(A+i,nm,A+m,nm,high+1);
    }

    // carry out elimination
    if (std::fabs(x) > PREC) {
      for (i=m+1, im1=(m+1)*nm+m-1; i <= high; i++, im1+=nm) {
        double y = A[im1];
        if (y != 0.e0) {
          y /= x;
          A[im1] = y;
          for (j=m, ij=i*nm+m, mj=m*nm+m; j < n; j++, ij++, mj++) A[ij] -= y*A[mj];
          for (j=0, ji=i, jm=m; j <= high; j++, ji+=nm, jm+=nm)   A[jm] += y*A[ji];
        }
      }
    }
  }
}

void eltran(int nm,int n,int low,int high,double *A,int work[],double *v) {

  int i,j,m,ij,im,im1,mj;

  // initialize v to identity matrix
  for (j=0; j < n; j++) {
    for (i=0, ij=j*nm; i < n; i++, ij++) v[ij] = 0.e0;
    v[j+j*nm] = 1.e0;
  }

  // loop from high-1 to low+1
  for (m=high-1; m > low; m--) {
    int m1 = m+1;
    for (i=m1, im=m1*nm+m, im1=m1+m*nm; i <= high; i++, im+=nm, im1++) 
      v[im1] = A[im-1];

    i = work[m];
    if (i == m) continue;

    for (j=m, mj=m+m*nm, ij=i+m*nm; j <= high; j++, mj+=nm, ij+=nm) {
      v[mj] = v[ij];
      v[ij] = 0.e0;
    }

    v[i+m*nm] = 1.e0;
  }
}

/**
 * Find eigenvalues of a real upper Hessenberg matrix by the QR method.
 */
int hqr(int nm,int n,int low,int high,double *H,double wr[],double wi[],double *vv) {

  static const double PREC = 1.e-16;

  int i,j,k,l,m,ij,ij1,ik,ik1,ik2,in,in1,jn,jn1,kj,kj1,kj2,nj,nj1,mmin;
  double p=0.0e0,q=0.0e0,r=0.0e0,s=0.0e0,x,y,z=0.0e0,u,v,w;

  // store roots isolated by balanc, and compute matrix norm
  k=0;
  double norm = 0.e0;
  for (i=0; i < n; i++) {
    for (j=k, ij=i*nm+k; j < n; j++, ij++) norm += std::fabs(H[ij]);

    k = i;
    if (i >= low && i <= high) continue;
    wr[i] = H[i*nm+i];
    wi[i] = 0.e0;
  }

  // search for next eigenvalues
  int nn=high,nn1;
  double t=0.e0;
  while (nn >= low) {

    int iter=0;
    do {

      // look for single small subdiagonal element
      for (l=nn; l > low; l--) {
        s = std::fabs(H[(l-1)*(nm+1)])+std::fabs(H[l*(nm+1)]);
        if (s == 0.e0) s = norm;
        if ((std::fabs(H[l*nm+l-1])+s) == s) break;
      }

      // form shift
      x = H[nn*nm+nn];
      if (l == nn) { // ... one root found
        wr[nn] = x+t;
        wi[nn] = 0.e0;
        H[nn*nm+nn] = wr[nn];
        nn--;
      }
      else {
        nn1 = nn-1;
        y = H[nn1*nm+nn1];
        w = H[nn*nm+nn1]*H[nn1*nm+nn];
        if (l == nn1) { // ... two roots found
          p = 0.5*(y-x);
          q = p*p+w;
          z = std::sqrt(std::fabs(q));
          H[nn*nm+nn] = x+t;
          x += t;
          H[nn1*nm+nn1] = y+t;
          if (q >= 0.e0) { // ... a real pair
            z = p+((p > 0.e0)?(z):(-z));
            wr[nn1] = wr[nn] = x+z;
            if (std::fabs(z) > PREC) wr[nn] = x-w/z;
            wi[nn1] = wi[nn] = 0.e0;

            x = H[nn*nm+nn1];
            s = std::fabs(x)+std::fabs(z);
            p = x/s;
            q = z/s;
            r = std::sqrt(p*p+q*q);
            p /= r;
            q /= r;
            // row modification
            for (j=nn1, nj=nn*nm+nn1, nj1=nn1*nm+nn1; j < n; j++, nj++, nj1++) {
              z = H[nj1];
              H[nj1] = q*z+p*H[nj];
              H[nj]  = q*H[nj]-p*z;
            }
            // column modification
            for (i=0, in=nn, in1=nn1; i <= nn; i++, in+=nm, in1+=nm) {
              z = H[in1];
              H[in1] = q*z+p*H[in];
              H[in]  = q*H[in]-p*z;
            }
            // accumulate transformations
            for (i=low, in=low+nn*nm, in1=low+nn1*nm; i <= high; i++, in++, in1++) {
              z = vv[in1];
              vv[in1] = q*z+p*vv[in];
              vv[in]  = q*vv[in]-p*z;
            }
          }
          else {           // ... a complex pair
            wr[nn1] = wr[nn] = x+p;
            wi[nn1] = z;
            wi[nn] = -z;
          }
          nn -= 2;
        }
        else {          // ... no roots found. Continue iterations.
          if (iter == 30) return nn+1;
          if (iter == 10 || iter == 20) { // form exceptional shift
            t += x;
            for (i=low; i <=nn; i++) H[i*nm+i] -= x;
            s = std::fabs(H[nn*nm+nn1])+std::fabs(H[nn1*nm+nn1-1]);
            y = x = 0.75*s;
            w = -0.4375*s*s;
          }
          iter++;

          // look for two consecutive small sub-diagonal elements
          for (m=nn-2; m >= l; m--) {
            z = H[m*nm+m];
            r = x-z;
            s = y-z;
            p = (r*s-w)/H[(m+1)*nm+m]+H[m*nm+m+1];
            q = H[(m+1)*(nm+1)]-z-r-s;
            r = H[(m+2)*nm+m+1];
            s = std::fabs(p)+std::fabs(q)+std::fabs(r);
            p /= s;
            q /= s;
            r /= s;
            if (m == l) break;
            u = std::fabs(p)*(std::fabs(H[(m-1)*(nm+1)])
                              +std::fabs(z)+std::fabs(H[(m+1)*(nm+1)]));
            v = u+std::fabs(H[m*nm+m-1])*(std::fabs(q)+std::fabs(r));
            if (u == v) break;
          }
          for (i=m+2; i <= nn; i++) {
            H[i*nm+i-2] = 0.e0;
            if (i != (m+2)) H[i*nm+i-3] = 0.e0;
          }

          // double QR step on rows l to nn and columns m to nn
          for (k=m; k <= nn1; k++) {

            if (k != m) { // begin setup of Householder vector
              p = H[k*nm+k-1];
              q = H[(k+1)*nm+k-1];
              r = 0.e0;
              if (k != nn1) r = H[(k+2)*nm+k-1];
              x = std::fabs(p)+std::fabs(q)+std::fabs(r);
              if (x < PREC) continue;
              // scale to prevent underflow or overflow
              p /= x;
              q /= x;
              r /= x;
            }

            s = std::sqrt(p*p+q*q+r*r);
            if (p < 0.e0) s = -s;
            if (k == m) {
              if (l != m) H[k*nm+k-1] = -H[k*nm+k-1];
            }
            else 
              H[k*nm+k-1] = -s*x;
            p += s;
            x = p/s;
            y = q/s;
            z = r/s;
            q /= p;
            r /= p;

            // row modification
            kj = k*nm+k; kj1 = kj+nm; kj2 = kj1+nm;
            for (j=k; j <= nn; j++, kj++, kj1++,kj2++) {
              p = H[kj]+q*H[kj1];
              if (k != nn1) {
                p += r*H[kj2];
                H[kj2] -= p*z;
              }
              H[kj1] -= p*y;
              H[kj]  -= p*x;
            }

            // column modification
            mmin = (nn < k+3) ? nn : k+3;
            ik = l*nm+k; ik1 = ik+1; ik2 = ik1+1;
            for (i=l; i <= mmin; i++, ik+=nm, ik1+=nm, ik2+=nm) {
              p = x*H[ik]+y*H[ik1];
              if (k != nn1) {
                p += z*H[ik2];
                H[ik2] -= p*r;
              }
              H[ik1] -= p*q;
              H[ik]  -= p;
            }

            // accumulate transformations
            ik = low+k*nm; ik1 = ik+nm; ik2 = ik1+nm;
            for (i=low; i <= high; i++, ik++, ik1++, ik2++) {
              p = x*vv[ik]+y*vv[ik1];
              if (k != nn1) {
                p += z*vv[ik2];
                vv[ik2] -= p*r;
              }
              vv[ik1] -= p*q;
              vv[ik]  -= p;
            }
          }
        }
      }

    } while (l < nn-1);
  }

  // Backsubstitute to find vectors of upper triangular form
  for (nn=n-1; nn >= 0; nn--) {
    p = wr[nn];
    q = wi[nn];
    nn1 = nn-1;
    if (q == 0.e0) { // real vector
      m = nn;
      H[nn*nm+nn] = 1.e0;
      for (i=nn1; i >= 0; i--) {
        in  = i*nm+nn;
        in1 = i*nm+nn1;
        w = H[i*nm+i]-p;

        r = 0.e0;
        for (j=m, ij=i*nm+m, jn=m*nm+nn; j <= nn; j++, ij++, jn+=nm) 
          r += H[ij]*H[jn];

        if (wi[i] < 0.e0) {
          z = w;
          s = r;
          continue;
        }

        m = i;
        if (wi[i] == 0.e0) {
          t = w;
          if (t == 0.e0) {
            u = norm;
            t = u;
            do {
              t *= 0.01;
              v = norm+t;
            } while (v > u);
          }
          H[in] = -r/t;
        }
        else { // solve real equation
          x = H[i*nm+i+1];
          y = H[(i+1)*nm+i];
          q = (wr[i]-p)*(wr[i]-p)+wi[i]*wi[i];
          t = (x*s-z*r)/q;
          H[i*nm+nn] = t;
          if (std::fabs(x) <= std::fabs(z))
            H[in+nm] = (-s-y*t)/z;
          else
            H[in+nm] = (-r-w*t)/x;
        }

        // overflow control
        t = std::fabs(H[in]);
        if (t != 0.e0) {
          u = t;
          v = t+1.e0/t;
          if (u >= v)
            for (j=i, jn=in; j <= nn; j++, jn+=nm) H[jn] /= t;
        }
      }
    }
    else if (q < 0.e0) { // complex vector
      m = nn1;

      // last vector component chosen imaginary, so that eigenvector matrix is triangular
      if (std::fabs(H[nn*nm+nn1]) <= std::fabs(H[nn1*nm+nn]))
        cdiv(0.e0,-H[nn1*nm+nn],H[nn1*nm+nn1]-p,q,H[nn1*nm+nn1],H[nn1*nm+nn]);
      else {
        H[nn1*nm+nn1] = q/H[nn*nm+nn1];
        H[nn1*nm+nn] = -(H[nn*nm+nn]-p)/H[nn*nm+nn1];
      }

      for (i=nn-2; i >= 0; i--) {
        in  = i*nm+nn;
        in1 = i*nm+nn1;
        w = H[i*nm+i]-p;

        double ra = 0.e0;
        double sa = 0.e0;
        for (j=m, ij=i*nm+m, jn=m*nm+nn, jn1=m*nm+nn1; j <= nn; 
             j++, ij++, jn+=nm, jn1+=nm) {
          ra += H[ij]*H[jn1];
          sa += H[ij]*H[jn];
        }

        if (wi[i] < 0.e0) {
          z = w;
          r = ra;
          s = sa;
          continue;
        }

        m = i;
        if (wi[i] == 0.e0) {
          cdiv(-ra,-sa,w,q,H[in1],H[in]);
        }
        else { // solve complex equations
          x = H[i*nm+i+1];
          y = H[(i+1)*nm+i];
          double vr = (wr[i]-p)*(wr[i]-p)+wi[i]*wi[i]-q*q;
          double vi = 2*(wr[i]-p)*q;
          if (vr == 0.e0 && vi == 0.e0) {
            u = norm*(std::fabs(w)+std::fabs(q)+std::fabs(x)
                      +std::fabs(y)+std::fabs(z));
            vr = u;
            do {
              vr *= 0.01;
              v = u+vr;
            } while (v > u);
          }
          cdiv(x*r-z*ra+q*sa,x*s-z*sa-q*ra,vr,vi,H[in1],H[in]);
          if (std::fabs(x) <= (std::fabs(z)+std::fabs(q)))
            cdiv(-r-y*H[in1],-s-y*H[in],z,q,
                 H[in1+nm],H[in+nm]);
          else {
            H[in1+nm] = (-ra-w*H[in1]+q*H[in])/x;
            H[in+nm]  = (-sa-w*H[in]-q*H[in1])/x;
          }
        }

        // overflow control
        u = std::fabs(H[in1]); v = std::fabs(H[in]);
        t = (u > v) ? u:v;
        if (t != 0.e0) {
          u = t;
          v = t+1.e0/t;
          if (u >= v) {
            for (j=i, jn=in, jn1=in1; j < nn; j++, jn+=nm, jn1+=nm) {
              H[jn1] /= t;
              H[jn]  /= t;
            }
          }
        }
      }
    }
  }

  // vectors of isolated roots
  for (i=0; i < n; i++) {
    if (i >= low && i <= high) continue;
    for (j=0, ij=i, ij1=i*nm; j < n; j++, ij+=nm, ij1++) 
      vv[ij] = H[ij1];
  }

  // multiply by transformation matrix to give vectors of original full matrix
  for (j=n-1; j >= 0; j--) {
    m = (j < high) ? j:high;
    for (i=low, ij=low+j*nm; i <= high; i++, ij++) {
      z = 0.e0;
      for (k=low, ik=i+low*nm, kj=low*nm+j; k <= m; k++, ik+=nm, kj+=nm) 
        z += vv[ik]*H[kj];
      vv[ij] = z;
    }
  }

  return 0;
}

void cdiv(double ar,double ai,double br,double bi,double& cr,double& ci) {
  double s = std::fabs(br)+std::fabs(bi);
  double si = 1.e0/s;
  double ars,ais,brs,bis;
  ars = ar*si;
  ais = ai*si;
  brs = br*si;
  bis = bi*si;
  s = brs*brs+bis*bis;
  cr = (ars*brs+ais*bis)/s;
  ci = (ais*brs-ars*bis)/s;
}

