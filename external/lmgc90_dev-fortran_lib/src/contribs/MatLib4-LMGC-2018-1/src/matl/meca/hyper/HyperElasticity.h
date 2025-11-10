/*
 *  $Id: HyperElasticity.h 142 2014-02-07 12:51:54Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_HYPER_ELASTICITY_H
#define ZORGLIB_MATL_MECA_HYPER_ELASTICITY_H

// config
#include <matlib_macros.h>

// std C library
#include <cmath>
// local
#include <matl/ConstitutiveModel.h>
#include <matl/meca/eos/EOS.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

// forward declaration
template <class ALG> class ConvexHyperElasticity;

/**
 * Base class for hyperelasticity models.
 */
template <class ALG>
class HyperElasticity : virtual public StandardMaterial {

 public:

  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::Tensor::TYPE     TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  typedef typename ALG::Tensor4          TENSOR4;

  // nested classes
  class Potential;
  class Dilatancy;
  
  // friend class
  friend class ConvexHyperElasticity<ALG>;
  
 protected:
  
  // associated potential
  Potential *potential;
  
  // associated equation-of-state
  EOS *eos;

  // associated dilatancy
  Dilatancy *dilatancy;

  // instance counter
  unsigned int *count;
  
  // empty constructor
  HyperElasticity(Potential* p = 0,EOS* e = 0,Dilatancy* d = 0) {
    count = new unsigned int(1);
    potential = p;
    eos       = e;
    dilatancy = d;
  }
  
 public:
    
  // constructors
  HyperElasticity(Potential& p) {
    count = new unsigned int(1);
    potential = &p;
    eos = 0;
    dilatancy = 0;
  }
  HyperElasticity(Potential& p,EOS& e) {
    count = new unsigned int(1);
    potential = &p;
    eos = &e;
    dilatancy = 0;
  }
  HyperElasticity(Potential& p,Dilatancy& d) {
    count = new unsigned int(1);
    potential = &p;
    eos = 0;
    dilatancy = &d;
  }
  HyperElasticity(Potential& p,EOS& e,Dilatancy& d) {
    count = new unsigned int(1);
    potential = &p;
    eos = &e;
    dilatancy = &d;
  }
  
  // copy constructor
  HyperElasticity(const HyperElasticity& src) {
    count = src.count;
    (*count)++;
    potential = src.potential;
    eos = src.eos;
    dilatancy = src.dilatancy;
  }
  
  // destructor
  virtual ~HyperElasticity() {
    if (--(*count) > 0) return;
    delete count;
    if (potential) delete potential;
    if (eos) delete eos;
    if (dilatancy) delete dilatancy;
  }

  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\nHyperelastic material:" << std::endl;

    // density
    try {
      double rho = material.getDoubleProperty("MASS_DENSITY");
      if (os) (*os) << "\n\tmass density = " << rho << std::endl;
    }
    catch (NoSuchPropertyException) {
      if (os) (*os) << "\n\tmass density is not defined" << std::endl;
    }

    // eos
    if (eos) eos->checkProperties(material,os);

    // check potential
    if (potential) potential->checkProperties(material,os);

    // check dilatancy
    if (dilatancy) dilatancy->checkProperties(material,os);
  }
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties& material,const Rotation& R) {
    if (potential) potential->rotateProperties(material,R);
    if (dilatancy) dilatancy->rotateProperties(material,R);
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& mater,const ParameterSet& extPar) {
    if (eos) eos->updateProperties(mater,extPar);
    if (potential) potential->updateProperties(mater,extPar);
    if (dilatancy) dilatancy->updateProperties(mater,extPar);
  }
  
  // how many external variables ?
  unsigned int nExtVar() const {return TENSOR::MEMSIZE;}
  
  // self-documenting utilities
  unsigned int nExtVarBundled() const {return 1;}
  ConstitutiveModel::VariableType typeExtVar(unsigned int i) const {
    switch (i) {
      case 0:
        return ConstitutiveModel::TYPE_TENSOR;
        break;
      default:
        return ConstitutiveModel::TYPE_NONE;
        break;
    }
  }
  unsigned int indexExtVar(unsigned int i) const {
    switch (i) {
      case 0:
        return 0;
        break;
      default:
        return TENSOR::MEMSIZE;
        break;
    }
  }
  std::string labelExtVar(unsigned int i) const {
    switch (i) {
      case 0:
        return "deformation";
        break;
      default:
        return "";
        break;
    }
  }
  std::string labelExtForce(unsigned int i) const {
    switch (i) {
      case 0:
        return "stress";
        break;
      default:
        return "";
        break;
    }
  }

  // how many internal variables ?
  unsigned int nIntVar() const {return 1;}
  
  // self-documenting utilities
  unsigned int nIntVarBundled() const {return 1;}
  unsigned int getIntVar(const std::string& str) const {
    if (str == "ENRG")
      return 0;
    else
      return 1;
  }
  ConstitutiveModel::VariableType typeIntVar(unsigned int i) const {
    switch (i) {
      case 0:
        return ConstitutiveModel::TYPE_SCALAR;
        break;
      default:
        return ConstitutiveModel::TYPE_NONE;
        break;
    }
  }
  unsigned int indexIntVar(unsigned int i) const {
    switch (i) {
      case 0:
        return 0;
        break;
      default:
        return 1;
        break;
    }
  }
  std::string labelIntVar(unsigned int i) const {
    switch (i) {
      case 0:
        return "elastically stored energy";
        break;
      default:
        return "";
        break;
    }
  }
  
  // initialize the state of the material
  void initState(const MaterialProperties& material,MaterialState& state) {
    ConstitutiveModel::initState(material,state);
    state.grad = TENSOR::identity();
    state.flux = 0.e0;
    state.internal = 0.e0;
  }
  
  // compute the incremental potential
  double incrementalPotential(const MaterialProperties& material,
                              const ParameterSet& extPar,
                              const MaterialState& state0,MaterialState& state,
                              double dTime,MatLibMatrix& T,
                              bool update,bool tangent) 
   throw (UpdateFailedException) {
    
    // get tensors
    TENSOR F(state.grad);
    TENSOR P(state.flux);
    TENSOR4 K(T);

    // right Cauchy-Green tensor
    SYM_TENSOR C;
    ALG::RightCauchyGreen(F,C);
    
    // compute stored energy
    SYM_TENSOR S;
    SYM_TENSOR4 M;
    double W = storedEnergy(material,extPar,C,S,M,update || tangent,tangent);
    state.internal[0] = W;
    
    // compute Piola tensor and Lagrangian tangents
    if (update) ALG::PK2ToPK1(S,F,P);
    if (tangent) ALG::MaterialToLagrangian(M,S,F,K);

    return W-state0.internal[0];
  }
  
  /*/ compute dual potential (polyconvex conjugate)
  double dualPotential(const MaterialProperties& material,
                       const ParameterSet& extPar,
                       const MaterialState& state0,MaterialState& state,
                       double dTime,MatLibMatrix& T,
                       bool update,bool tangent) 
   throw (UpdateFailedException) {

    // update ?
    if (update) state.internal = state0.internal;

    // get tensors
    TENSOR F(state.grad),F0(state0.grad);
    TENSOR P(state.flux);
    TENSOR coF(state.grad,TENSOR::MEMSIZE);
    TENSOR coP(state.flux,TENSOR::MEMSIZE);
    double J  = state.grad[2*TENSOR::MEMSIZE];
    double Pi = state.flux[2*TENSOR::MEMSIZE];

    // initialize
    MaterialState s0,s1;
    initState(material,s0);
    s0.grad = F0;
    s0.internal = state0.internal;
    initState(material,s1);
    s1.grad = F;
    s1.internal = state.internal;
     
    // find F = arg inf [F.P+coF.coP+J.Pi - Winc(F)]
    MatLibMatrix M(TENSOR::MEMSIZE);
    double W = incrementalPotential(material,extPar,s0,s1,dTime,M,update,update || tangent);
    if (update) {
      TENSOR FF(s1.grad),PP(s1.flux);
      static const unsigned int ITMAX = 20;
      static const double PRECISION = 1.e-12;
      static const double TOLERANCE = 1.e-08;
      unsigned int iter = 0;
      double norm0;
      while (iter < ITMAX) {

        // evaluate residual
        TENSOR4 tmp;
        tmp = outerProd(coF,coF);
        tmp.addIJKL(-1.0,coF);
        double Jinv = 1.0/J;
        tmp *= Jinv;
        TENSOR R = P+tmp*coP+Pi*coF-PP;
        double norm1 = normL2(R);
        std::cout << "iter=" << iter << "-R=" << R << "-norm=" << norm1 << "(norm0=" << norm0 << ")" << std::endl;
        if (norm1 < PRECISION) break;
        if (iter > 0) {
          if (norm1 < TOLERANCE*norm0) break;
        }
        else
          norm0 = norm1;
        
        // assemble Hessian matrix
        TENSOR PF = Jinv*(coP*coF.transposed());
        double val = trace(PF)+Pi;
        M -= val*tmp;
        unsigned int i,j,k,l,ij,il,kj,kl,m,mi,mj,mk,ml;
        for (i=0,ij=0; i < ALG::DIMENSION; i++)
          for (j=0; j < ALG::DIMENSION; j++,ij++)
            for (k=0,kj=j,kl=0; k < ALG::DIMENSION; k++,kj+=ALG::DIMENSION)
              for (l=0,il=i*ALG::DIMENSION; l < ALG::DIMENSION; l++,il++,kl++)
                for (m=0,mi=i,mj=j,mk=k,ml=l; m < ALG::DIMENSION; 
                     m++,mi+=ALG::DIMENSION,mj+=ALG::DIMENSION,mk+=ALG::DIMENSION,ml+=ALG::DIMENSION)
                  M[ij][kl] -= tmp[il][mj]*PF[mk]+tmp[kj][ml]*PF[mi];
        for (m=ALG::DIMENSION,ij=ALG::DIMENSION*ALG::DIMENSION; m < 3; m++,ij++)
          M[ij][ij] -= 2*tmp[ij][ij]*PF[ij];

        // solve
        TENSOR dF;
        M.solve(dF,R,true);
        FF += dF;

        // recompute cofactor and determinant
        F = FF;
        ALG::Tensor::cofactor(FF,coF);
        J = determinant(FF);
        
        // compute incremental energy
        W = incrementalPotential(material,extPar,s0,s1,dTime,M,true,true);
        iter++;
      }
      
      // finish update
      state.grad[2*TENSOR::MEMSIZE] = J;
    }
     
    // tangents
    if (tangent) {
      
      // derivative of cof[F]
      TENSOR4 tmp;
      tmp = outerProd(coF,coF);
      tmp.addIJKL(-1.0,coF);
      double Jinv = 1.0/J;
      tmp *= Jinv;
      
      // invert material tangent
      M.invert();
      
      // build by blocks
      ShortMatrix T11(T,TENSOR::MEMSIZE,TENSOR::MEMSIZE);
      T11 = M;
      ShortMatrix T12(T,TENSOR::MEMSIZE,TENSOR::MEMSIZE,0,TENSOR::MEMSIZE);
      T12 = M*tmp;
      ShortMatrix T21(T,TENSOR::MEMSIZE,TENSOR::MEMSIZE,TENSOR::MEMSIZE,0);
      T21 = tmp*M;
      ShortMatrix T22(T,TENSOR::MEMSIZE,TENSOR::MEMSIZE,TENSOR::MEMSIZE,TENSOR::MEMSIZE);
      T22 = tmp*M*tmp;
      ShortMatrix T13(T,TENSOR::MEMSIZE,1,0,2*TENSOR::MEMSIZE);
      ShortMatrix T31(T,1,TENSOR::MEMSIZE,2*TENSOR::MEMSIZE,0);
      TENSOR T13_asArray = M*coF;
      for (unsigned int k=0; k < TENSOR::MEMSIZE; k++) {
        T13[k][0] = T13_asArray[k];
        T31[0][k] = T13_asArray[k];
      }
      ShortMatrix T23(T,TENSOR::MEMSIZE,1,TENSOR::MEMSIZE,2*TENSOR::MEMSIZE);
      ShortMatrix T32(T,1,TENSOR::MEMSIZE,2*TENSOR::MEMSIZE,TENSOR::MEMSIZE);
      TENSOR T23_asArray = tmp*M*coF;
      for (unsigned int k=0; k < TENSOR::MEMSIZE; k++) {
        T23[k][0] = T23_asArray[k];
        T32[0][k] = T23_asArray[k];
      }
      T[2*TENSOR::MEMSIZE][2*TENSOR::MEMSIZE] = innerProd(coF,M*coF);
    }

    return innerProd(F,P)+innerProd(coF,coP)+J*Pi-W;
  }*/

 protected:

  // compute polyconvex conjugate energy
  double dualEnergy(const MaterialProperties& material,
                    const ParameterSet& extPar,
                    const MatLibArray& TS,MatLibArray& TU,MatLibMatrix& M,
                    bool first,bool second) {
    
    // get tensors
    const SYM_TENSOR S(TS),coS(TS,SYM_TENSOR::MEMSIZE);
    SYM_TENSOR U(TU),coU(TU,SYM_TENSOR::MEMSIZE);
    double Pi = TS[2*SYM_TENSOR::MEMSIZE];
    double J  = TU[2*SYM_TENSOR::MEMSIZE];
    //std::cout << TS << std::endl;

    // find U = arg inf [U.S+coU.coS+J.Pi - W(U*U)]
    SYM_TENSOR Uc = contravariant(U);
    SYM_TENSOR SS;
    SYM_TENSOR4 MM;
    double W = this->storedEnergy(material,extPar,symProd(Uc,Uc),SS,MM,first,first || second);
    /*{
      SYM_TENSOR SStmp,SSnum,RP,RM;
      SYM_TENSOR4 MMnum,tmp;
      for (unsigned int ij=0; ij < SYM_TENSOR::MEMSIZE; ij++) {
        U[ij] += 1.e-6;
        Uc = contravariant(U);
        double WP = this->storedEnergy(material,extPar,symProd(Uc,Uc),SStmp,MMnum,true,false);
        ALG::SymTensor::cofactor(Uc,coU);
        coU = covariant(coU);
        J = determinant(Uc);
        WP -= innerProd(U,S)+innerProd(coU,coS)+J*Pi;
        tmp = outerProd(contravariant(coU),contravariant(coU));
        tmp.addIJKL(-0.5,contravariant(coU));
        tmp /= J;
        RP = S+innerProd2(tmp,coS)+Pi*contravariant(coU)-symProd(SStmp,Uc);
        U[ij] -= 2.e-6;
        Uc = contravariant(U);
        double WM = this->storedEnergy(material,extPar,symProd(Uc,Uc),SStmp,MMnum,true,false);
        ALG::SymTensor::cofactor(Uc,coU);
        coU = covariant(coU);
        J = determinant(Uc);
        WM -= innerProd(U,S)+innerProd(coU,coS)+J*Pi;
        tmp = outerProd(contravariant(coU),contravariant(coU));
        tmp.addIJKL(-0.5,contravariant(coU));
        tmp /= J;
        RM = S+innerProd2(tmp,coS)+Pi*contravariant(coU)-symProd(SStmp,Uc);
        U[ij] += 1.e-6;
        Uc = contravariant(U);
        ALG::SymTensor::cofactor(Uc,coU);
        coU = covariant(coU);
        J = determinant(Uc);
        SSnum[ij] = (WP-WM)*0.5e6;
        for (unsigned int kl=0; kl < SYM_TENSOR::MEMSIZE; kl++)
          MMnum[kl][ij] = (RP[kl]-RM[kl])*0.5e6;
      }
      std::cout << SSnum << std::endl;
      std::cout << MMnum << std::endl;
    }*/
    if (first) {
      static const unsigned int ITMAX = 20;
      static const double PRECISION = 1.e-12;
      static const double TOLERANCE = 1.e-08;
      unsigned int iter = 0;
      double norm0;
      while (iter < ITMAX) {
        
        // evaluate residual
        SYM_TENSOR4 tmp1,tmp2,tmp3;
        tmp1 = outerProd(contravariant(coU),contravariant(coU));
        tmp2 = 0.0;
        tmp2.addIJKL(0.5,contravariant(coU));
        tmp1 -= tmp2;
        double Jinv = 1.0/J;
        tmp1 *= Jinv;
        SYM_TENSOR R = S+innerProd2(tmp1,coS)+Pi*contravariant(coU)-symProd(SS,Uc);
        double norm1 = normL2(R);
        std::cout << "***iter=" << iter << "-R=" << R << "-norm=" << norm1 << "(norm0=" << norm0 << ")" << std::endl;
        if (norm1 < PRECISION) break;
        if (iter > 0) {
          if (norm1 < TOLERANCE*norm0) break;
        }
        else
          norm0 = norm1;
        
        // assemble Hessian matrix
        SYM_TENSOR tmp;
        tmp = Jinv*innerProd2(tmp2,coS);
        double val = Jinv*innerProd2(contravariant(coU),coS)+Pi;
        tmp3 = symProd(Uc,symProd(MM,Uc));
        tmp3.addIJKL(0.25,SS,SYM_TENSOR::identity());
        tmp3.addIJKL(0.25,SYM_TENSOR::identity(),SS);
        MM = tmp3+Jinv*(outerProd(contravariant(coU),tmp)+outerProd(tmp,contravariant(coU)))-val*tmp1;
        MM.addIJKL(-0.5*Jinv,contravariant(coU),tmp);
        MM.addIJKL(-0.5*Jinv,tmp,contravariant(coU));
        //std::cout << MM << std::endl;
        
        // solve
        SYM_TENSOR dU;
        MM.solve(dU,R,true);
        U += dU;
        
        // recompute cofactor and determinant
        Uc = contravariant(U);
        ALG::SymTensor::cofactor(Uc,coU);
        coU = covariant(coU);
        J = determinant(Uc);
        
        // compute incremental energy
        W = storedEnergy(material,extPar,symProd(Uc,Uc),SS,MM,true,true);
        iter++;
      }
      
      // finish update
      TU[2*SYM_TENSOR::MEMSIZE] = J;
    }
    
    // tangents
    if (second) {
      
      // derivative of cof[U]
      SYM_TENSOR4 tmp1,tmp2,tmp3;
      tmp1 = outerProd(contravariant(coU),contravariant(coU));
      tmp2 = 0.0;
      tmp2.addIJKL(0.5,contravariant(coU));
      tmp1 -= tmp2;
      double Jinv = 1.0/J;
      tmp1 *= Jinv;
      
      // invert material tangent
      SYM_TENSOR tmp;
      tmp = Jinv*innerProd2(tmp2,coS);
      double val = Jinv*innerProd2(contravariant(coU),coS)+Pi;
      tmp3 = symProd(Uc,symProd(MM,Uc));
      tmp3.addIJKL(0.25,SS,SYM_TENSOR::identity());
      tmp3.addIJKL(0.25,SYM_TENSOR::identity(),SS);
      MM = tmp3+Jinv*(outerProd(contravariant(coU),tmp)+outerProd(tmp,contravariant(coU)))-val*tmp1;
      MM.addIJKL(-0.5*Jinv,contravariant(coU),tmp);
      MM.addIJKL(-0.5*Jinv,tmp,contravariant(coU));
      MM.invert();
      
      // build by blocks
      SYM_TENSOR4 II = SYM_TENSOR4::covariantIdentity();
      ShortMatrix T11(M,SYM_TENSOR::MEMSIZE,SYM_TENSOR::MEMSIZE);
      T11 = MM;
      ShortMatrix T12(M,SYM_TENSOR::MEMSIZE,SYM_TENSOR::MEMSIZE,0,SYM_TENSOR::MEMSIZE);
      //T12 = innerProd2(MM,tmp1);
      T12 = MM*tmp1*II;
      ShortMatrix T21(M,SYM_TENSOR::MEMSIZE,SYM_TENSOR::MEMSIZE,SYM_TENSOR::MEMSIZE,0);
      //T21 = innerProd2(tmp1,MM);
      T21 = II*tmp1*MM;
      ShortMatrix T22(M,SYM_TENSOR::MEMSIZE,SYM_TENSOR::MEMSIZE,SYM_TENSOR::MEMSIZE,SYM_TENSOR::MEMSIZE);
      //T22 = innerProd2(tmp1,innerProd2(MM,tmp1));
      T22 = II*tmp1*T12;
      ShortMatrix T13(M,SYM_TENSOR::MEMSIZE,1,0,2*SYM_TENSOR::MEMSIZE);
      ShortMatrix T31(M,1,SYM_TENSOR::MEMSIZE,2*SYM_TENSOR::MEMSIZE,0);
      //SYM_TENSOR T13_asArray = innerProd2(MM,contravariant(coU));
      SYM_TENSOR T13_asArray;
      T13_asArray = MM*contravariant(coU);
      for (unsigned int k=0; k < SYM_TENSOR::MEMSIZE; k++) {
        T13[k][0] = T13_asArray[k];
        T31[0][k] = T13_asArray[k];
      }
      ShortMatrix T23(M,SYM_TENSOR::MEMSIZE,1,SYM_TENSOR::MEMSIZE,2*SYM_TENSOR::MEMSIZE);
      ShortMatrix T32(M,1,SYM_TENSOR::MEMSIZE,2*SYM_TENSOR::MEMSIZE,SYM_TENSOR::MEMSIZE);
      //SYM_TENSOR T23_asArray = innerProd2(tmp1,innerProd2(MM,contravariant(coU)));
      SYM_TENSOR T23_asArray;
      T23_asArray = II*tmp1*T13_asArray;
      for (unsigned int k=0; k < SYM_TENSOR::MEMSIZE; k++) {
        T23[k][0] = T23_asArray[k];
        T32[0][k] = T23_asArray[k];
      }
      //M[2*SYM_TENSOR::MEMSIZE][2*SYM_TENSOR::MEMSIZE] = innerProd2(contravariant(coU),innerProd2(MM,contravariant(coU)));
      M[2*SYM_TENSOR::MEMSIZE][2*SYM_TENSOR::MEMSIZE] = innerProd(contravariant(coU),MM*contravariant(coU));
    }

    return innerProd(U,S)+innerProd(coU,coS)+J*Pi-W;
  }

  // compute stored energy, accounting for EOS
  double storedEnergy(const MaterialProperties& material,
                      const ParameterSet& extPar,
                      const SYM_TENSOR& C,SYM_TENSOR& S,SYM_TENSOR4& M,
                      bool first,bool second) {
    double W = 0.e0;
    
    static const double ONE_THIRD = 1./3.;
    static const double TWO_THIRD = 2./3.;
    
    // if there is an e.o.s.
    if (potential && eos) {
      
      // compute distortion strain
      double J;
      SYM_TENSOR Cinv,Cbar;
      if (first || second)
        Cinv = C.inverse(J);
      else
        J = determinant(C);
      if (J < 1.0e-16)
	      throw UpdateFailedException("zero jacobian (det[C])");
      J = std::sqrt(J);
      double coef = std::pow(J,-TWO_THIRD);
      Cbar = coef*C;
      
      // deviatoric part
      SYM_TENSOR Sbar;
      SYM_TENSOR4 Mbar;
      W = potential->storedEnergy(material,extPar,Cbar,Sbar,Mbar,
                                  first,second);
      
      // volumic part
      double p,K;
      W += eos->storedEnergy(material,extPar,J,p,K,first || second,second);

      // add contributions
      double press=0.e0,trS=0.e0;
      if (first || second) {
        
        // Lagrangian pressure
        press = J*p;
        
        // compute stress (Lagrangian) trace
        trS = ONE_THIRD*innerProd2(Sbar,C);
        
        // compute stresses (PK2)
        S = coef*Sbar+(press-coef*trS)*Cinv;
      }

      if (second) {
        double coef1 = TWO_THIRD*coef;
        double coef2 = coef*coef;
        
        SYM_TENSOR tmp = ONE_THIRD*innerProd2(Mbar,C);
        double CMC = ONE_THIRD*innerProd2(C,tmp);
        
        double val1 = coef*trS-press;
        double val2 = press+K*J*J;
        M = coef2*(Mbar-outerProd(Cinv,tmp)-outerProd(tmp,Cinv))
           -coef1*(outerProd(Cinv,Sbar)+outerProd(Sbar,Cinv))
           +(coef2*CMC+coef1*trS+val2)*outerProd(Cinv,Cinv);
        M.addIJKL(val1,Cinv);
      }
    }
    // no e.o.s.
    else if (potential) {
      W = potential->storedEnergy(material,extPar,C,S,M,first,second);
    }
    // e.o.s. only (does not really make sense at this level)
    else if (eos) {
      // TO DO
    }
    else {
      if (first) S = 0.0e0;
      if (second) M = 0.0e0;
    }
    
    // dilatancy term (part of this may also be included in the e.o.s.)
    if (dilatancy) {
      SYM_TENSOR ST;
      SYM_TENSOR4 MT;
      W += dilatancy->couplingEnergy(material,extPar,C,ST,MT,first,second);
      if (first) S += ST;
      if (second) M += MT;
    }
    
    return W;
  }
};  


/**
 * Base class for hyperelastic potentials.
 */
template <class ALG>
class HyperElasticity<ALG>::Potential {

 protected:
  
  // constructor
  Potential() {}

 public:

  // destructor
  virtual ~Potential() {}

  // check consistency of material properties
  virtual void checkProperties(MaterialProperties&,std::ostream* = 0) 
    throw (InvalidPropertyException, NoSuchPropertyException) = 0;
  
  // apply rotation to material properties
  virtual void rotateProperties(MaterialProperties&,const Rotation&) {}
  
  // update properties in function of external parameters
  virtual void updateProperties(MaterialProperties&,const ParameterSet&) {}
  
  // compute stored energy
  virtual double storedEnergy(const MaterialProperties&,const ParameterSet&,
                              const SYM_TENSOR&,SYM_TENSOR&,
                              SYM_TENSOR4&,bool,bool) = 0;
};


/**
 * Base class for (isotropic) hyperelastic potentials,
 * which are function of principal stretches.
 */
template <class ALG>
class SpectralHEPotential : virtual public HyperElasticity<ALG>::Potential {

 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
 protected:
  
  // constructor
  SpectralHEPotential() {}
  
 public:
  
  // destructor
  virtual ~SpectralHEPotential() {}
  
  // compute stored energy
  double storedEnergy(const MaterialProperties& material,
                      const ParameterSet& extPar,
                      const SYM_TENSOR& C,SYM_TENSOR& S,
                      SYM_TENSOR4& M,bool first,bool second) {
    
    // compute principal stretches
    double lambda[3],t[3],h[3][3];
    SYM_TENSOR N[3];
    C.eigenSplit(lambda,N);
    
    // compute stored energy
    double W = storedEnergy(material,extPar,lambda,t,h,
                            first || second,second);
    
    // stresses
    if (first) {
      S = 0.0e0;
      for (unsigned int k=0; k < 3; k++) S += (2*t[k])*N[k];
    }
    
    // tangents
    if (second) {
      M = 0.0e0;
      for (unsigned int k=0; k < 3; k++)
        for (unsigned int l=0; l < 3; l++) {
          M += (4*h[k][l])*outerProd(N[k],N[l]);
          if (k == l) continue;
          double dl = lambda[l]-lambda[k];
          double coef;
          if (std::fabs(dl) > 1.e-16)
            coef = 2*(t[l]-t[k])/dl;
          else
            coef = 2*(h[l][l]-h[k][l]);
          M.addIJKL(coef,N[k],N[l]);
        }
    }

    return W;
  }
  
  // compute stored energy from principal stretches
  virtual double storedEnergy(const MaterialProperties&,const ParameterSet&,
                              const double[],double[],double[][3],bool,bool) = 0;
};


/**
 * Base class for dilatancy potentials.
 */
template <class ALG>
class HyperElasticity<ALG>::Dilatancy {
  
 protected:
  
  // constructor
  Dilatancy() {}
  
 public:
  
  // destructor
  virtual ~Dilatancy() {}
  
  // check consistency of material properties
  virtual void checkProperties(MaterialProperties&,std::ostream* = 0)
    throw (InvalidPropertyException, NoSuchPropertyException)= 0;
  
  // apply rotation to material properties
  virtual void rotateProperties(MaterialProperties&,const Rotation&) {}
  
  // update properties in function of external parameters
  virtual void updateProperties(MaterialProperties&,const ParameterSet&) {}
  
  // definition of the coupling energy
  virtual double couplingEnergy(const MaterialProperties&,const ParameterSet&,
                                const SYM_TENSOR&,SYM_TENSOR&,
                                SYM_TENSOR4&,bool,bool) = 0;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
