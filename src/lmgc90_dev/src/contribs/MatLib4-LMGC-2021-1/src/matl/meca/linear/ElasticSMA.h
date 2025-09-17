/*
 *  $Id: ElasticSMA.h 139 2013-08-30 15:33:21Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_ELASTIC_SMA_H
#define ZORGLIB_MATL_MECA_LINEAR_ELASTIC_SMA_H

// config
#include <matlib_macros.h>

// std C library
#include <cstdio>
// local
#include <matl/meca/linear/Elasticity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif


/**
 * Base class for latent heat potentials.
 */
class LatentHeatPotential {
  
 protected:
  
  // constructor
  LatentHeatPotential() {}
  
 public:
  
  // destructor
  virtual ~LatentHeatPotential() {}
  
  // check consistency of material properties
  virtual void checkProperties(MaterialProperties&,std::ostream* = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) = 0;
  
  // update properties in function of external parameters
  virtual void updateProperties(MaterialProperties&,const ParameterSet&) {}
    
  // compute latent heat energy
  virtual double latentHeat(const MaterialProperties&,const ParameterSet&,
                            double,double&,double&,bool,bool) = 0;
};


/**
 * Base class for (geometrically linear) thermo-elastic shape memory alloys.
 */
template <class ALG>
class ElasticSMA : virtual public Elasticity<ALG> {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;

  // define default number of variants
  static const unsigned int DEFAULT_N_VARIANTS = 1;

 protected:

  // number of variants
  unsigned int nVariants;

  // associated latent heat
  LatentHeatPotential *latent;
  
  // empty constructor
  ElasticSMA(LatentHeatPotential* l = 0) {
    nVariants = DEFAULT_N_VARIANTS;
    latent = l;
  }

 public:
    
  // constructors
  ElasticSMA(typename Elasticity<ALG>::Potential& p,LatentHeatPotential& l) 
  : Elasticity<ALG>(p) {nVariants = DEFAULT_N_VARIANTS; latent = &l;}
  ElasticSMA(typename Elasticity<ALG>::Potential& p,LatentHeatPotential& l,
             typename Elasticity<ALG>::Dilatancy& d) 
  : Elasticity<ALG>(p,d) {nVariants = DEFAULT_N_VARIANTS; latent = &l;}
  
  // copy constructor
  ElasticSMA(const ElasticSMA& src) 
  : Elasticity<ALG>(src) {nVariants = src.nVariants; latent = src.latent;}
  
  // destructor
  virtual ~ElasticSMA() {
    if (*(this->count) > 1) return;
    if (latent) delete latent;
  }
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\nLinear elastic shape-memory alloy:" << std::endl;

    // density
    try {
      double rho = material.getDoubleProperty("MASS_DENSITY");
      if (os) (*os) << "\n\tmass density = " << rho << std::endl;
    }
    catch (NoSuchPropertyException) {
      if (os) (*os) << "\n\tmass density is not defined" << std::endl;
    }

    // check potential
    this->potential->checkProperties(material,os);

    // check dilatancy
    if (this->dilatancy) this->dilatancy->checkProperties(material,os);
    
    // check variants
    nVariants = checkVariants(material,os);

    // check latent heat
    if (latent) latent->checkProperties(material,os);
  }

  // check variants
  static unsigned int checkVariants(MaterialProperties& material,std::ostream* os = 0)
   throw (InvalidPropertyException, NoSuchPropertyException) {
    unsigned int nVar;
    try {
      nVar = material.getIntegerProperty("NUMBER_OF_VARIANTS");
    }
    catch (NoSuchPropertyException) {
      nVar = DEFAULT_N_VARIANTS;
      material.setProperty("NUMBER_OF_VARIANTS",static_cast<int>(DEFAULT_N_VARIANTS));
    }
    if (nVar > 1) { // read variants
      char str[64];
      StdProperty< SYM_TENSOR > BProp;
      SYM_TENSOR& B = BProp.value();
      // loop on variants
      for (unsigned int n=0; n < nVar; n++) {
        std::sprintf(str,"TRANSFORMATION_STRAIN_11_%u",n+1);
        B[SYM_TENSOR::MAP[0][0]] = material.getDoubleProperty(str);
        if (ALG::DIMENSION >= 2) {
          std::sprintf(str,"TRANSFORMATION_STRAIN_12_%u",n+1);
          B[SYM_TENSOR::MAP[0][1]] = material.getDoubleProperty(str);
        }
        std::sprintf(str,"TRANSFORMATION_STRAIN_22_%u",n+1);
        B[SYM_TENSOR::MAP[1][1]] = material.getDoubleProperty(str);
        if (ALG::DIMENSION == 3) {
          std::sprintf(str,"TRANSFORMATION_STRAIN_13_%u",n+1);
          B[SYM_TENSOR::MAP[0][2]] = material.getDoubleProperty(str);
          std::sprintf(str,"TRANSFORMATION_STRAIN_23_%u",n+1);
          B[SYM_TENSOR::MAP[1][2]] = material.getDoubleProperty(str);
        }
        std::sprintf(str,"TRANSFORMATION_STRAIN_33_%u",n+1);
        B[SYM_TENSOR::MAP[2][2]] = material.getDoubleProperty(str);
        std::sprintf(str,"TRANSFORMATION_STRAIN_%u",n+1);
        material.setProperty(str,BProp);
      }
    }
    else { // read variant
      StdProperty< SYM_TENSOR > BProp;
      SYM_TENSOR& B = BProp.value();
      B[SYM_TENSOR::MAP[0][0]] = material.getDoubleProperty("TRANSFORMATION_STRAIN_11");
      if (ALG::DIMENSION >= 2) {
        B[SYM_TENSOR::MAP[0][1]] = material.getDoubleProperty("TRANSFORMATION_STRAIN_12");
      }
      B[SYM_TENSOR::MAP[1][1]] = material.getDoubleProperty("TRANSFORMATION_STRAIN_22");
      if (ALG::DIMENSION == 3) {
        B[SYM_TENSOR::MAP[0][2]] = material.getDoubleProperty("TRANSFORMATION_STRAIN_13");
        B[SYM_TENSOR::MAP[1][2]] = material.getDoubleProperty("TRANSFORMATION_STRAIN_23");
      }
      B[SYM_TENSOR::MAP[2][2]] = material.getDoubleProperty("TRANSFORMATION_STRAIN_33");
      material.setProperty("TRANSFORMATION_STRAIN",BProp);
    }

    return nVar;
  }

  // apply rotation to material properties
  void rotateProperties(MaterialProperties& material,const Rotation& R) {
    this->potential->rotateProperties(material,R);
    if (this->dilatancy) this->dilatancy->rotateProperties(material,R);
    rotateVariants(material,R);
  }

  // rotate transformation strain(s)
  static void rotateVariants(MaterialProperties& material,const Rotation& R) {
    Tensor3D R0;
    R.toTensor(R0);
    unsigned int nVar = material.getIntegerProperty("NUMBER_OF_VARIANTS");
    if (nVar > 1) { // read variants
      char str[64];
      // loop on variants
      for (unsigned int n=0; n < nVar; n++) {
        // get transformation strain
        std::sprintf(str,"TRANSFORMATION_STRAIN_%u",n+1);
        StdProperty< SYM_TENSOR >& BProp
          = dynamic_cast<StdProperty< SYM_TENSOR >&>(material.getProperty(str));
        SYM_TENSOR& B = BProp.value();
        
        // rotate transformation strain
        B = covariant(B.contravariant().contravariantPush(R0));
      }
    }
    else { // read variant
      // get transformation strain
      StdProperty< SYM_TENSOR >& BProp
        = dynamic_cast<StdProperty< SYM_TENSOR >&>(material.getProperty("TRANSFORMATION_STRAIN"));
      SYM_TENSOR& B = BProp.value();
    
      // rotate transformation strain
      B = covariant(B.contravariant().contravariantPush(R0));
    }
  }

  // update properties in function of external parameters
  void updateProperties(MaterialProperties& mater,const ParameterSet& extPar) {
    this->potential->updateProperties(mater,extPar);
    if (this->dilatancy) this->dilatancy->updateProperties(mater,extPar);
    latent->updateProperties(mater,extPar);
  }
  
  // how many internal variables ?
  unsigned int nIntVar() const {return nVariants+2;}
  
  // self-documenting utilities
  unsigned int nIntVarBundled() const {return nVariants+2;}
  unsigned int getIntVar(const std::string& str) const {
    if (str == "VFRC" || str == "VFRC1")
      return 0;
    else if (nVariants > 1 && str == "VFRC2")
      return 1;
    else if (nVariants > 2 && str == "VFRC3")
      return 2;
    else if (nVariants > 3 && str == "VFRC4")
      return 3;
    else if (nVariants > 4 && str == "VFRC5")
      return 4;
    else if (nVariants > 5 && str == "VFRC6")
      return 5;
    else if (nVariants > 6 && str == "VFRC7")
      return 6;
    else if (nVariants > 7 && str == "VFRC8")
      return 7;
    else if (nVariants > 8 && str == "VFRC9")
      return 8;
    else if (nVariants > 9 && str == "VFRC10")
      return 9;
    else if (str == "ENRG")
      return nVariants;
    else if (str == "TNRG")
      return nVariants+1;
    else
      return nVariants+2;
  }
  ConstitutiveModel::VariableType typeIntVar(unsigned int i) const {
    if (i < nVariants+2)
      return ConstitutiveModel::TYPE_SCALAR;
    else
      return ConstitutiveModel::TYPE_NONE;
  }
  unsigned int indexIntVar(unsigned int i) const {
    if (i < nVariants+2)
      return i;
    else
      return nVariants+2;
  }
  std::string labelIntVar(unsigned int i) const {
    if (nVariants == 1 && i == 0)
      return "martensite volume fraction";
    else if (i < nVariants) {
      char str[64];
      std::sprintf(str,"volume fraction variant %u",i+1);
      return str;
    }
    else if (i == nVariants)
      return "elastically stored energy";
    else if (i == nVariants+1)
      return "thermally stored energy";
    else
      return "";
  }
  
  // check if the material behaviour is linear ?
  bool isLinear() const {return false;}
  
  // compute the incremental potential
  double incrementalPotential(const MaterialProperties& material,
                              const ParameterSet& extPar,
                              const MaterialState& state0,MaterialState& state,
                              double dTime,MatLibMatrix& M,
                              bool update,bool tangent) 
   throw (UpdateFailedException) {
     
    // get tensors
    SYM_TENSOR eps(state.grad);
    SYM_TENSOR sig(state.flux);
    SYM_TENSOR4 K(M);
    
    // update ?
    if (update) state.internal = state0.internal;
    
    // constitutive update
    double W = constitutiveUpdate(material,extPar,eps,sig,
                                  state0.internal,state.internal,
                                  dTime,K,update,update,tangent);
        
    return W;
  }
  
 protected:
    
  // constitutive update
  double constitutiveUpdate(const MaterialProperties& material,
                            const ParameterSet& extPar,
                            const SYM_TENSOR& eps,SYM_TENSOR& sig,
                            const MatLibArray& intVar0,MatLibArray& intVar,
                            double dTime,SYM_TENSOR4& M,
                            bool update,bool stress,bool tangent) 
   throw (UpdateFailedException) {
    
    static const unsigned int ITMAX = 5;
    static const double PRECISION = 1.e-16;
    static const double TOLERANCE = 1.e-08;
    double We,Wh,Y,C;

    // 1-variant case
    if (nVariants == 1) {
      
      // get transformation strain
      StdProperty< SYM_TENSOR >& BProp
        = dynamic_cast<StdProperty< SYM_TENSOR >&>(material.getProperty("TRANSFORMATION_STRAIN"));
      SYM_TENSOR& B = BProp.value();

      // elastic energy
      SYM_TENSOR epsEl;
      double X = intVar[0];
      epsEl = eps-X*B;  // B IS IN COVARIANT FORM !!!
      We = this->storedEnergy(material,extPar,epsEl,sig,M,update || stress,
                              update || tangent);
      
      // latent heat
      Wh = latent->latentHeat(material,extPar,X,Y,C,update || stress,
                              update || tangent);

      // update volume fraction
      if (update) {
        unsigned int iter=0;
        double test0 = 1.0e0;
        for (; iter < ITMAX; iter++) {

          // test equilibrium
          double test = -innerProd(sig,B)+Y;
          if ((std::fabs(test) < TOLERANCE*test0)
              || (X < PRECISION && test > 0.0e0) 
              || ((1.0e0-X) < PRECISION && test < 0.0e0)) break;
          if (iter == 0) test0 = std::fabs(test);
        
          // compute updated volume fraction
          double dX = -test/(innerProd(B,M*B)+C);
          X += dX;
          if (X < 0.0e0) X = 0.0e0;
          if (X > 1.0e0) X = 1.0e0;

          // elastic energy
          epsEl = eps-X*B;  // B IS IN COVARIANT FORM !!!
          We = this->storedEnergy(material,extPar,epsEl,sig,M,true,true);
          
          // latent heat
          Wh = latent->latentHeat(material,extPar,X,Y,C,true,true);
        }
        if (iter == ITMAX) {
          throw UpdateFailedException("no convergence in constitutive update");
        }
        else {
          intVar[0] = X;
          intVar[1] = We;
          intVar[2] = Wh;
        }
      }
    }
    // multi-variant case
    else {
      We = Wh = 0.0e0;
      /* TO DO */
    
      // update internal variables
      if (update) {
        intVar[nVariants  ] = We;
        intVar[nVariants+1] = Wh;
      }
    }
    
    // compute tangents
    if (tangent) {

      // 1-variant case
      if (nVariants == 1) {
        // elastic step ?
        double dX = intVar[0]-intVar0[0];
        if (std::fabs(dX) > PRECISION
            && intVar[0] > PRECISION && (1.0e0-intVar[0]) > PRECISION) {
          
          // get transformation strain
          StdProperty< SYM_TENSOR >& BProp
            = dynamic_cast<StdProperty< SYM_TENSOR >&>(material.getProperty("TRANSFORMATION_STRAIN"));
          SYM_TENSOR& B = BProp.value();
          
          // compute corrective terms
          SYM_TENSOR S;
          S = M*B;
          double coef = 1.e0/(innerProd(B,S)+C);
          M -= coef*outerProd(S,S);
        }
      }
      // multi-variant case
      else {
        /* TO DO */
      }
    }
    
    return We+Wh-intVar0[nVariants]-intVar0[nVariants+1];
  }
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
