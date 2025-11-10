/*
 *  $Id: ThermoElasticSMA.h 142 2014-02-07 12:51:54Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_THERMO_LINEAR_ELASTIC_SMA_H
#define ZORGLIB_MATL_MECA_THERMO_LINEAR_ELASTIC_SMA_H

// config
#include <matlib_macros.h>

// std C library
#include <cstdio>
// local
#include <matl/thermomeca/linear/ThermoElasticity.h>
#include <matl/meca/linear/ElasticSMA.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for latent heat potentials.
 */
class LatentHeatThermalPotential : virtual public LatentHeatPotential {
  
 protected:
  
  // constructor
  LatentHeatThermalPotential() {}
  
 public:
  
  // destructor
  virtual ~LatentHeatThermalPotential() {}

  // compute latent heat energy
  using LatentHeatPotential::latentHeat;
  virtual double latentHeat(const MaterialProperties&,const ParameterSet&,
                            double,double,double&,double&,
                            double&,double&,double&,bool,bool) = 0;
};


/**
 * Base class for (geometrically linear) thermo-elastic shape memory alloys.
 */
template <class ALG>
class ThermoElasticSMA : virtual public ThermoElasticity<ALG> {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
 protected:

  // number of variants
  unsigned int nVariants;

  // associated latent heat
  LatentHeatThermalPotential *latent;
  
  // empty constructor
  ThermoElasticSMA(LatentHeatThermalPotential* l = 0) {
    nVariants = ElasticSMA<ALG>::DEFAULT_N_VARIANTS;
    latent = l;
  }
  
 public:
    
  // constructors
  ThermoElasticSMA(typename ThermoElasticity<ALG>::Potential& p,
                   LinThermalCapacity& c,LatentHeatThermalPotential& l) 
  : ThermoElasticity<ALG>(p,c) {
    nVariants = ElasticSMA<ALG>::DEFAULT_N_VARIANTS;
    latent = &l;
  }
  ThermoElasticSMA(typename ThermoElasticity<ALG>::Potential& p,
                   LinThermalCapacity& c,LatentHeatThermalPotential& l,
                   typename ThermoElasticity<ALG>::Dilatancy& d) 
  : ThermoElasticity<ALG>(p,c,d) {
    nVariants = ElasticSMA<ALG>::DEFAULT_N_VARIANTS;
    latent = &l;
  }
  
  // copy constructor
  ThermoElasticSMA(const ThermoElasticSMA& src) 
  : ThermoElasticity<ALG>(src) {nVariants = src.nVariants; latent = src.latent;}
  
  // destructor
  virtual ~ThermoElasticSMA() {
    if (*(this->count) > 1) return;
    if (latent) delete latent;
  }
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\nLinear thermo-elastic shape-memory alloy:" << std::endl;

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

    // check capacity
    this->capacity->checkProperties(material,os);

    // check dilatancy
    if (this->dilatancy) this->dilatancy->checkProperties(material,os);
    
    // check variants
    nVariants = ElasticSMA<ALG>::checkVariants(material,os);

    // check latent heat
    if (latent) latent->checkProperties(material,os);
  }
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties& material,const Rotation& R) {
    this->potential->rotateProperties(material,R);
    if (this->dilatancy) this->dilatancy->rotateProperties(material,R);
    ElasticSMA<ALG>::rotateVariants(material,R);
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& mater,const ParameterSet& extPar) {
    this->potential->updateProperties(mater,extPar);
    this->capacity->updateProperties(mater,extPar);
    if (this->dilatancy) this->dilatancy->updateProperties(mater,extPar);
    latent->updateProperties(mater,extPar);
  }
  
  // how many internal variables ?
  unsigned int nIntVar() const {return nVariants+3;}
  
  // self-documenting utilities
  unsigned int nIntVarBundled() const {return nVariants+3;}
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
    else if (str == "ENTP")
      return nVariants;
    else if (str == "ENRG")
      return nVariants+1;
    else if (str == "TNRG")
      return nVariants+2;
    else
      return nVariants+3;
  }
  ConstitutiveModel::VariableType typeIntVar(unsigned int i) const {
    if (i < nVariants+3)
      return ConstitutiveModel::TYPE_SCALAR;
    else
      return ConstitutiveModel::TYPE_NONE;
  }
  unsigned int indexIntVar(unsigned int i) const {
    if (i < nVariants+3)
      return i;
    else
      return nVariants+3;
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
      return "entropy";
    else if (i == nVariants+1)
      return "elastically stored energy";
    else if (i == nVariants+2)
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
    
    // get temperature
    double Th0 = state0.grad[SYM_TENSOR::MEMSIZE];
    double Th1 = state.grad[SYM_TENSOR::MEMSIZE];
    double dN,C;
    SYM_TENSOR dSig;
    
    // constitutive update
    double W = constitutiveUpdate(material,extPar,eps,Th1,sig,dN,
                                  state0.internal,state.internal,dTime,
                                  K,dSig,C,update,update,tangent);
    
    // update
    if (update) {
      state.flux[SYM_TENSOR::MEMSIZE] = dN;
    }
    if (tangent) {
      M[SYM_TENSOR::MEMSIZE][SYM_TENSOR::MEMSIZE] = C;
      for (unsigned int i=0; i < SYM_TENSOR::MEMSIZE; i++)
        M[i][SYM_TENSOR::MEMSIZE] = M[SYM_TENSOR::MEMSIZE][i] = dSig[i];
    }
    
    return W+state0.internal[nVariants]*(Th1-Th0);
  }
  
 protected:
    
  // constitutive update
  double constitutiveUpdate(const MaterialProperties& material,
                            const ParameterSet& extPar,
                            const SYM_TENSOR& eps,double Th,
                            SYM_TENSOR& sig,double& dN,
                            const MatLibArray& intVar0,MatLibArray& intVar,
                            double dTime,SYM_TENSOR4& M,SYM_TENSOR& dSig,
                            double& C,bool update,bool stress,bool tangent) 
   throw (UpdateFailedException) {
    
    static const unsigned int ITMAX = 5;
    static const double PRECISION = 1.e-16;
    static const double TOLERANCE = 1.e-08;
    double We,Wh,N,Nh,Y,Ch11,Ch12,Ch22;

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
      We = this->storedEnergy(material,extPar,epsEl,Th,sig,N,M,dSig,C,
			      update || stress,update || tangent);
      
      // latent heat
      Wh = latent->latentHeat(material,extPar,Th,X,Nh,Y,Ch11,Ch22,Ch12,
                              update || stress,update || tangent);

      // update volume fraction
      if (update) {
        unsigned int iter=0;
        double test0 = 1.0e0;
        for (; iter < ITMAX; iter++) {

          // test equilibrium
          double test = -innerProd(sig,B)+Y;
          if ((std::fabs(test) < TOLERANCE*test0)
              || (X < PRECISION && test > 0.0e0) 
              || ((1.0-X) < PRECISION && test < 0.0e0)) break;
          if (iter == 0) test0 = std::fabs(test);
        
          // compute updated volume fraction
          double dX = -test/(innerProd(B,M*B)+Ch22);
          X += dX;
          if (X < 0.0e0) X = 0.0e0;
          if (X > 1.0e0) X = 1.0e0;

          // elastic energy
          epsEl = eps-X*B;  // B IS IN COVARIANT FORM !!!
          We = this->storedEnergy(material,extPar,epsEl,Th,sig,N,M,dSig,C,
				  true,true);
          
          // latent heat
          Wh = latent->latentHeat(material,extPar,Th,X,Nh,Y,
                                  Ch11,Ch22,Ch12,true,true);
        }
        if (iter == ITMAX) {
          std::cout << "eps=" << eps << ", T=" << Th << ", X=" << X << std::endl;
          throw UpdateFailedException("no convergence in constitutive update");
        }
        else {
          intVar[0] = X;
          intVar[2] = We;
        }
      }
    }
    // multi-variant case
    else {
      We = Wh = 0.0e0;
      /* TO DO */
    }
    
    // thermal capacity
    if (this->capacity) {
      double NT,CT;
      double WT = this->capacity->internalEnergy(material,extPar,Th,NT,
                                                 CT,stress,tangent);
      Wh += WT;
      if (stress) Nh += NT;
      if (tangent) Ch11 += CT;
    }
    
    // update internal variables
    if (update) {
      intVar[nVariants] = -N-Nh;
      intVar[nVariants+1] = We;
      intVar[nVariants+2] = Wh;
    }
    
    // compute first and second derivatives
    if (stress) {
      dN = N+Nh+intVar0[nVariants];
    }
    
    if (tangent) {
      C += Ch11;
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
          double coef = 1.e0/(innerProd(B,S)+Ch22);
          M -= coef*outerProd(S,S);
          double val = innerProd(dSig,B)-Ch12;
          dSig -= (coef*val)*S;
          C -= coef*val*val;
        }
      }
      // multi-variant case
      else {
        /* TO DO */
      }
    }
    
    return We+Wh-intVar0[nVariants+1]-intVar0[nVariants+2];
  }
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
