/*
 *  $Id: PolyCrystalHEModel.h 139 2013-08-30 15:33:21Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_HYPER_POLYCRYSTAL_MODEL_H
#define ZORGLIB_MATL_MECA_HYPER_POLYCRYSTAL_MODEL_H

// config
#include <matlib_macros.h>

// local
#include <data/Rotation3D.h>
#include <math/TensorAlgebra.h>
#include <matl/ConstitutiveModel.h>
#include <matl/meca/eos/EOS.h>
#include <matl/meca/hyper/GeneralHenckyPotential.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class to store polycrystal texture.
 */
class TextureProperty : public Property {
  
 private:
  
  // data
  std::vector<double> weights;
  std::vector<Rotation3D> orientations;
  
 public:
  
  // default constructor
  TextureProperty() {}
  
  // copy constructor
  TextureProperty(const TextureProperty&);
  
  // destructor
  virtual ~TextureProperty() {}
  
  // get number of grains
  unsigned int nGrains() const {return orientations.size();}

  // get orientation
  Rotation3D& orientation(unsigned int n) {return orientations[n];}
  
  // get weight
  double weight(unsigned int n) {return weights[n];}
  
  // duplicate object
  TextureProperty* clone() const {return new TextureProperty(*this);}
  
  // read from a file
  void readFrom(const char*) throw (FileException);
  void readFrom(std::istream&);

  // output as a string
  std::string toString() const;
};

/**
 * Finite-strain variational polycrystal model (Taylor averaging).
 */
template <class ALG>
class PolyCrystalHEModel : virtual public StandardMaterial {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::Tensor::TYPE     TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  typedef typename ALG::Tensor4          TENSOR4;
  
 protected:
  
  // associated equation-of-state
  EOS *eos;
  
  // constitutive model for individual grains
  StandardMaterial *gModel;
  
  // number of grains
  unsigned int nGrains;

  // instance counter
  unsigned int *count;
  
  // empty constructor
  PolyCrystalHEModel(StandardMaterial* m = 0,EOS* e = 0) {
    count = new unsigned int(1);
    eos    = e;
    gModel = m;
  }
  
 public:
    
  // constructors
  PolyCrystalHEModel(StandardMaterial& m) {
      count = new unsigned int(1);
      eos = 0;
      gModel = &m;
    }
  PolyCrystalHEModel(StandardMaterial& m,EOS& e) {
      count = new unsigned int(1);
      eos = &e;
      gModel = &m;
    }
  
  // copy constructor
  PolyCrystalHEModel(const PolyCrystalHEModel& src) {
    count = src.count;
    (*count)++;
    eos = src.eos;
    gModel = src.gModel;
  }
  
  // destructor
  virtual ~PolyCrystalHEModel() {
    if (--(*count) > 0) return;
    delete count;
    if (eos) delete eos;
    delete gModel;
  }
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\nPolycrystalline material (Taylor model):" << std::endl;

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

    // check grain model
    if (os) (*os) << "\nConstitutive model for grains:" << std::endl;
    gModel->checkProperties(material,os);
    
    // check texture
    std::string odf = material.getStringProperty("ORIENTATION_DISTRIBUTION_FILE");
    TextureProperty texture;
    texture.readFrom(odf.c_str());
    material.setProperty("TEXTURE",texture);
    nGrains = texture.nGrains();
  }
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties& material,const Rotation& R) {
    // grain model
    gModel->rotateProperties(material,R);
    
    // texture
    MatLibMatrix RMat(3),RMat0(3),RMat1(3);
    R.toMatrix(RMat);
    TextureProperty& texture = dynamic_cast<TextureProperty&>(material.getProperty("TEXTURE"));
    for (unsigned int n=0; n < texture.nGrains(); n++) {
      texture.orientation(n).toMatrix(RMat0);
      RMat1 = RMat*RMat0;
      Rotation3D R1(RMat1);
      texture.orientation(n) = R1;
    }
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& mater,const ParameterSet& extPar) {
    if (eos) eos->updateProperties(mater,extPar);
    gModel->updateProperties(mater,extPar);
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
  unsigned int nIntVar() const {return nGrains*gModel->nIntVar();}
  
  // self-documenting utilities
  unsigned int nIntVarBundled() const {return nGrains*gModel->nIntVarBundled();}
  unsigned int getIntVar(const std::string& str) const {
    size_t p = str.find_last_not_of("0123456789");
    std::string str1(str,0,p+1);
    unsigned int i = gModel->getIntVar(str1);
    if (p < str.length() && i < gModel->nIntVarBundled()) {
      unsigned int n = std::atoi(str.c_str()+p+1);
      return (n-1)*gModel->nIntVarBundled()+i;
    }
    else
      return nIntVar();
  }
  ConstitutiveModel::VariableType typeIntVar(unsigned int i) const {
    unsigned int i0 = i % gModel->nIntVarBundled();
    return gModel->typeIntVar(i0);
  }
  unsigned int indexIntVar(unsigned int i) const {
    unsigned int n = i / gModel->nIntVarBundled();
    return n*gModel->nIntVar() + gModel->indexIntVar(i-n*gModel->nIntVarBundled());
  }
  std::string labelIntVar(unsigned int i) const {
    unsigned int i0 = i % gModel->nIntVarBundled();
    return gModel->labelIntVar(i0);
  }
  
  // initialize the state of the material
  void initState(const MaterialProperties& material,MaterialState& state) {
    ConstitutiveModel::initState(material,state);
    state.grad = TENSOR::identity();
    state.flux = 0.e0;
    MaterialState tmp;
    gModel->initState(material,tmp);
    for (unsigned int n=0; n < nGrains; n++) {
      unsigned int nIntVar = gModel->nIntVar();
      MatLibArray intV(state.internal,nIntVar,n*nIntVar);
      intV = tmp.internal;
    }
  }
  
  // compute the incremental potential
  double incrementalPotential(const MaterialProperties& material,
                              const ParameterSet& extPar,
                              const MaterialState& state0,MaterialState& state,
                              double dTime,MatLibMatrix& T,
                              bool update,bool tangent) 
   throw (UpdateFailedException) {
     
    // update ?
    if (update) state.internal = state0.internal;
     
    // initialize
    unsigned int nExtVar = gModel->nExtVar();
    unsigned int nIntVar = gModel->nIntVar();
    MaterialState gState0,gState1;

    gState0.grad.resize(nExtVar);
    gState0.flux.resize(nExtVar);
    gState0.internal.resize(nIntVar);
    gState0.grad = state0.grad;
    gState0.flux = state0.flux;

    gState1.grad.resize(nExtVar);
    gState1.flux.resize(nExtVar);
    gState1.internal.resize(nIntVar);
    gState1.grad = state.grad;

    MatLibMatrix gT;
    gT.resize(nExtVar);

    // get texture
    TextureProperty& texture = dynamic_cast<TextureProperty&>(material.getProperty("TEXTURE"));

    // loop on grains
    double coef = 0.0e0;
    double W = 0.0e0;
    if (update) state.flux = 0.0e0;
    if (tangent) T = 0.0e0;
    for (unsigned int n=0; n < nGrains; n++) {

      // grain weight
      double w = texture.weight(n);
      coef += w;

      // rotate properties
      MaterialProperties mater(material);
      gModel->rotateProperties(mater,texture.orientation(n));

      // compute grain contribution to incremental potential
      MatLibArray intV0(state0.internal,nIntVar,n*nIntVar);
      gState0.internal = intV0;
      W += w*gModel->incrementalPotential(mater,extPar,gState0,gState1,
                                          dTime,gT,update,tangent);

      if (update) {
        // save internal variables
        MatLibArray intV1(state.internal,nIntVar,n*nIntVar);
        intV1 = gState1.internal;

        // compute contribution to stress tensor
        state.flux += w*gState1.flux;
      }

      if (tangent) T += w*gT;
    }

    // normalize
    coef = 1.0e0/coef;
    W *= coef;
    if (update) state.flux *= coef;
    if (tangent) T *= coef;

    return W;
  }
};


/**
 * Implementations of the model.
 */
class CubicPolyCrystalHElasticity3D : public PolyCrystalHEModel<TensorAlgebra3D> {
  
 public:
  
  // constructor
  CubicPolyCrystalHElasticity3D(EOS *eos = 0)
  : PolyCrystalHEModel<TensorAlgebra3D>(new CubicHyperElasticity3D(),eos) {}
  
  // copy constructor
  CubicPolyCrystalHElasticity3D(const CubicPolyCrystalHElasticity3D& src) 
  : PolyCrystalHEModel<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~CubicPolyCrystalHElasticity3D() {}
};

/**
 * The associated model builder
 */
class CubicPolyCrystalHElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  CubicPolyCrystalHElasticityBuilder();
  
  // the instance
  static CubicPolyCrystalHElasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~CubicPolyCrystalHElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
