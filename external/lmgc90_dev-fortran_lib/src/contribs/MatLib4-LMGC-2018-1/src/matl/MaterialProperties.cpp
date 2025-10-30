/*
 *  $Id: MaterialProperties.cpp 149 2014-08-24 19:21:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "MaterialProperties.h"

// std C library
#include <cctype>
#include <cstring>
// std C++ library
#include <fstream>
#include <string>
// local
#include <data/TabulatedFunction2.h>

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


// copy constructor
MaterialProperties::MaterialProperties(const MaterialProperties& src) {
  name = src.name;
  
  // duplicate properties
  properties.clear();
  ConstIterator it;
  for (it=src.properties.begin(); it != src.properties.end(); it++)
    properties[it->first] = it->second->clone();
}

// destructor
MaterialProperties::~MaterialProperties() {
  // delete properties
  Iterator it;
  for (it=properties.begin(); it != properties.end(); it++)
    delete it->second;
  properties.clear();
}

// assignment operator
MaterialProperties& MaterialProperties::operator=(
                                    const MaterialProperties& src) {
  name = src.name;
  
  // delete existing properties
  Iterator it1;
  for (it1=properties.begin(); it1 != properties.end(); it1++)
    delete it1->second;
  properties.clear();
  
  // duplicate properties
  ConstIterator it2;
  for (it2=src.properties.begin(); it2 != src.properties.end(); it2++)
    properties[it2->first] = it2->second->clone();
  
  return *this;
}

// check if property exists
bool MaterialProperties::checkProperty(const std::string& keyword) const {
  if (properties.count(keyword))
    return true;
  else
    return false;
}


// get property associated to keyword
Property& MaterialProperties::getProperty(const std::string& keyword) const
 throw (NoSuchPropertyException) {
  if (!properties.count(keyword))
    throw NoSuchPropertyException(keyword);
  Property *p = properties.find(keyword)->second;
  if (!p) throw NoSuchPropertyException(keyword);
  return *p;
}

int MaterialProperties::getIntegerProperty(const std::string& keyword) const
 throw (NoSuchPropertyException) {
  if (!properties.count(keyword))
    throw NoSuchPropertyException(keyword);
  IntegerProperty *p
    = dynamic_cast<IntegerProperty*>(properties.find(keyword)->second);
  if (!p) throw NoSuchPropertyException(keyword);
  return p->value();
}

double MaterialProperties::getDoubleProperty(const std::string& keyword) const
 throw (NoSuchPropertyException) {
  if (!properties.count(keyword))
    throw NoSuchPropertyException(keyword);
  DoubleProperty *p
    = dynamic_cast<DoubleProperty*>(properties.find(keyword)->second);
  if (!p) throw NoSuchPropertyException(keyword);
  return p->value();
}

std::string MaterialProperties::getStringProperty(const std::string& keyword) const
 throw (NoSuchPropertyException) {
  if (!properties.count(keyword))
    throw NoSuchPropertyException(keyword);
  StringProperty *p
    = dynamic_cast<StringProperty*>(properties.find(keyword)->second);
  if (!p) throw NoSuchPropertyException(keyword);
  return p->value();
}

Function& 
MaterialProperties::getFunctionProperty(const std::string& keyword) const
 throw (NoSuchPropertyException) {
  if (!properties.count(keyword))
    throw NoSuchPropertyException(keyword);
  FunctionProperty *p
    = dynamic_cast<FunctionProperty*>(properties.find(keyword)->second);
  if (!p) throw NoSuchPropertyException(keyword);
  return p->function();
}

// set property associated to keyword

void MaterialProperties::setProperty(const std::string& keyword,Property& prp) {
  if (properties.count(keyword)) {
    Property *p = properties[keyword];
    if (p) delete p;
  }
  properties[keyword] = prp.clone();
}

void MaterialProperties::setProperty(const std::string& keyword,int value) {
  if (properties.count(keyword)) {
    IntegerProperty *p = dynamic_cast<IntegerProperty*>(properties[keyword]);
    if (!p) return;
    p->setValue(value);
  }
  else
    properties[keyword] = new IntegerProperty(value);
}

void MaterialProperties::setProperty(const std::string& keyword,double value) {
  if (properties.count(keyword)) {
    DoubleProperty *p = dynamic_cast<DoubleProperty*>(properties[keyword]);
    if (!p) return;
    p->setValue(value);
  }
  else
    properties[keyword] = new DoubleProperty(value);
}

void MaterialProperties::setProperty(const std::string& keyword,const char* str) {
  if (properties.count(keyword)) {
    StringProperty *p = dynamic_cast<StringProperty*>(properties[keyword]);
    if (!p) return;
    p->setValue(str);
  }
  else
    properties[keyword] = new StringProperty(str);
}

void MaterialProperties::setProperty(const std::string& keyword,Function& fct) {
  if (properties.count(keyword)) {
    FunctionProperty *p = dynamic_cast<FunctionProperty*>(properties[keyword]);
    if (!p) return;
    p->setFunction(fct);
  }
  else
    properties[keyword] = new FunctionProperty(fct);
}

// read material properties from an input stream
void MaterialProperties::readFrom(std::istream& is,const char* prefix)
 throw (SyntaxError) {
  
  char buffer[256],*key,*strval,*type;
  size_t pos;
  
  // read property set's name
  is.getline(buffer,255);
  name = buffer;
  
  // read properties
  while(!is.eof()) {
    is.getline(buffer,255);
    // get keyword
    key = std::strtok(buffer,"= \t");
    // skip blank lines
    if (key == 0) continue;
    // skip comments
    if (key[0] == '#' || key[0] == '!') continue;
    // get value
    strval = key+std::strlen(key)+1;
    pos = std::strspn(strval,"= \t");
    strval += pos;
    if (strval[0] == '[') // tabulated function
      pos = std::strcspn(strval,"]")+1;
    else if (strval[0] == '\"') // filed function
      pos = std::strcspn(strval+1,"\"")+2;
    else // other
      pos = std::strcspn(strval,"( \t");
    strval[pos] = '\0';
    // get property type
    type = std::strtok(strval+pos+1,"() \t");
    
    // make key all upper case
    size_t l = std::strlen(key);
    for (size_t i=0; i < l; i++) key[i] = std::toupper(key[i]);
    
    // store property
    if (std::strncmp(type,"integer",3) == 0)
      setProperty(key,(int)strtol(strval,0,10));
    else if (std::strcmp(type,"real") == 0)
      setProperty(key,strtod(strval,0));
    else if (std::strcmp(type,"string") == 0) {
      l = std::strlen(strval);
      strval[l-1] = '\0';
      setProperty(key,strval+1);
    }
    else if (std::strcmp(type,"file") == 0) {
      char buffer1[256];
      l = std::strlen(strval);
      strval[l-1] = '\0';
      if (prefix) {
        std::strcpy(buffer1,prefix);
        std::strcat(buffer1,++strval);
      }
      else
        std::strcpy(buffer1,++strval);
      setProperty(key,buffer1);
    }
    else if (std::strncmp(type,"function",8) == 0) {
      /* read directly */
      if (strval[0] == '[') {
        char *strpt = std::strtok(strval+1,";]");
        std::vector<double> x,y;
        while (strpt) {
          l = std::strlen(strpt);
          char *strx = std::strtok(strpt,", ");
          char *stry = std::strtok(0,", ");
          x.push_back(std::strtod(strx,0));
          y.push_back(std::strtod(stry,0));
          strpt = std::strtok(strpt+l+1,";]");
        }
        unsigned int nPts = x.size();
        if (type[8] == '2') {
          TabulatedFunction2 fct(key,nPts);
          for (unsigned int i=0; i < nPts; i++) fct.setPoint(i,x[i],y[i]);
          setProperty(key,fct);
        }
        else {
          TabulatedFunction fct(key,nPts);
          for (unsigned int i=0; i < nPts; i++) fct.setPoint(i,x[i],y[i]);
          setProperty(key,fct);
        }
      }
      /* read from file */
      else if (strval[0] == '\"') {
        char buffer1[256];
        l = std::strlen(strval);
        strval[l-1] = '\0';
        if (prefix) {
          std::strcpy(buffer1,prefix);
          std::strcat(buffer1,++strval);
        }
        else
          std::strcpy(buffer1,++strval);
        std::ifstream fctFile(buffer1);
        if (!fctFile.is_open()) {
          std::string msg("cannot open function file ");
          msg += buffer1;
          throw SyntaxError(msg.c_str());
        }
        fctFile.getline(buffer1,255);
        std::vector<double> x,y;
        while (!fctFile.eof()) {
          char buffer2[256];
          char* word;
          fctFile.getline(buffer2,255);
          word = std::strtok(buffer2," \t");
          if (word == 0) break;
          x.push_back(std::strtod(word,0));
          word = std::strtok(0," \t");
          y.push_back(std::strtod(word,0));
        }
        unsigned int nPts = x.size();
        if (type[8] == '2') {
          TabulatedFunction2 fct(buffer1,nPts);
          for (unsigned int i=0; i < nPts; i++) fct.setPoint(i,x[i],y[i]);
          setProperty(key,fct);
        }
        else {
          TabulatedFunction fct(buffer1,nPts);
          for (unsigned int i=0; i < nPts; i++) fct.setPoint(i,x[i],y[i]);
          setProperty(key,fct);
        }
      }
      else
        throw SyntaxError("invalid function");
    }
    else
      throw SyntaxError("unknown property type");
  }
}
void MaterialProperties::readFrom(const char* iFileName)
 throw (FileException, SyntaxError) {
   
  // open input file
  if (iFileName) {
    std::ifstream file(iFileName);
    if (!file.is_open()) {
      std::string msg("cannot open material file: ");
      msg += iFileName;
      throw FileException(msg);
    }
    // extract prefix
    const char* tmp = std::strrchr(iFileName,'/');
    if (tmp) {
      char prefix[256];
      size_t plen = tmp+1-iFileName;
      std::strncpy(prefix,iFileName,plen);
      prefix[plen] = '\0';
      readFrom(file,prefix);
    }
    else
      readFrom(file);
  }
  else 
    readFrom(std::cin);
}

// utility functions
void MaterialProperties::pullProperties(unsigned int i,const MaterialProperties& mp1,MaterialProperties& mp2) {
  ConstIterator iter;
  for (iter=mp1.begin(); iter != mp1.end(); iter++) {
    size_t p = iter->first.find_last_not_of("0123456789");
    if (p >= iter->first.length()) continue;
    unsigned int n = std::atoi(iter->first.c_str()+p+1);
    if (n != i+1) continue;
    std::string str(iter->first,0,p);
    mp2.setProperty(str,*(iter->second));
  }
}
void MaterialProperties::pushProperties(unsigned int i,const MaterialProperties& mp1,MaterialProperties& mp2) {
  ConstIterator iter;
  for (iter=mp1.begin(); iter != mp1.end(); iter++) {
    char str[64];
    std::sprintf(str,"%s@%u",iter->first.c_str(),i+1);
    mp2.setProperty(str,*(iter->second));
  }
}
