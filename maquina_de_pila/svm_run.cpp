#include <sstream>
#include <iostream>
#include <stdlib.h>
#include <cstring>
#include <fstream>

#include "svm_parser.hh"
#include "svm.hh"

int main(int argc, const char* argv[]) {

  bool usarParser = true;
  SVM* svm;

  if (usarParser) {
  
    if (argc != 2) {
      cout << "Falta el nombre del archivo" << endl;
      exit(1);
    }
    cout << "Leyendo programa del archivo " << argv[1] << endl;
    std::ifstream t(argv[1]);
    std::stringstream buffer;
    buffer << t.rdbuf();
    Scanner scanner(buffer.str());
  
    Parser parser(&scanner);
    svm = parser.parse();

  } else {

    list<Instruction*> listaInstrucciones;
    listaInstrucciones.push_back(new Instruction("", Instruction::IPUSH, 30));
    listaInstrucciones.push_back(new Instruction("", Instruction::IPUSH, 3));
    listaInstrucciones.push_back(new Instruction("", Instruction::IDIV));
    listaInstrucciones.push_back(new Instruction("", Instruction::IDUP));
    listaInstrucciones.push_back(new Instruction("", Instruction::IPUSH, 2));  
    listaInstrucciones.push_back(new Instruction("", Instruction::IADD));
    listaInstrucciones.push_back(new Instruction("", Instruction::ISWAP));
    listaInstrucciones.push_back(new Instruction("", Instruction::IGE));
    listaInstrucciones.push_back(new Instruction("", Instruction::IJMPN, "L20"));
    listaInstrucciones.push_back(new Instruction("", Instruction::IPUSH, 10));		  
    listaInstrucciones.push_back(new Instruction("", Instruction::IGOTO, "LEND")); // 9
    listaInstrucciones.push_back(new Instruction("L20", Instruction::IPUSH, 20));
    listaInstrucciones.push_back(new Instruction("LEND", Instruction::ISKIP));
   
    svm = new SVM(listaInstrucciones);

  }
  
  cout << "Programa:" << endl;
  svm->imprimir();
  cout << "----------------" << endl;

  cout << "Ejecutando ...." << endl;
  svm->ejecutar();
  cout << "Terminado" << endl;

  svm->imprimir_pila();
  
}