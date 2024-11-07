#ifndef SVMHH
#define SVMHH

#define TAM_MEMORIA 5000

#include <string>
#include <list>
#include <stack>
#include <vector>
#include <unordered_map>
using namespace std;

class Instruction {
public:
  enum IType { IPUSH=0, IPOP, IDUP, ISWAP, IADD, ISUB, IMUL, IDIV, IGOTO, IEQ, IGT,
  IGE, ILT, ILE, ISKIP, ISTORE, ILOAD, IPRINT, IJMPZ, IJMPN, IAND, IOR, INEG, INOT,
  IHALT, ICALL, IMARK, ISTORER, ILOADR, IPUSHA, IENTER, IALLOC, IRETURN, ILOADA };
  string etiqueta, etiquetaSalto;
  IType tipo;
  bool tieneArg;
  int argEntero;
  Instruction(string etiq, IType tipoInstruccion);
  Instruction(string etiq, IType tipoInstruccion, int arg);
  Instruction(string etiq, IType tipoInstruccion, string arg);
};

class SVM {
private:
  int sp, pc, fp, ep, hp;
  int pila[TAM_MEMORIA];
  int maxPila;
  vector<Instruction*> instrucciones;
  unordered_map<string,int> etiquetas;
  bool ejecutar(Instruction* s);
  void error_svm(string msg);
  void pila_pop();
  void pila_push(int v);
  int pila_top();
  bool pila_vacia();
  void memoria_escribir(int, int);
  int memoria_leer(int);
  bool verificar_memoria(int);
  void ejecutar_mark();
  void ejecutar_call();
  void ejecutar_enter(int m);
  void ejecutar_alloc(int n);
  void ejecutar_return(int n);
public:
  SVM(list<Instruction*>& sl);
  void ejecutar();
  void imprimir_pila();
  void imprimir();
  int cima();
};

#endif