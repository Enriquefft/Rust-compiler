#include <iostream>

#include "svm.hh"

string nombresInstrucciones[33] = { "push", "pop", "dup", "swap", "add", "sub", "mult", "div", "goto", "eq", "gt", "ge", "lt", "le", "skip", "store", "load", "print", "jmpz", "jmpn", "and", "or", "neg", "not", "halt", "call", "mark", "storer", "loadr", "pusha", "enter", "alloc", "return" };

Instruction::Instruction(string etiqueta, IType tipoInstruccion):etiqueta(etiqueta),tipo(tipoInstruccion),tieneArg(false) {}
Instruction::Instruction(string etiqueta, IType tipoInstruccion, int arg):etiqueta(etiqueta),tipo(tipoInstruccion),tieneArg(true), argEntero(arg),etiquetaSalto("") {}
Instruction::Instruction(string etiqueta, IType tipoInstruccion, string arg):etiqueta(etiqueta),tipo(tipoInstruccion),tieneArg(true),etiquetaSalto(arg) {}

SVM::SVM(list<Instruction*>& listaInstrucciones) {
  instrucciones.reserve(listaInstrucciones.size());
  copy(begin(listaInstrucciones), end(listaInstrucciones), back_inserter(instrucciones));
  sp = pc = 0;
  sp = maxPila = 0;
  hp = TAM_MEMORIA;
  ep = hp-1;
  for(int i=0; i < instrucciones.size(); i++) {
    string etiqueta = instrucciones[i]->etiqueta;
    if (etiqueta != "")
      etiquetas[etiqueta] = i;
  }
  for(int i=0; i < instrucciones.size(); i++) {
    string etiquetaSalto = instrucciones[i]->etiquetaSalto;
    if (etiquetaSalto != "") {
      std::unordered_map<std::string,int>::const_iterator it = etiquetas.find(etiquetaSalto);
      if (it == etiquetas.end()) {
        cout << "No se encontró la etiqueta " << etiquetaSalto << endl;
        exit(0);
      } else
        instrucciones[i]->argEntero = it->second;
    }
  }
}

void SVM::ejecutar() {
  while (true) {
    if (pc >= instrucciones.size()) {
      cout << "Contador de programa fuera de límites...saliendo" << endl;
      break;
    }
    if (!ejecutar(instrucciones[pc])) break;
  }
}

bool SVM::ejecutar(Instruction* instr) {
  Instruction::IType tipoInstruccion = instr->tipo;
  int siguiente, cima;
  if (tipoInstruccion==Instruction::IPOP || tipoInstruccion==Instruction::IDUP ||
      tipoInstruccion==Instruction::IPRINT || tipoInstruccion==Instruction::ISKIP) {
    switch (tipoInstruccion) {
    case(Instruction::IPOP):
      if (pila_vacia()) error_svm("No se puede hacer pop de una pila vacía");
      pila_pop(); break;
    case(Instruction::IDUP):
      if (pila_vacia()) error_svm("No se puede duplicar de una pila vacía");
      pila_push(pila_top()); break;
    case(Instruction::IPRINT): cout << pila_top() << endl; pila_pop(); break;
    case(Instruction::ISKIP): break;
    default: error_svm("Error de programación 1");
    }
    pc++;
  } else if (tipoInstruccion==Instruction::IPUSH || tipoInstruccion==Instruction::ISTORE ||
             tipoInstruccion==Instruction::ILOAD || tipoInstruccion==Instruction::IALLOC ||
             tipoInstruccion==Instruction::ISTORER || tipoInstruccion==Instruction::ILOADR ||
             tipoInstruccion==Instruction::IPUSHA || tipoInstruccion==Instruction::ILOADA)  {
    int arg = instr->argEntero;
    switch (tipoInstruccion) {
    case(Instruction::IPUSH): pila_push(instr->argEntero); break;
    case(Instruction::IPUSHA): pila_push(instr->argEntero); break;
    case (Instruction::ISTORE):
      if (pila_vacia()) error_svm("No se puede almacenar desde una pila vacía");
      memoria_escribir(instr->argEntero, pila_top()); pila_pop(); break;
    case (Instruction::ISTORER):
      if (pila_vacia()) error_svm("No se puede almacenar desde una pila vacía");
      memoria_escribir(fp+instr->argEntero, pila_top()); pila_pop(); break;
    case(Instruction::ILOAD):
      pila_push(memoria_leer(instr->argEntero)); break;
    case(Instruction::ILOADR):
      pila_push(memoria_leer(fp+instr->argEntero)); break;
    case(Instruction::IALLOC):
      ejecutar_alloc(arg);
      break;
    case(Instruction::ILOADA):
      pila_push(memoria_leer(fp + arg));
      break;
    default: error_svm("Error de programación 2");
    }
    pc++;
  } else if (tipoInstruccion==Instruction::IJMPZ || tipoInstruccion==Instruction::IJMPN ) {
    bool salto = false;
    cima = pila_top(); pila_pop();
    switch(tipoInstruccion) {
    case(Instruction::IJMPZ): salto = (cima==0); break;
    case(Instruction::IJMPN): salto = (cima!=0); break;
    default: error_svm("Error de programación 3");
    }
    if (salto) pc=instr->argEntero; else pc++;
  } else if
      (tipoInstruccion==Instruction::IADD || tipoInstruccion==Instruction::ISUB || tipoInstruccion==Instruction::IMUL ||
       tipoInstruccion==Instruction::IDIV || tipoInstruccion==Instruction::IEQ || tipoInstruccion==Instruction::IGT ||
       tipoInstruccion==Instruction::IGE  || tipoInstruccion==Instruction::ILT || tipoInstruccion==Instruction::ILE ||
       tipoInstruccion==Instruction::IAND  || tipoInstruccion==Instruction::IOR || tipoInstruccion==Instruction::ISWAP)  {
    cima = pila_top(); pila_pop();
    siguiente = pila_top(); pila_pop();
    switch(tipoInstruccion) {
    case(Instruction::IADD): pila_push(siguiente+cima); break;
    case(Instruction::ISUB): pila_push(siguiente-cima); break;
    case(Instruction::IMUL): pila_push(siguiente*cima); break;
    case(Instruction::IDIV): pila_push(siguiente/cima); break;
    case(Instruction::IEQ): pila_push(siguiente==cima?1:0); break;
    case(Instruction::IGT): pila_push(siguiente>cima?1:0); break;
    case(Instruction::IGE): pila_push(siguiente>=cima?1:0); break;
    case(Instruction::ILT): pila_push(siguiente<cima?1:0); break;
    case(Instruction::ILE): pila_push(siguiente<=cima?1:0); break;
    case(Instruction::IAND): pila_push(siguiente&&cima?1:0); break;
    case(Instruction::IOR): pila_push(siguiente||cima?1:0); break;
    case(Instruction::ISWAP): pila_push(cima); pila_push(siguiente); break;
    default: error_svm("Error de programación 4");
    }
    pc++;
  } else if (tipoInstruccion == Instruction::IGOTO) {
    pc = instr->argEntero;
  } else if ((tipoInstruccion == Instruction::INEG) || (tipoInstruccion == Instruction::INOT)) {
    cima = pila_top(); pila_pop();
    if (tipoInstruccion == Instruction::INEG) {
      cima = -cima;
    } else {
      cima = (cima==0)?1:0;
    }
    pila_push(cima);
    pc++;
  } else if (tipoInstruccion == Instruction::IMARK) {
    ejecutar_mark();
    pc++;
  } else if (tipoInstruccion == Instruction::ICALL) {
    ejecutar_call(); pc++;
  } else if (tipoInstruccion == Instruction::IENTER) {
    ejecutar_enter(instr->argEntero); pc++;
  } else if (tipoInstruccion == Instruction::IRETURN) {
    ejecutar_return(instr->argEntero); pc++;
  } else if (tipoInstruccion == Instruction::IHALT) {
    return false;
  }
  else {
    cout << "Error de programación: ejecutar instrucción" << endl;
    exit(0);
  }
  return true;
}

void SVM::imprimir_pila() {
  int i = 0;
  cout << "pila [ ";
  while(i <= sp) {
    cout << pila[i++] << " ";
  }
  cout << "]" << endl;
}

void SVM::imprimir() {
  for(int i= 0; i < instrucciones.size(); i++) {
    Instruction* s = instrucciones[i];
    if (s->etiqueta != "")
      cout << s->etiqueta << ": ";
    cout << nombresInstrucciones[s->tipo] << " ";
    if (s->tieneArg) {
      if (s->etiquetaSalto == "")
        cout << s->argEntero;
      else {
        cout << s->etiquetaSalto;
      }
    }
    cout << endl;
  }
}

void SVM::pila_pop() {
  sp--;
}

void SVM::pila_push(int valor) {
  sp++;
  pila[sp] = valor;
}

int SVM::pila_top() {
  return pila[sp];
}

bool SVM::pila_vacia() {
  return (sp == 0);
}

void SVM::memoria_escribir(int direccion, int valor) {
  verificar_memoria(direccion);
  pila[direccion] = valor;
}

int SVM::memoria_leer(int direccion) {
  verificar_memoria(direccion);
  return pila[direccion];
}

void SVM::error_svm(string mensaje) {
  cout << "error: " << mensaje << endl;
  exit(0);
}

bool SVM::verificar_memoria(int direccion) {
  if (direccion > sp) error_svm("Acceso a memoria fuera de la memoria asignada");
  if (direccion <= 0) {
    cout << "Inválido: " << direccion << endl;
    error_svm("Acceso a memoria: dirección inválida");
  }
  return true;
}

void SVM::ejecutar_mark() {
  pila[++sp] = ep;
  pila[++sp] = fp;
  return;
}

void SVM::ejecutar_call() {
  int tmp = pc;
  fp = sp;
  pc = pila[sp];
  pila[sp] = tmp;
  return;
}

void SVM::ejecutar_enter(int m) {
  ep = sp +  m;
  if (ep >= hp) error_svm("Desbordamiento de pila");
  return;
}

void SVM::ejecutar_alloc(int n) {
  sp += n;
}

void SVM::ejecutar_return(int n) {
  pc = pila[fp];
  ep = pila[fp-2];
  if (ep >= hp) error_svm("Desbordamiento de pila");
  sp = fp-n;
  fp = pila[fp-1];
  return;
}
