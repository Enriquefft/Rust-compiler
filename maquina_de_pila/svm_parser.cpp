#include <sstream>
#include <iostream>
#include <stdlib.h>
#include <cstring>
#include <fstream>
#include "svm_parser.hh"

const char* Token::token_names[39] = { "ID", "LABEL", "NUM", "EOL", "ERR", "END",
"PUSH", "EQ", "GT", "GE", "LT", "LE", "GOTO", "SKIP", "POP", "DUP", "SWAP", "ADD",
"SUB", "MUL", "DIV", "STORE", "LOAD", "PRINT", "JMPZ", "JMPN", "AND", "OR", "NEG",
"NOT", "HALT", "CALL", "MARK", "ENTER", "ALLOC", "STORER", "LOADR", "LOADA", "RETURN"};

Token::Token(Type tipo):tipo(tipo) { lexema = ""; }
Token::Token(Type tipo, char c):tipo(tipo) { lexema = c; }
Token::Token(Type tipo, const string fuente):tipo(tipo) {
  lexema = fuente;
}

std::ostream& operator << ( std::ostream& outs, const Token & tok ){
  if (tok.lexema.empty())
    return outs << Token::token_names[tok.tipo];
  else
    return outs << Token::token_names[tok.tipo] << "(" << tok.lexema << ")";
}

std::ostream& operator << ( std::ostream& outs, const Token* tok ) {
  return outs << *tok;
}

Scanner::Scanner(string s):entrada(s),primero(0),actual(0) {
  // Inicializa el mapa de palabras reservadas
  reservado["skip"] = Token::SKIP;
  reservado["pop"] = Token::POP;
  reservado["dup"] = Token::DUP;
  reservado["swap"] = Token::SWAP;
  reservado["add"] = Token::ADD;   
  reservado["sub"] = Token::SUB;
  reservado["mul"] = Token::MUL;
  reservado["div"] = Token::DIV;
  reservado["eq"] = Token::EQ;
  reservado["gt"] = Token::GT;
  reservado["ge"] = Token::GE;
  reservado["lt"] = Token::LT;
  reservado["le"] = Token::LE;
  reservado["and"] = Token::AND;
  reservado["or"] = Token::OR;
  reservado["print"] = Token::PRINT;
  reservado["halt"] = Token::HALT;
  reservado["call"] = Token::CALL;
  reservado["mark"] = Token::MARK;
  reservado["neg"] = Token::NEG;
  reservado["not"] = Token::NOT;
  reservado["push"] = Token::PUSH;
  reservado["store"] = Token::STORE;
  reservado["load"] = Token::LOAD;
  reservado["storer"] = Token::STORER;
  reservado["loadr"] = Token::LOADR;
  reservado["pusha"] = Token::PUSHA;
  reservado["enter"] = Token::ENTER;
  reservado["alloc"] = Token::ALLOC;
  reservado["loada"] = Token::LOADA;
  reservado["goto"] = Token::GOTO;
  reservado["jmpz"] = Token::JMPZ;
  reservado["jmpn"] = Token::JMPN;
  reservado["return"] = Token::RETURN;
}

Token* Scanner::nextToken() {
  Token* token;
  char c;
  string lex;
  Token::Type tipoToken;
  c = nextChar();
  while (c == ' ') c = nextChar(); // Ignora espacios en blanco
  if (c == '\0') return new Token(Token::END); // Fin de entrada
  startLexema();
  estado = 0;
  while (1) {
    switch (estado) {
    case 0:
      if (isalpha(c)) { estado = 1; } // Identificador
      else if (isdigit(c)||(c=='-')) { startLexema(); estado = 4; } // Número
      else if (c == '\n') estado = 6; // Fin de línea
      else return new Token(Token::ERR, c); // Error
      break;
    case 1:
      c = nextChar();
      if (isalpha(c) || isdigit(c) || c=='_') estado = 1; // Continuar identificador
      else if (c == ':') estado = 3; // Etiqueta
      else estado = 2; // Fin de identificador
      break;
    case 4:
      c = nextChar();
      if (isdigit(c)) estado = 4; // Continuar número
      else estado = 5; // Fin de número
      break;
    case 6:
      c = nextChar();
      if (c == '\n') estado = 6; // Continuar fin de línea
      else estado = 7; // Fin de línea
      break;
    case 2:
      rollBack();
      lex = getLexema();
      tipoToken = checkReserved(lex);
      if (tipoToken != Token::ERR)
        return new Token(tipoToken);
      else
        return new Token(Token::ID, getLexema()); 
    case 3:
      rollBack();
      token = new Token(Token::LABEL,getLexema());
      nextChar();
      return token;
    case 5:
      rollBack();
      lex = getLexema();
      if (lex.size()==1 && lex[0]=='-') {
        return new Token(Token::ERR, lex);
      }
      return new Token(Token::NUM,lex);
    case 7:
      rollBack();
      return new Token(Token::EOL);
    default:
      cout << "Error de programación ... saliendo" << endl;
      exit(0);
    }
  }
}

Scanner::~Scanner() { }

char Scanner::nextChar() {
  int c = entrada[actual];
  actual++;
  return c;
}

void Scanner::rollBack() {
    actual--;
}

void Scanner::startLexema() {
  primero = actual-1;
  return;
}

void Scanner::incrStartLexema() {
  primero++;
}

string Scanner::getLexema() {
  return entrada.substr(primero,actual-primero);
}

Token::Type Scanner::checkReserved(string lexema) {
  std::unordered_map<std::string,Token::Type>::const_iterator it = reservado.find(lexema);
  if (it == reservado.end())
    return Token::ERR;
  else
    return it->second;
}

Instruction::IType Token::tokenToIType(Token::Type tt) {
  Instruction::IType tipoInstruccion;
  switch (tt) {
  case(Token::SKIP): tipoInstruccion = Instruction::ISKIP; break;
  case(Token::POP): tipoInstruccion = Instruction::IPOP; break;
  case(Token::DUP): tipoInstruccion = Instruction::IDUP; break;
  case(Token::SWAP): tipoInstruccion = Instruction::ISWAP; break;
  case(Token::ADD): tipoInstruccion = Instruction::IADD; break;
  case(Token::SUB): tipoInstruccion = Instruction::ISUB; break;
  case(Token::MUL): tipoInstruccion = Instruction::IMUL; break;
  case(Token::DIV): tipoInstruccion = Instruction::IDIV; break;
  case(Token::EQ): tipoInstruccion = Instruction::IEQ; break;
  case(Token::GT): tipoInstruccion = Instruction::IGT; break;
  case(Token::GE): tipoInstruccion = Instruction::IGE; break;
  case(Token::LT): tipoInstruccion = Instruction::ILT; break;
  case(Token::LE): tipoInstruccion = Instruction::ILE; break;
  case(Token::AND): tipoInstruccion = Instruction::IAND; break;
  case(Token::OR): tipoInstruccion = Instruction::IOR; break;
  case(Token::PRINT): tipoInstruccion = Instruction::IPRINT; break;
  case(Token::HALT): tipoInstruccion = Instruction::IHALT; break;
  case(Token::CALL): tipoInstruccion = Instruction::ICALL; break;
  case(Token::MARK): tipoInstruccion = Instruction::IMARK; break;
  case(Token::NEG): tipoInstruccion = Instruction::INEG; break;
  case(Token::NOT): tipoInstruccion = Instruction::INOT; break;
  case(Token::PUSH): tipoInstruccion = Instruction::IPUSH; break;
  case(Token::STORE): tipoInstruccion = Instruction::ISTORE; break;
  case(Token::LOAD): tipoInstruccion = Instruction::ILOAD; break;
  case(Token::STORER): tipoInstruccion = Instruction::ISTORER; break;
  case(Token::LOADR): tipoInstruccion = Instruction::ILOADR; break;
  case(Token::PUSHA): tipoInstruccion = Instruction::IPUSHA; break;
  case(Token::ENTER): tipoInstruccion = Instruction::IENTER; break;
  case(Token::ALLOC): tipoInstruccion = Instruction::IALLOC; break;
  case(Token::RETURN): tipoInstruccion = Instruction::IRETURN; break;
  case(Token::GOTO): tipoInstruccion = Instruction::IGOTO; break;
  case(Token::JMPZ): tipoInstruccion = Instruction::IJMPZ; break;
  case(Token::JMPN): tipoInstruccion = Instruction::IJMPN; break;
  default: cout << "Error: Tipo de palabra clave desconocido" << endl; exit(0);
  }
  return tipoInstruccion;
}

bool Parser::match(Token::Type tipoToken) {
  if (check(tipoToken)) {
    advance();
    return true;
  }
  return false;
}

bool Parser::check(Token::Type tipoToken) {
  if (isAtEnd()) return false;
  return actual->tipo == tipoToken;
}

bool Parser::advance() {
  if (!isAtEnd()) {
    Token* temp = actual;
    if (anterior) delete anterior;
    actual = scanner->nextToken();
    anterior = temp;
    if (check(Token::ERR)) {
      cout << "Error de análisis, carácter no reconocido: " << actual->lexema << endl;
      exit(0);
    }
    return true;
  }
  return false;
} 

bool Parser::isAtEnd() {
  return (actual->tipo == Token::END);
} 

Parser::Parser(Scanner* sc):scanner(sc) {
  anterior = actual = NULL;
  return;
};

SVM* Parser::parse() {
  actual = scanner->nextToken();
  if (check(Token::ERR)) {
      cout << "Error en scanner - carácter inválido" << endl;
      exit(0);
  }
  Instruction* instr = NULL;
  list<Instruction*> sl;
  while (actual->tipo != Token::END) {
    instr = parseInstruction();
    sl.push_back(instr);
  }  
  if (actual->tipo != Token::END) {
    cout << "Esperaba fin-de-input, se encontró " << actual << endl;
  }
  if (actual) delete actual;
  return new SVM(sl);
}

Instruction* Parser::parseInstruction() {
  Instruction* instr = NULL;
  string etiqueta = "";
  string etiquetaSalto;
  Token::Type tipoToken;
  int tipo = 0;
  int n;
  if (match(Token::LABEL)) {
    etiqueta = anterior->lexema;
  }
  if (match(Token::SKIP) || match(Token::POP) || match(Token::DUP) ||
      match(Token::SWAP) || match(Token::ADD) || match(Token::SUB) ||
      match(Token::MUL) || match(Token::DIV) || match(Token::EQ) ||
      match(Token::GT) || match(Token::GE) || match(Token::LT) ||
      match(Token::LE) || match(Token::AND) || match(Token::OR) ||
      match(Token::PRINT) ||match(Token::NEG) || match(Token::NOT)||
      match(Token::HALT) ||match(Token::CALL) || match(Token::MARK)
      )  { 
    tipo = 0;
    tipoToken = anterior->tipo;
  } else if (match(Token::PUSH) || match(Token::STORE) || match(Token::LOAD) ||
             match(Token::ENTER) || match(Token::ALLOC) || 
             match(Token::STORER) || match(Token::LOADR) || match(Token::RETURN)
             ) {
    tipo = 1;
    tipoToken = anterior->tipo;
    if (!match(Token::NUM)) {
      cout << "Esperaba NUM después de " << Token::token_names[tipoToken] << endl;
      exit(0);
    }
    n = stoi(anterior->lexema);
  } else if (match(Token::JMPZ) || match(Token::JMPN) ||
             match(Token::GOTO) || match(Token::PUSHA )) {
    tipo = 2;
    tipoToken = anterior->tipo;
    if (!match(Token::ID)) {
      cout << "Esperaba ID después de " << Token::token_names[tipoToken] << endl;
      exit(0);
    }   
    etiquetaSalto = anterior->lexema;
  } else {
    cout << "Error: no pudo encontrar match para " << actual << endl;  
    exit(0);
  }
  if (!match(Token::EOL)) {
    cout << "Esperaba fin de línea" << endl;
    exit(0);
  }
  if (tipo == 0) {
    instr = new Instruction(etiqueta, Token::tokenToIType(tipoToken));
  } else if (tipo == 1) {
    instr = new Instruction(etiqueta, Token::tokenToIType(tipoToken), n);
  } else { 
    instr = new Instruction(etiqueta, Token::tokenToIType(tipoToken), etiquetaSalto);
  }
  return instr;
}