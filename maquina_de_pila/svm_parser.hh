#ifndef SVM_PARSER
#define SVM_PARSER
#include <string>
#include "svm.hh"
using namespace std;

class Token {
public:
  enum Type { ID=0, LABEL, NUM, EOL, ERR, END, PUSH, EQ, GT, GE, LT, LE, GOTO, SKIP,
  POP, DUP, SWAP, ADD, SUB, MUL, DIV, STORE, LOAD, PRINT, JMPZ, JMPN, AND, OR, NEG, NOT,
  HALT, CALL, MARK, ENTER, ALLOC, STORER, LOADR, PUSHA, LOADA, RETURN };
  static const char* token_names[39]; 
  Type tipo;
  string lexema;
  Token(Type);
  Token(Type, char c);
  Token(Type, const string fuente);
  static Instruction::IType tokenToIType(Token::Type tt);
};

std::ostream& operator << ( std::ostream& outs, const Token* tok );

class Scanner {
public:
  Scanner(string entrada);
  Token* nextToken();
  ~Scanner();
private:
  string entrada;
  int primero, actual;
  int estado;
  unordered_map<string, Token::Type> reservado;
  char nextChar();
  void rollBack();
  void startLexema();
  void incrStartLexema();
  string getLexema();
  Token::Type checkReserved(string lexema);
};

class Parser {
private:
  Scanner* scanner;
  Token *actual, *anterior;
  bool match(Token::Type tipoToken);
  bool check(Token::Type tipoToken);
  bool advance();
  bool isAtEnd();
  Instruction* parseInstruction();
public:
  Parser(Scanner* scanner);
  SVM* parse();
};

#endif