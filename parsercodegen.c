// Written by Nicholas Aristizabal and Conner McKay

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>

#define MAX_IDENT 11
#define MAX_DIGITS 5
#define MAX_SYMS 2

#define ANSI_WARN "\x1b[35m"
#define ANSI_ERR "\x1b[31m"
#define ANSI_HIGHLIGHT "\x1b[33m"
#define ANSI_RESET "\x1b[0m"

#define DEBUG

#ifdef DEBUG
# define debug(fmt, ...) fprintf(stderr, fmt, ##__VA_ARGS__)
#else
# define debug(fmt, ...)
#endif

static void assert(const bool condition, const char message[const]) {
	if (!condition) {
		perror(message);
		exit(1);
	}
}



/// The line and column number in the input file.
typedef struct FilePos {
	unsigned int line;
	unsigned int col;
} FilePos;



/// Print error message and context (such as a bad token that caused the error) with line/col info and terminate compilation.
static void err_with_pos(const char message[const], const char context[const], const FilePos position) {
	fprintf(stderr, ANSI_HIGHLIGHT "%u" ANSI_RESET ":" ANSI_HIGHLIGHT "%u" ANSI_RESET ": " ANSI_ERR "error" ANSI_RESET ": %s: " ANSI_HIGHLIGHT "%s" ANSI_RESET "\n", position.line, position.col, message, context);
	exit(2);
}

/// Print warning message with line/col info but do NOT terminate compilation.
static void warn_with_pos(const char message[const], const char context[const], const FilePos position) {
	fprintf(stderr, ANSI_HIGHLIGHT "%u" ANSI_RESET ":" ANSI_HIGHLIGHT "%u" ANSI_RESET ": " ANSI_WARN "warning" ANSI_RESET ": %s: " ANSI_HIGHLIGHT "%s" ANSI_RESET "\n", position.line, position.col, message, context);
}



typedef struct Vector {
	unsigned int len;
	unsigned int capacity;
	void* arr;
	// `elem_size` is just used for debugging to make sure we don't get in an inconsistent state.
	unsigned int elem_size;
} Vector;

static Vector new_vector(const unsigned int capacity, const size_t size) {
	const Vector list = {
		.len = 0,
		.capacity = capacity,
		.arr = malloc(capacity*size),
		.elem_size = size,
	};

	assert(list.arr != NULL, "new_vector malloc failed");

	return list;
}

static void vector_push(Vector *const restrict list, const void* const restrict item, const size_t size) {
	assert(list->elem_size == size, "a developer pushed the wrong thing to this vector");

	if (list->len >= list->capacity) {
		list->capacity *= 2;
		list->arr = realloc(list->arr, list->capacity*size);

		assert(list->arr != NULL, "push_vector realloc failed");
	}

	memcpy((char*)list->arr + list->len*size, item, size);
	list->len += 1;
}

static void* _vector_get(Vector *const restrict list, const unsigned int index, const size_t size) {
	assert(list->elem_size == size, "Bad type provided to vector_get");
	assert(index < list->len, "Out of bounds vector access");

	return ((char*)list->arr) + (size * index);
}
#define vector_get(vector, index, type) ((type*)_vector_get(&vector, index, sizeof(type)))



/// All valid token numbers. Named like the assignment says but without the trailing "sym".
typedef enum TokenType {
	TK_ODD = 1,
	TK_IDENT,
	TK_NUMBER,
	TK_PLUS,
	TK_MINUS,
	TK_MULT,
	TK_SLASH,
	TK_XOR,
	TK_EQL,
	TK_NEQ,
	TK_LESS,
	TK_LEQ,
	TK_GTR,
	TK_GEQ,
	TK_LPARENT,
	TK_RPARENT,
	TK_COMMA,
	TK_SEMICOLON,
	TK_PERIOD,
	TK_BECOME,
	TK_BEGIN,
	TK_END,
	TK_IF,
	TK_THEN,
	TK_WHILE,
	TK_DO,
	TK_CALL,
	TK_CONST,
	TK_VAR,
	TK_PROC,
	TK_WRITE,
	TK_READ,
	TK_ELSE,
} TokenType;



struct KeywordMap {
	TokenType token_type;
	char* string;
};

const struct KeywordMap KEYWORDS[] = {
	{TK_BEGIN, "begin"},
	{TK_END, "end"},
	{TK_IF, "if"},
	{TK_THEN, "then"},
	{TK_XOR, "xor"},
	{TK_ELSE, "else"},
	{TK_WHILE, "while"},
	{TK_DO, "do"},
	{TK_CALL, "call"},
	{TK_CONST, "const"},
	{TK_VAR, "var"},
	{TK_PROC, "procedure"},
	{TK_WRITE, "write"},
	{TK_READ, "read"},
	{TK_ODD, "odd"},
};

const struct KeywordMap SPECIAL_TOKENS[] = {
	{TK_SEMICOLON, ";"},
	{TK_BECOME, ":="},
	{TK_PLUS, "+"},
	{TK_MINUS, "-"},
	{TK_MULT, "*"},
	{TK_SLASH, "/"},
	{TK_EQL, "="},
	{TK_GEQ, ">="},
	{TK_LEQ, "<="},
	{TK_LESS, "<"},
	{TK_GTR, ">"},
	{TK_NEQ, "<>"},
	{TK_LPARENT, "("},
	{TK_RPARENT, ")"},
	{TK_COMMA, ","},
	{TK_PERIOD, "."},
};

const char SPECIAL_CHARS[] = {'<', '>', ':', ';', ',', '.', '=', '+', '-', '*', '/', '(', ')'};



/// Null-terminated array of chars for use in a symbol table.
typedef struct Symbol {
	unsigned int kind;
	char string[MAX_IDENT + 1];
	int value;
	unsigned int level;
	unsigned int  address;
	bool mark;

} Symbol;



union TokenData {
	unsigned int symbol_index;
	int int_literal;
};

/// A token, with associated data (such as symbol table address or literal value) and file position.
typedef struct Token {
	TokenType type;
	union TokenData data;
	FilePos pos;
} Token;

/// Print a token out as it would appear in source code. Additionally, it colors identifiers for clarity.
static void token_tostring(const Token t, const Vector *const symbol_table, FILE *const file) {
	if (t.type == TK_IDENT) {
		const Symbol *const symbols = (Symbol*) symbol_table->arr;
		fprintf(file, ANSI_HIGHLIGHT "%s" ANSI_RESET, symbols[t.data.symbol_index].string);
	}
	else if (t.type == TK_NUMBER) {
		fprintf(file, "%d", t.data.int_literal);
	}

	for (int i=0; i<(int)(sizeof(KEYWORDS)/sizeof(*KEYWORDS)); i++) {
		if (t.type == KEYWORDS[i].token_type) {
			fprintf(file, "%s", KEYWORDS[i].string);
		}
	}
	for (int i=0; i<(int)(sizeof(SPECIAL_TOKENS)/sizeof(*SPECIAL_TOKENS)); i++) {
		if (t.type == SPECIAL_TOKENS[i].token_type) {
			fprintf(file, "%s", SPECIAL_TOKENS[i].string);
		}
	}

	switch (t.type) {
		case TK_SEMICOLON:
		case TK_BEGIN:
		case TK_THEN:
		case TK_DO:
			fprintf(file, "\n");
			break;
		default:
			fprintf(file, " ");
	}
}



typedef enum CharType {
	CH_INVALID,
	CH_WHITESPACE,
	CH_ALPHA,
	CH_NUMBER,
	CH_SPECIAL,
} CharType;

CharType char_type(const char c) {
	if (isalpha(c)) return CH_ALPHA;
	else if (isdigit(c)) return CH_NUMBER;
	else if (isspace(c)) return CH_WHITESPACE;

	for (int i=0; i<(int)(sizeof(SPECIAL_CHARS)/sizeof(*SPECIAL_CHARS)); i++) {
		if (c == SPECIAL_CHARS[i]) return CH_SPECIAL;
	}

	return CH_INVALID;
}



static void tokenize(Vector *const restrict token_table, Vector *const restrict symbol_table, FILE *const restrict input_file) {
	FilePos current_pos = {.line = 1, .col = 0};

	while (true) {
		const char start_char = fgetc(input_file);

		switch (char_type(start_char)) {
			case CH_WHITESPACE: {
				// This is the only possible time we can have a newline, since it's a type of whitespace.
				if (start_char == '\n') {
					current_pos.col = 0;
					current_pos.line++;
				}
				else {
					current_pos.col++;
				}
				break;
			}

			case CH_ALPHA: {
				const FilePos position = current_pos;

				int ident_buf_idx = 1;
				char ident_buf[MAX_IDENT + 1] = {start_char, 0};

				current_pos.col++;

				while(1) {
					const char c = fgetc(input_file);
					const CharType type = char_type(c);

					// Identifier-like things can only be alphanumeric
					if (type != CH_ALPHA && type != CH_NUMBER) {
						ungetc(c, input_file);
						break;
					}
					// Only append chars to the buffer if we have room, silently truncate otherwise.
					else if (ident_buf_idx < MAX_IDENT) {
						ident_buf[ident_buf_idx] = c;
						ident_buf_idx++;
					}
					// Warn at the moment we start truncating the identifier.
					else if (ident_buf_idx == MAX_IDENT) {
						ident_buf_idx++;
						warn_with_pos("Identifier truncated", ident_buf, position);
					}
				}

				debug("Ident-like:\t%s\n", ident_buf);

				// Create the token
				Token token = {.type = TK_IDENT, .pos = position};

				// Check if this is actually a keyword.
				for (int i=0; i<(int)(sizeof(KEYWORDS)/sizeof(*KEYWORDS)); i++) {
					if (strcmp(ident_buf, KEYWORDS[i].string) == 0) {
						token.type = KEYWORDS[i].token_type;
						break;
					}
				}

				// If not a keyword, put the identifer in the symbol table.
				if (token.type == TK_IDENT) {
					// First search for symbol in table
					const Symbol* const symbols = (Symbol*)symbol_table->arr;

					bool found = false;

					for (unsigned int i=symbol_table->len;; i--) {
						if (strcmp(symbols[i].string, ident_buf) == 0) {
							found = true;
							token.data.symbol_index = i;
							break;
						}

						if (i == 0) break;
					}

					// Create new symbol table entry and point the token to that.
					if (!found) {
						token.data.symbol_index = symbol_table->len;

						Symbol sym = {0};
						strncpy(sym.string, ident_buf, MAX_IDENT);

						vector_push(symbol_table, &sym, sizeof(Symbol));
					}
				}

				// Add token to the token list.
				vector_push(token_table, &token, sizeof(Token));
				break;
			}

			case CH_NUMBER: {
				const FilePos position = current_pos;

				int num_buf_idx = 1;
				char num_buf[MAX_DIGITS + 1] = {start_char, 0};

				current_pos.col++;

				while(1) {
					const char c = fgetc(input_file);
					const CharType type = char_type(c);

					if (type == CH_ALPHA) {
						err_with_pos("Identifiers must not begin with digits", num_buf, position);
					}
					else if (type == CH_NUMBER && num_buf_idx < MAX_DIGITS) {
						num_buf[num_buf_idx] = c;
						num_buf_idx++;
					}
					// Warn at the moment we start truncating digits.
					else if (type == CH_NUMBER && num_buf_idx == MAX_DIGITS) {
						num_buf_idx++;
						warn_with_pos("Number truncated", num_buf, position);
					}
					// This token is over.
					else {
						ungetc(c, input_file);
						break;
					}
				}

				debug("Number: \t%s\n", num_buf);

				Token token = {.type = TK_NUMBER, .pos = position};
				sscanf(num_buf, "%d", &token.data.int_literal);

				vector_push(token_table, &token, sizeof(Token));
				break;
			}

			case CH_SPECIAL: {
				const FilePos position = current_pos;

				int special_buf_idx = 1;
				char special_buf[MAX_SYMS + 1] = {start_char, 0};

				current_pos.col++;

				while(1) {
					const char c = fgetc(input_file);
					const CharType type = char_type(c);

					if (type == CH_SPECIAL && special_buf_idx < MAX_SYMS) {
						special_buf[special_buf_idx] = c;
						special_buf_idx++;
					}
					else {
						ungetc(c, input_file);
						break;
					}
				}

				// Check for comment start.
				if (special_buf[0] == '/' && special_buf[1] == '*') {
					char c1 = '\0';
					char c2 = '\0';

					debug("Comment start\t%u:%u\n", current_pos.line, current_pos.col);

					while(1) {
						c1 = c2;
						c2 = fgetc(input_file);

						if (c2 == '\n') {
							current_pos.line++;
							current_pos.col = 0;
						}
						else {
							current_pos.col++;
						}

						if (c1 == '*' && c2 == '/') {
							break;
						}
					}

					debug("Comment end\t%u:%u\n", current_pos.line, current_pos.col);
					break;
				}

				Token token = {.type = 0, .pos = position};

				// Check against all lengths, so that ");" and similar cases are handled correctly.
				for (int len=special_buf_idx; len>0; len--) {
					bool found_flag = false;

					for (int i=0; i<(int)(sizeof(SPECIAL_TOKENS)/sizeof(*SPECIAL_TOKENS)); i++) {
						if (strcmp(special_buf, SPECIAL_TOKENS[i].string) == 0) {
							token.type = SPECIAL_TOKENS[i].token_type;
							debug("Special:\t%s\n", special_buf);

							found_flag = true;
							break;
						}
					}

					if (found_flag) {
						break;
					}

					special_buf_idx--;
					ungetc(special_buf[special_buf_idx], input_file);
					special_buf[special_buf_idx] = '\0';
				}

				// Didn't match any special token.
				if (token.type == 0) {
					err_with_pos("Invalid token", special_buf, position);
				}

				vector_push(token_table, &token, sizeof(Token));
				break;
			}

			default: {
				// fgetc can return an EOF. This cast should deal with signed OR unsigned char type because we cast the integer return to a char.
				if (start_char == (char)EOF) {
					return;
				}

				char buf[10];
				snprintf(buf, sizeof(buf)/sizeof(*buf), "%d/%c", start_char, start_char);

				err_with_pos("Invalid symbol", buf, current_pos);
			}
		}
	}
}



typedef int Word;

// Instruction type.
typedef struct Inst {
	Word op;
	Word l;
	Word m;
} Inst;

// Opcode table.
enum Opcode {
	LIT = 1,
	OPR,
	LOD,
	STO,
	CAL,
	INC,
	JMP,
	JPC,
	SYS,
};

// ALU operations for the `OPR` instruction.
enum OprCode {
	RTN = 0,
	ADD,
	SUB,
	MUL,
	DIV,
	EQL,
	NEQ,
	LSS,
	LEQ,
	GTR,
	GEQ,
};


/* ----------------------------------------------------------------------------------------

APPENDIX B:

program ::= block "." .

block ::= const-declaration var-declaration statement.

constdeclaration ::= [ “const” ident "=" number {"," ident "=" number} “;"].

var-declaration ::= [ "var" ident {"," ident} “;"].

statement ::= [ ident ":=" expression
| "begin" statement { ";" statement } "end"
| "if" condition "then" statement
| "xor" condition "then" statement “else" statement
| "while" condition "do" statement
| "read" ident
| "write" expression
| empty ] .

condition ::= "odd" expression
| expression rel-op expression.

rel-op ::= "="|“=<"|"<"|"=>"|">"|"<>“.

expression ::= term { ("+"|"-") term}.

term ::= factor {("*"|"/") factor}.

factor ::= ident | number | "(" expression ")“.

number ::= digit {digit}.

ident ::= letter {letter | digit}.

digit ;;= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9“.

letter ::= "a" | "b" | ... | "y" | "z" | "A" | "B" | ... |"Y" | "Z".


Pay attention to this:
** Do not print the symbol table. That will be replaced by inserting:
**JMP 0 3 as the first instruction in the generated code
**Replace in HW2 skipsym by oddsym.
**Include in HW1 OPR: 11 ODD pas[sp] <— pas[sp] mod 2


Appendix C:

Error messages for the tiny PL/0 Parser:
	• program must end with period
	• const, var, and read keywords must be followed by identiNier
	• symbol name has already been declared
	• constants must be assigned with =
	• constants must be assigned an integer value
	• constant and variable declarations must be followed by a semicolon
	• undeclared identiNier
	• only variable values may be altered
	• assignment statements must use :=
	• begin must be followed by end
	• if must be followed by then
	• while must be followed by do
	• condition must contain comparison operator
	• right parenthesis must follow left parenthesis
	• arithmetic equations must contain operands, parentheses, numbers, or
	symbols

  ---------------------------------------------------------------------------------------- */
void expression(Token t, Vector *token_table, Vector *symbol_table, Vector *code);
void condition(Token t, Vector *token_table, Vector *symbol_table, Vector *code);
void const_declaration(Token t, Vector *token_table, Vector *symbol_table, Vector *identifier_table);
int var_declaration(Token t, Vector *token_table, Vector *symbol_table, Vector *identifier_table);
void block(Token t, Vector *token_table, Vector *symbol_table, Vector *identifier_table, Vector *code);
void statement(Token t, Vector *token_table, Vector *symbol_table, Vector *code);
void term(Token t, Vector *token_table, Vector *symbol_table, Vector *code);
void factor(Token t, Vector *token_table, Vector *symbol_table, Vector *code);

unsigned int lexical_level = 0;
unsigned int token_table_index;
unsigned int current_instruction = 0;
Vector *ident_table;

Token get_next_token(Vector *token_table){
	return *vector_get(*token_table, ++token_table_index, Token);
}

int symbol_table_check(Symbol symbol, Vector *symbol_table){

	/*
	SYMBOLTABLECHECK (string)
		linear search through symbol table looking at name
		return index if found, -1 if not
	*/

	const Symbol *const symbols = (Symbol*)symbol_table->arr;
	for(int i= symbol_table->len-1; i>=0; i--){
		if (strcmp(symbols[i].string, symbol.string) == 0) {
			return i;
		}
	}

	return -1;
}

/// Returns a pointer to the `Symbol` in the `symbol_table` matching the name `ident` if it is not marked, or NULL otherwise.
Symbol* symbol_table_get(Vector *symbol_table, const unsigned int ident_index) {
	const Symbol *const ident = vector_get(*ident_table, ident_index, Symbol);

	if (symbol_table->len != 0) {
		for (long i=symbol_table->len - 1;; i--) {
			Symbol *const sym = vector_get(*symbol_table, i, Symbol);
			if (sym->mark == 0 && strncmp(ident->string, sym->string, MAX_IDENT) == 0) {
				return sym;
			}

			if (i == 0) {
				break;
			}
		}
	}

	return NULL;
}

void program(Token t, Vector *token_table, Vector *symbol_table, Vector *identifier_table, Vector *code){

	/*
	PROGRAM
		BLOCK
		if token != periodsym
			error
		emit HALT
	*/

	block(t, token_table, symbol_table, identifier_table, code);
	t = get_next_token(token_table);
	if(t.type != TK_PERIOD)
		printf("Missing period at end of program\n");
	
	// emit HALT
	vector_push(code, &(Inst){SYS, 0, 3}, sizeof(Inst));
}

void block(Token t, Vector *token_table, Vector *symbol_table, Vector *identifier_table, Vector *code){

	/*
	BLOCK
		CONST-DECLARATION
		numVars = VAR-DECLARATION
		emit INC (M = 3 + numVars)
		STATEMENT
	*/

	printf("BLOCK\n");

	const_declaration(t, token_table, symbol_table, identifier_table);

	t = *vector_get(*token_table, token_table_index, Token);
	int numVars = var_declaration(t, token_table, symbol_table, identifier_table);

	// emit INC 3 + numVars
	vector_push(code, &(Inst){INC, 0, 3 + numVars}, sizeof(Inst));

	statement(*vector_get(*token_table, token_table_index, Token), token_table, symbol_table, code);
}

void const_declaration(Token t, Vector *token_table, Vector *symbol_table, Vector *identifier_table){

	/*
	CONST-DECLARATION
		if token == const
			do
				get next token
				if token != identsym
					error
				if SYMBOLTABLECHECK (token) != -1
					error
				save ident name
				get next token
				if token != eqlsym
					error
				get next token
				if token != numbersym
					error
				add to symbol table (kind 1, saved name, number, 0, 0)
				get next token
			while token == commasym
			if token != semicolonsym
				error
			get next token
	*/

	printf("\nCONST_DECLARATION\n\n");
	const Symbol *const symbols = (Symbol*)symbol_table->arr;
	
	Token next_token;

	if(t.type == TK_CONST){

		do{
			// Identifier
			next_token = get_next_token(token_table);
			if(next_token.type != TK_IDENT){
				printf("ERROR, invalid token type in constant declaration Line: %d Col: %d\n", next_token.pos.line, next_token.pos.col);
				return;
			}
			if(symbol_table_check(symbols[next_token.data.symbol_index], symbol_table) != -1){
				printf("ERROR, identifier already defined in constant declaration Line: %d Col: %d\n", next_token.pos.line, next_token.pos.col);
				return;
			}

			int token_index = next_token.data.symbol_index;

			// :=
			next_token = get_next_token(token_table);
			if(next_token.type != TK_BECOME){
				printf("ERROR, expected ':=' in constant declaration Line: %d Col: %d\n", next_token.pos.line, next_token.pos.col);
				return;
			}

			// number
			next_token = get_next_token(token_table);
			if(next_token.type != TK_NUMBER){
				printf("ERROR, expected a number in constant declaration Line: %d Col: %d\n", next_token.pos.line, next_token.pos.col);
				return;
			}
		
			// Add symbol to symbol table
			// Copy symbol from identifier table (comes with its string) and add its other propertiess
			Symbol *s = vector_get(*identifier_table, token_index, Symbol);
			s->kind = 1;
			s->value = next_token.data.int_literal;
			s->level = lexical_level;
			s->mark = 0;

			// Push symbol to symbol table
			vector_push(symbol_table, s, sizeof(Symbol));

			printf("Symbol from table:\n Kind= %d Name= %s Value= %d Level= %d Mark= %d\n", symbols[token_index].kind, symbols[token_index].string, symbols[token_index].value, symbols[token_index].level, symbols[token_index].mark);

			// , or ;
			next_token = get_next_token(token_table);
		}while(next_token.type == TK_COMMA); 

		if(next_token.type != TK_SEMICOLON){
			printf("ERROR, expected semicolon\n");
		}
		token_table_index++;
	}
}

int var_declaration(Token t, Vector *token_table, Vector *symbol_table, Vector *identifier_table){

	/*
	VAR-DECLARATION – returns number of variables
		numVars = 0
		if token == varsym
			do
				numVars++
				get next token
				if token != identsym
					error
				if SYMBOLTABLECHECK (token) != -1
					error
				add to symbol table (kind 2, ident, 0, 0, var# + 2)
				get next token
			while token == commasym
			if token != semicolonsym
				error
			get next token
		return numVars
	*/

	Token next_token;
	printf("\nVAR_DECLARATION\n\n");
	const Symbol *const symbols = (Symbol*)symbol_table->arr;
	int numVars = 0;

	if(t.type == TK_VAR){
		do{
			numVars++;
			// Identifier
			next_token = get_next_token(token_table);
			if(next_token.type == TK_SEMICOLON) {
				token_table_index++;
				break;
			}
			else if(next_token.type == TK_COMMA) {
				continue;
			}
			else if(next_token.type != TK_IDENT){
				err_with_pos("Expected identifier or \";\"", "", next_token.pos);
			}
			if(symbol_table_check(symbols[next_token.data.symbol_index], symbol_table) != -1){
				printf("ERROR, identifier already defined in var declaration Line: %d Col: %d\n", next_token.pos.line, next_token.pos.col);
			}

			// Add symbol to symbol table
			// Copy symbol from identifier table (comes with its string) and add its other propertiess
			int token_index = next_token.data.symbol_index; 
			Symbol *s = vector_get(*identifier_table, token_index, Symbol);
			s->kind = 2;
			s->value = 0;
			s->level = lexical_level;
			s->address = numVars + 2;
			s->mark = 0;

			// Push symbol to symbol table
			vector_push(symbol_table, s, sizeof(Symbol));

			// , or ;
			next_token = get_next_token(token_table);

			printf("Symbol from table:\n Kind= %d Name= %s Value= %d Adress= %d Level= %d Mark= %d\n", symbols[token_index].kind, symbols[token_index].string, symbols[token_index].value, symbols[token_index].address, symbols[token_index].level, symbols[token_index].mark);
		}while(next_token.type == TK_COMMA);

		if(next_token.type != TK_SEMICOLON){
			printf("ERROR, expected semicolon\n");
		}
		token_table_index++;
	}
	return numVars;
}

void statement(Token t, Vector *token_table, Vector *symbol_table, Vector *code){

	/*
	STATEMENT
		if token == identsym
			symIdx = SYMBOLTABLECHECK (token)
			if symIdx == -1
				error
			if table[symIdx].kind != 2 (not a var)
				error
			get next token
			if token != becomessym
				error
			get next token
			EXPRESSION
			emit STO (M = table[symIdx].addr)
			return
		if token == beginsym
			do
				get next token
				STATEMENT
			while token == semicolonsym
			if token != endsym
				error
			get next token
			return
		if token == ifsym
			get next token
			CONDITION
			jpcIdx = current code index
			emit JPC
			if token != thensym
				error
			get next token
			STATEMENT
			code[jpcIdx].M = current code index
			return
		if token == xorsym
			do
				get next token
				CONDITION
				jpcIdx = current code index
				emit JPC
				if token != thensym
					error
				get next token
				STATEMENT
				if token != semicolonsym
					error
				else
					get next token
				if token != elsesym
					error
				get next token
				STATEMENT
			code[jpcIdx].M = current code index
			
		if token == whilesym
			get next token
			loopIdx = current code index
			CONDITION
			if token != dosym
				error
			get next token
			jpcIdx = current code index
			emit JPC
			STATEMENT
			emit JMP (M = loopIdx)
			code[jpcIdx].M = current code index
			return
		if token == readsym
			get next token
			if token != identsym
				error
			symIdx = SYMBOLTABLECHECK (token)
			if symIdx == -1
				error
			if table[symIdx].kind != 2 (not a var)
				error
			get next token
			emit READ
			emit STO (M = table[symIdx].addr)
			return
		if token == writesym
			get next token
			EXPRESSION
			emit WRITE
			return
	*/

	printf("STATEMENT\n");
	printf("T TYPE IS %d\n", t.type);

	switch (t.type) {
		case TK_IDENT: {
			printf("--IDENT--\n");
			int symbol_index = t.data.symbol_index;
			debug("%d %d\n", symbol_index, symbol_table->len);
			const Symbol *const symbol = symbol_table_get(symbol_table, symbol_index);

			if (symbol == NULL) {
				err_with_pos("No such variable", "", t.pos);
			}

			debug("Symbol kind of %s is %d\n", symbol->string, symbol->kind);

			if (symbol->kind != 2) {
				err_with_pos("Expected identifier", symbol->string, t.pos);
			}

			Token becomeToken = get_next_token(token_table);
			if (becomeToken.type != TK_BECOME) {
				err_with_pos("Expected \":=\" after identifier", "", becomeToken.pos);
			}

			expression(*vector_get(*token_table, ++token_table_index, Token), token_table, symbol_table, code);
			// emit STO sym.addr
			vector_push(code, &(Inst){STO, 0, symbol->address}, sizeof(Inst));

			break;
		}
		
		case TK_BEGIN: {
			printf("--BEGIN--\n");
			do {
				debug("---- Start ----\n");
				t = get_next_token(token_table);
				statement(t, token_table, symbol_table, code);
				t = *vector_get(*token_table, token_table_index, Token);
				debug("---- End ----\n");

			}while(t.type == TK_SEMICOLON);

			if (t.type == TK_END) {
				break;
			}
			else if (t.type != TK_SEMICOLON) {
				err_with_pos("Expected \";\" or \"end\"", "", t.pos);
			}
		
			break;
		}
		case TK_IF: {
			printf("--IF--\n");
			condition(get_next_token(token_table), token_table, symbol_table, code);

			// emit JPC
			vector_push(code, &(Inst){JPC, 0, 0}, sizeof(Inst));
			Inst *const jmp_inst = vector_get(*code, code->len - 1, Inst);

			t = *vector_get(*token_table, token_table_index, Token);
			if (t.type != TK_THEN) {
				err_with_pos("Expected \"then\"", "", t.pos);
			}

			statement(get_next_token(token_table), token_table, symbol_table, code);

			jmp_inst->m = 3*(code->len - 1);

			break;
		}

		case TK_XOR: {
			printf("--XOR--\n");
			t = get_next_token(token_table);
			printf("BEFORE XOR CONDITION T IS %d\n", t.type);
			condition(t, token_table, symbol_table, code);

			// emit JPC
			vector_push(code, &(Inst){JPC, 0, 0}, sizeof(Inst));
			Inst *const jmp_inst = vector_get(*code, code->len - 1, Inst);

			t = *vector_get(*token_table, token_table_index, Token);
			if (t.type != TK_THEN) {
				err_with_pos("Expected \"then\"", "", t.pos);
			}

			statement(get_next_token(token_table), token_table, symbol_table, code);

			Token else_token = get_next_token(token_table);
			if (else_token.type != TK_ELSE) {
				err_with_pos("Expected \"else\"", "", else_token.pos);
			}

			statement(get_next_token(token_table), token_table, symbol_table, code);

			jmp_inst->m = 3*(code->len - 1);

			break;
		}

		case TK_WHILE: {
			printf("--WHILE--\n");
			const unsigned int loop_head_idx = code->len;

			condition(get_next_token(token_table), token_table, symbol_table, code);

			Token do_token = get_next_token(token_table);
			if (do_token.type != TK_THEN) {
				err_with_pos("Expected \"do\"", "", do_token.pos);
			}

			// emit JPC
			vector_push(code, &(Inst){JPC, 0, 0}, sizeof(Inst));
			Inst *const jpc_inst = vector_get(*code, code->len - 1, Inst);

			statement(get_next_token(token_table), token_table, symbol_table, code);

			// emit JMP
			vector_push(code, &(Inst){JMP, 0, loop_head_idx}, sizeof(Inst));

			jpc_inst->m = 3*(code->len - 1);

			break;
		}

		case TK_READ: {
			printf("--READ--\n");
			const Token ident_token = get_next_token(token_table);
			if (ident_token.type != TK_IDENT) {
				err_with_pos("Expected identifier", "", ident_token.pos);
			}

			int symbol_index = ident_token.data.symbol_index;
			const Symbol *const symbol = vector_get(*symbol_table, symbol_index, Symbol);
			if (symbol->kind != 2) {
				err_with_pos("Expected variable", symbol->string, t.pos);
			}

			t = get_next_token(token_table);
			// emit READ
			vector_push(code, &(Inst){SYS, 0, 2}, sizeof(Inst));
			// emit STO symbol.address
			vector_push(code, &(Inst){STO, 0, symbol->address}, sizeof(Inst));

			break;
		}

		case TK_WRITE: {
			printf("--WRITE--\n");
			t = get_next_token(token_table);
			expression(t, token_table, symbol_table, code);
			// emit WRITE
			vector_push(code, &(Inst){SYS, 0, 1}, sizeof(Inst));

			break;
		}

		case TK_END: {
			break;
		}

		default:
			err_with_pos("Unhandled token", "", t.pos);
	}
}

void condition(Token t, Vector *token_table, Vector *symbol_table, Vector *code){

	/*
	CONDITION
		if token == oddsym
			get next token
			EXPRESSION
			emit ODD
		else
			EXPRESSION
			if token == eqlsym 
				get next token
				EXPRESSION
				emit EQL
			else if token == neqsym
				get next token
				EXPRESSION
				emit NEQ
		else if token == lessym
				get next token
				EXPRESSION
				emit LSS
			else if token == leqsym
				get next token
				EXPRESSION
				emit LEQ
			else if token == gtrsym
				get next token
				EXPRESSION
				emit GTR
			else if token == geqsym
				get next token
				EXPRESSION
				emit GEQ
			else
				error
	*/

	printf("CONDITION\n");

	if(t.type == TK_ODD){
		t = get_next_token(token_table);
		expression(t, token_table, symbol_table, code);
		// emit ODD
		//vector_push(code, &(Inst){OPR, 0, });
		err_with_pos("Odd operator not implemented", "", t.pos);

	}else{
		expression(t, token_table, symbol_table, code);
		t = *vector_get(*token_table, token_table_index, Token);

		if(t.type == TK_EQL){
			t = get_next_token(token_table);
			expression(t, token_table, symbol_table, code);
			// emit EQL
			vector_push(code, &(Inst){OPR, 0, EQL}, sizeof(Inst));

		}else if(t.type == TK_NEQ){
			t = get_next_token(token_table);
			expression(t, token_table, symbol_table, code);
			// emit NEQ
			vector_push(code, &(Inst){OPR, 0, NEQ}, sizeof(Inst));

		}else if(t.type == TK_LESS){
			t = get_next_token(token_table);
			expression(t, token_table, symbol_table, code);
			// emit LSS
			vector_push(code, &(Inst){OPR, 0, LSS}, sizeof(Inst));

		}else if(t.type == TK_LEQ){
			t = get_next_token(token_table);
			expression(t, token_table, symbol_table, code);
			// emit LEQ
			vector_push(code, &(Inst){OPR, 0, LEQ}, sizeof(Inst));

		}else if(t.type == TK_GTR){
			t = get_next_token(token_table);
			expression(t, token_table, symbol_table, code);
			// emit GTR
			vector_push(code, &(Inst){OPR, 0, GTR}, sizeof(Inst));

		}else if(t.type == TK_GEQ){
			t = get_next_token(token_table);
			expression(t, token_table, symbol_table, code);
			// emit GEQ
			vector_push(code, &(Inst){OPR, 0, GEQ}, sizeof(Inst));

		}else{
			err_with_pos("Invalid operator", "", t.pos);
		}
	}
	
}

void expression(Token t, Vector *token_table, Vector *symbol_table, Vector *code){

	/*
	EXPRESSION
		while token == plussym || token == minussym
				if token == plussym
				get next token
				TERM
				emit ADD
			else
				get next token
				TERM
				emit SUB
	*/

	printf("EXPRESSION\n");

	term(t, token_table, symbol_table, code);
	t = *vector_get(*token_table, token_table_index, Token);

	while(t.type == TK_PLUS || t.type == TK_MINUS){
		if(t.type == TK_PLUS){
			t = get_next_token(token_table);
			term(t, token_table, symbol_table, code);
			// emit ADD
			vector_push(code, &(Inst){OPR, 0, ADD}, sizeof(Inst));

		}else{
			t = get_next_token(token_table);
			term(t, token_table, symbol_table, code);
			// emit SUB
			vector_push(code, &(Inst){OPR, 0, SUB}, sizeof(Inst));
		}
	}

}

void term(Token t, Vector *token_table, Vector *symbol_table, Vector *code){

	/*
	TERM
		FACTOR
		while token == multsym || token == slashsym
				if token == multsym
					get next token
					FACTOR
					emit MUL
				else
					get next token
					FACTOR
					emit DIV
	*/
	printf("TERM\n");

	factor(t, token_table, symbol_table, code);
	t = *vector_get(*token_table, token_table_index, Token);

	while(t.type == TK_MULT || t.type == TK_SLASH){
		if(t.type == TK_MULT){
			t = get_next_token(token_table);
			factor(t, token_table, symbol_table, code);
			// emit MUL
			vector_push(code, &(Inst){OPR, 0, MUL}, sizeof(Inst));

		}else{
			t = get_next_token(token_table);
			factor(t, token_table, symbol_table, code);
			// emit DIV
			vector_push(code, &(Inst){OPR, 0, DIV}, sizeof(Inst));
		}
	}
}

void factor(Token t, Vector *token_table, Vector *symbol_table, Vector *code){

	/*
	FACTOR
		if token == identsym
				symIdx = SYMBOLTABLECHECK (token)
				if symIdx == -1
					error
				if table[symIdx].kind == 1 (const)
					emit LIT (M = table[symIdx].Value)
				else (var)
					emit LOD (M = table[symIdx].addr)
				get next token
		else if token == numbersym
				emit LIT
				get next token
		else if token == lparentsym
				get next token
				EXPRESSION
				if token != rparentsym
				error
				get next token
		else
				error
	*/

	printf("FACTOR\n");

	const Symbol *const symbols = (Symbol*)symbol_table->arr;
	Token next_token;

	if(t.type == TK_IDENT){
		int symInx = t.data.symbol_index;
		if(symInx == -1){
			printf("Unknown identifier\n");
		}
		if(symbols[symInx].kind == 1){
			// LIT 0 VALUE
			vector_push(code, &(Inst){LIT, 0, symbols[symInx].value}, sizeof(Inst));
			
		}else if(symbols[symInx].kind == 2){
			// LOD 0 ADDR
			vector_push(code, &(Inst){LOD, 0, symbols[symInx].address}, sizeof(Inst));

		}
		next_token = get_next_token(token_table);

	}else if(t.type == TK_NUMBER){
		// emit LIT
		vector_push(code, &(Inst){LIT, 0, t.data.int_literal}, sizeof(Inst));
		next_token = get_next_token(token_table);

	}else if(t.type == TK_LPARENT){
		next_token = get_next_token(token_table);
		expression(next_token, token_table, symbol_table, code);
		if(next_token.type != TK_RPARENT){
			printf("Inbalanced parenthesis\n");
			next_token = get_next_token(token_table);
		}

	}else{
		printf("Error\n");
	}
}

/*
		CURRENT PROBLEMS

		- Symbols need to be added during parsing not during tokenizing. This gives a false positive error
			in var/const declaration saying that the variable is already in symbol table. Maybe reset symbol table
			after tokenizing?

		TO DO

		- FACTOR testing
		- TERM testing
		- EXPRESSION testing
		- CONDITION testing
		- STATEMENT
*/

int main(const int argc, const char *const *const argv) {
	assert(argc == 2, "Expected exactly 1 argument: exe <INPUT>");

	FILE *const input_file = fopen(argv[1], "r");
	assert(input_file != NULL, "Cannot open file");


	Vector token_table = new_vector(256, sizeof(Token));
	Vector identifier_table = new_vector(256, sizeof(Symbol));

	tokenize(&token_table, &identifier_table, input_file);


	// Print token stream.
	printf(ANSI_WARN "\nTokens:\n" ANSI_RESET);
	for (unsigned int i=0; i<token_table.len; i++) {
		const Token t = *vector_get(token_table, i, Token);
		printf("%d %u (%u:%u)\n", t.type, t.data.symbol_index, t.pos.line, t.pos.col);
	}

	// Print symbol table.
	printf(ANSI_WARN "\nIdentifier table:\n" ANSI_RESET);
	for (unsigned int i=0; i<identifier_table.len; i++) {
		printf("%s\n", vector_get(identifier_table, i, Symbol)->string);
	}

	// Print source code reconstructed from token stream.
	printf(ANSI_WARN "\nSource code reconstruction:\n" ANSI_RESET);
	for (unsigned int i=0; i<token_table.len; i++) {
		token_tostring(*vector_get(token_table, i, Token), &identifier_table, stdout);
	}

	printf("\n\n\n");

	fclose(input_file);



	token_table_index = 0;
	ident_table = &identifier_table;
	Vector symbol_table = new_vector(256, sizeof(Symbol));
	Vector code = new_vector(256, sizeof(Inst));

	// The first instruction must be this one according to assignment.
	vector_push(&code, &(Inst){JMP, 0, 3}, sizeof(Inst));


	Token *t = vector_get(token_table, token_table_index, Token);
	program(*t, &token_table, &symbol_table, &identifier_table, &code);


	FILE *const output_file = fopen("output", "w");
	assert(output_file != NULL, "Cannot open output file");

	// Output code to file.
	for (unsigned int i=0; i<code.len; i++) {\
		const Inst inst = *vector_get(code, i, Inst);
		fprintf(output_file, "%d %d %d\n", inst.op, inst.l, inst.m);
	}

	fclose(output_file);



	free(token_table.arr);
	free(symbol_table.arr);
	free(code.arr);
}
