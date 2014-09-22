%{

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "parser.tab.h"
#include "AST.h"

extern struct ASTNode* head;
extern int size;
extern int capacity;
extern char* EnumNames[];

void show(char* , char* );
int addASTNode(enum ASTNodeType type, char* content);
int lines=0, column;

%}

DECIMAL_CONSTANT	[0-9]+
HEXADECIMAL_CONSTANT	0x[0-9a-fA-F]+
INT_CONSTANT		{DECIMAL_CONSTANT}|{HEXADECIMAL_CONSTANT}
LONG_CONSTANT		{INT_CONSTANT}[lL]
DOUBLE_CONSTANT		([0-9]*["."]?[0-9]+)([e][+-]?[0-9]+)?
FLOAT_CONSTANT		{DOUBLE_CONSTANT}[fF]
CHARACTER_CONSTANT	\'[^\n\']\'
LITERAL_CONSTANT	\"[^\n\"]*\"
IDENTIFIER		[_a-zA-Z][_a-zA-Z0-9]*
SINGLE_COMMENT      "//"[^\n]*\n
MULTILINE_COMMENT   "/*"([^*]|\*+[^/*])*\*+"/"
STARTWITH_SHARP_LINE ^["#"].*\n

%%

"{"			{ show(yytext, "LBRACE"); yylval = addASTNode(ASTNode_LBRACE, NULL); return LBRACE; }
"}"			{ show(yytext, "RBRACE"); yylval = addASTNode(ASTNode_RBRACE, NULL); return RBRACE; }
"["			{ show(yytext, "LBRACKET"); yylval = addASTNode(ASTNode_LBRACKET, NULL); return LBRACKET; }
"]"			{ show(yytext, "RBRACKET"); yylval = addASTNode(ASTNode_RBRACKET, NULL); return RBRACKET; }
"("			{ show(yytext, "LPARENTHESIS"); yylval = addASTNode(ASTNode_LPARENTHESIS, NULL); return LPARENTHESIS; }
")"			{ show(yytext, "RPARENTHESIS"); yylval = addASTNode(ASTNode_RPARENTHESIS, NULL); return RPARENTHESIS; }
"."			{ show(yytext, "DOT");  yylval = addASTNode(ASTNode_DOT, NULL); return DOT; }
"->"			{ show(yytext, "DOMAIN");  yylval = addASTNode(ASTNode_ARROW, NULL); return ARROW; }
"+"			{ show(yytext, "POS_OR_ADD");  yylval = addASTNode(ASTNode_POS_OR_ADD, NULL); return POS_OR_ADD; }
"-"			{ show(yytext, "NEG_SUB");  yylval = addASTNode(ASTNode_NEG_OR_SUB, NULL); return NEG_OR_SUB; }
"++"			{ show(yytext, "PLUS_PLUS");  yylval = addASTNode(ASTNode_PLUS_PLUS, NULL); return PLUS_PLUS; }
"--"			{ show(yytext, "MINUS_MINUS");  yylval = addASTNode(ASTNode_MINUS_MINUS, NULL); return MINUS_MINUS; }
"*"			{ show(yytext, "STAR");  yylval = addASTNode(ASTNode_DEREF_MUL, NULL); return DEREF_MUL; }
"&"			{ show(yytext, "ADDRESS");  yylval = addASTNode(ASTNode_ADDRESS_BIT_AND, NULL); return ADDRESS_BIT_AND; }
"!"			{ show(yytext, "NOT");  yylval = addASTNode(ASTNode_NOT, NULL); return NOT; }
"~"			{ show(yytext, "BIT_REVERSE"); yylval = addASTNode(ASTNode_BIT_REVERSE, NULL); return BIT_REVERSE; }
"/"			{ show(yytext, "DIV");  yylval = addASTNode(ASTNode_DIV, NULL); return DIV; }
"%"			{ show(yytext, "MOD");  yylval = addASTNode(ASTNode_MOD, NULL); return MOD; }
"<<"			{ show(yytext, "LSHIFT");  yylval = addASTNode(ASTNode_LSHIFT, NULL); return LSHIFT; }
">>"			{ show(yytext, "RSHIFT");  yylval = addASTNode(ASTNode_RSHIFT, NULL); return RSHIFT; }
">"			{ show(yytext, "GREATER_THAN");  yylval = addASTNode(ASTNode_GREATER_THAN, NULL); return GREATER_THAN; }
">="			{ show(yytext, "GREATER_EQUAL");  yylval = addASTNode(ASTNode_GREATER_EQUAL, NULL); return GREATER_EQUAL; }
"<"			{ show(yytext, "LESS_THAN");  yylval = addASTNode(ASTNode_LESS_THAN, NULL); return LESS_THAN; }
"<="			{ show(yytext, "LESS_EQUAL");  yylval = addASTNode(ASTNode_LESS_EQUAL, NULL); return LESS_EQUAL; }
"=="			{ show(yytext, "EQUAL");  yylval = addASTNode(ASTNode_EQUAL, NULL); return EQUAL; }
"!="			{ show(yytext, "NOT_EQUAL");  yylval = addASTNode(ASTNode_NOT_EQUAL, NULL); return NOT_EQUAL; }
"^"			{ show(yytext, "BIT_XOR"); yylval = addASTNode(ASTNode_BIT_XOR, NULL); return BIT_XOR; }
"|"			{ show(yytext, "BIT_OR"); yylval = addASTNode(ASTNode_BIT_OR, NULL); return BIT_OR; }
"&&"			{ show(yytext, "AND"); yylval = addASTNode(ASTNode_AND, NULL); return AND; }
"||"			{ show(yytext, "OR"); yylval = addASTNode(ASTNode_OR, NULL); return OR; }
"?"			{ show(yytext, "QUESTION_MARK"); yylval = addASTNode(ASTNode_QUESTION_MARK, NULL); return QUESTION_MARK; }
"="			{ show(yytext, "ASSIGN"); yylval = addASTNode(ASTNode_ASSIGN, NULL); return ASSIGN; }
"/="			{ show(yytext, "DIV_ASSIGN"); yylval = addASTNode(ASTNode_DIV_ASSIGN, NULL); return DIV_ASSIGN; }
"*="			{ show(yytext, "MUL_ASSIGN"); yylval = addASTNode(ASTNode_MUL_ASSIGN, NULL); return MUL_ASSIGN; }
"%="			{ show(yytext, "MOD_ASSIGN"); yylval = addASTNode(ASTNode_MOD_ASSIGN, NULL); return MOD_ASSIGN; }
"+="			{ show(yytext, "PLUS_ASSIGN"); yylval = addASTNode(ASTNode_ADD_ASSIGN, NULL); return ADD_ASSIGN; }
"-="			{ show(yytext, "SUB_ASSIGN"); yylval = addASTNode(ASTNode_SUB_ASSIGN, NULL); return SUB_ASSIGN; }
"<<="			{ show(yytext, "LSHIFT_ASSIGN"); yylval = addASTNode(ASTNode_LSHIFT_ASSIGN, NULL); return LSHIFT_ASSIGN; }
">>="			{ show(yytext, "RSHIFT_ASSIGN"); yylval = addASTNode(ASTNode_RSHIFT_ASSIGN, NULL); return RSHIFT_ASSIGN; }
"&="			{ show(yytext, "BIT_AND_ASSIGN"); yylval = addASTNode(ASTNode_BIT_AND_ASSIGN, NULL); return BIT_AND_ASSIGN; }
"^="			{ show(yytext, "BIT_XOR_ASSIGN"); yylval = addASTNode(ASTNode_BIT_XOR_ASSIGN, NULL); return BIT_XOR_ASSIGN; }
"|="			{ show(yytext, "BIT_OR_ASSIGN"); return yylval = addASTNode(ASTNode_BIT_OR_ASSIGN, NULL); BIT_OR_ASSIGN; }
","			{ show(yytext, "COMMA"); yylval = addASTNode(ASTNode_COMMA, NULL); return COMMA; }
":"			{ show(yytext, "COLON"); yylval = addASTNode(ASTNode_COLON, NULL); return COLON; }
";"			{ show(yytext, "SEMICOLON"); yylval = addASTNode(ASTNode_SEMICOLON, NULL); return SEMICOLON; }

"auto"			{ show(yytext, "KEYWORD_AUTO"); yylval = addASTNode(ASTNode_KEYWORD_AUTO, NULL); return KEYWORD_AUTO; }
"break"			{ show(yytext, "KEYWORD_BREAK"); yylval = addASTNode(ASTNode_KEYWORD_BREAK, NULL); return KEYWORD_BREAK; }
"case"			{ show(yytext, "KEYWORD_CASE"); yylval = addASTNode(ASTNode_KEYWORD_CASE, NULL); return KEYWORD_CASE; }
"char"			{ show(yytext, "KEYWORD_CHAR"); yylval = addASTNode(ASTNode_KEYWORD_CHAR, NULL); return KEYWORD_CHAR; }
"const"			{ show(yytext, "KEYWORD_CONST"); yylval = addASTNode(ASTNode_KEYWORD_CONST, NULL); return KEYWORD_CONST; }
"continue"		{ show(yytext, "KEYWORD_CONTINUE"); yylval = addASTNode(ASTNode_KEYWORD_CONTINUE, NULL); return KEYWORD_CONTINUE; }
"default"		{ show(yytext, "KEYWORD_DEFAULT"); yylval = addASTNode(ASTNode_KEYWORD_DEFAULT, NULL); return KEYWORD_DEFAULT; }
"do"			{ show(yytext, "KEYWORD_DO"); yylval = addASTNode(ASTNode_KEYWORD_DO, NULL); return KEYWORD_DO; }
"double"		{ show(yytext, "KEYWORD_DOUBLE"); yylval = addASTNode(ASTNode_KEYWORD_DOUBLE, NULL); return KEYWORD_DOUBLE; }
"else"			{ show(yytext, "KEYWORD_ELSE"); yylval = addASTNode(ASTNode_KEYWORD_ELSE, NULL); return KEYWORD_ELSE; }
"enum"			{ show(yytext, "KEYWORD_ENUM"); yylval = addASTNode(ASTNode_KEYWORD_ENUM, NULL); return KEYWORD_ENUM; }
"extern"		{ show(yytext, "KEYWORD_EXTERN"); yylval = addASTNode(ASTNode_KEYWORD_EXTERN, NULL); return KEYWORD_EXTERN; }
"float"			{ show(yytext, "KEYWORD_FLOAT"); yylval = addASTNode(ASTNode_KEYWORD_FLOAT, NULL); return KEYWORD_FLOAT; }
"for"			{ show(yytext, "KEYWORD_FOR"); yylval = addASTNode(ASTNode_KEYWORD_FOR, NULL); return KEYWORD_FOR; }
"goto"			{ show(yytext, "KEYWORD_GOTO"); yylval = addASTNode(ASTNode_KEYWORD_GOTO, NULL); return KEYWORD_GOTO; }
"if"			{ show(yytext, "KEYWORD_IF"); yylval = addASTNode(ASTNode_KEYWORD_IF, NULL); return KEYWORD_IF; }
"int"			{ show(yytext, "KEYWORD_INT"); yylval = addASTNode(ASTNode_KEYWORD_INT, NULL); return KEYWORD_INT; }
"long"			{ show(yytext, "KEYWORD_LONG"); yylval = addASTNode(ASTNode_KEYWORD_LONG, NULL); return KEYWORD_LONG; }
"register"		{ show(yytext, "KEYWORD_REGISTER"); yylval = addASTNode(ASTNode_KEYWORD_REGISTER, NULL); return KEYWORD_REGISTER; }
"return"		{ show(yytext, "KEYWORD_RETURN"); yylval = addASTNode(ASTNode_KEYWORD_RETURN, NULL); return KEYWORD_RETURN; }
"short"			{ show(yytext, "KEYWORD_SHORT"); yylval = addASTNode(ASTNode_KEYWORD_SHORT, NULL); return KEYWORD_SHORT; }
"signed"		{ show(yytext, "KEYWORD_SIGNED"); yylval = addASTNode(ASTNode_KEYWORD_SIGNED, NULL); return KEYWORD_SIGNED; }
"sizeof"		{ show(yytext, "KEYWORD_SIZEOF"); yylval = addASTNode(ASTNode_KEYWORD_SIZEOF, NULL); return KEYWORD_SIZEOF; }
"static"		{ show(yytext, "KEYWORD_STATIC"); yylval = addASTNode(ASTNode_KEYWORD_STATIC, NULL); return KEYWORD_STATIC; }
"struct"		{ show(yytext, "KEYWORD_STRUCT"); yylval = addASTNode(ASTNode_KEYWORD_STRUCT, NULL); return KEYWORD_STRUCT; }
"switch"		{ show(yytext, "KEYWORD_SWITCH"); yylval = addASTNode(ASTNode_KEYWORD_SWITCH, NULL); return KEYWORD_SWITCH; }
"typedef"		{ show(yytext, "KEYWORD_TYPEDEF"); yylval = addASTNode(ASTNode_KEYWORD_TYPEDEF, NULL); return KEYWORD_TYPEDEF; }
"union"			{ show(yytext, "KEYWORD_UNION"); yylval = addASTNode(ASTNode_KEYWORD_UNION, NULL); return KEYWORD_UNION; }
"unsigned"		{ show(yytext, "KEYWORD_UNSIGNED"); yylval = addASTNode(ASTNode_KEYWORD_UNSIGNED, NULL); return KEYWORD_UNSIGNED; }
"void"			{ show(yytext, "KEYWORD_VOID"); yylval = addASTNode(ASTNode_KEYWORD_VOID, NULL); return KEYWORD_VOID; }
"volatile"		{ show(yytext, "KEYWORD_VOLATILE"); yylval = addASTNode(ASTNode_KEYWORD_VOLATILE, NULL); return KEYWORD_VOLATILE; }
"while"			{ show(yytext, "KEYWORD_WHILE"); yylval = addASTNode(ASTNode_KEYWORD_WHILE, NULL); return KEYWORD_WHILE; }
"include"		{ show(yytext, "KEYWORD_INCLUDE"); yylval = addASTNode(ASTNode_KEYWORD_INCLUDE, NULL); return KEYWORD_INCLUDE; }
"define"		{ show(yytext, "KEYWORD_DEFINE"); yylval = addASTNode(ASTNode_KEYWORD_DEFINE, NULL); return KEYWORD_DEFINE; }

{INT_CONSTANT}		{ show(yytext, "integer" ); yylval = addASTNode(ASTNode_INTEGER_CONSTANT, yytext); return INTEGER_CONSTANT; }
{LONG_CONSTANT}		{ show(yytext, "long"); yylval = addASTNode(ASTNode_LONG_CONSTANT, yytext); return LONG_CONSTANT; }
{DOUBLE_CONSTANT}	{ show(yytext, "double"); yylval = addASTNode(ASTNode_DOUBLE_CONSTANT, yytext); return DOUBLE_CONSTANT; }
{FLOAT_CONSTANT}	{ show(yytext, "float"); yylval = addASTNode(ASTNode_FLOAT_CONSTANT, yytext); return FLOAT_CONSTANT; }
{CHARACTER_CONSTANT}	{ show(yytext, "character"); yylval = addASTNode(ASTNode_CHARACTER_CONSTANT, yytext); return CHARACTER_CONSTANT; }
{LITERAL_CONSTANT}	{ show(yytext, "string"); yylval = addASTNode(ASTNode_LITERAL_CONSTANT, yytext); return LITERAL_CONSTANT; }
{IDENTIFIER}		{ show(yytext, "IDENTIFIER"); yylval = addASTNode(ASTNode_IDENTIFIER, yytext);return IDENTIFIER; }
{SINGLE_COMMENT}	{ }
{MULTILINE_COMMENT} { }
{STARTWITH_SHARP_LINE} {}
[\n]			{ lines++; }
[ \t]			{ /* ignore whitespace */ }
.			{ printf("syntax error"); exit(1); }
%%

void show(char *s1, char *s2) {
//    printf("%s\t\t%s\n", s1, s2);
}

int addASTNode(enum ASTNodeType type, char* content) {
    if(size == capacity) 
	{
		capacity *= 2;
		struct ASTNode *newSpace = (struct ASTNode*)malloc(sizeof(struct ASTNode) * capacity);
		int i;
		for(i = 0; i < size; ++i) {
			newSpace[i].type = head[i].type;
			newSpace[i].way = head[i].way;
			newSpace[i].numOfSubNode = head[i].numOfSubNode;
			newSpace[i].content = head[i].content;
			int j;
			for(j = 0; j < MAX_SUBNODE; ++j)
			newSpace[i].subNode[j] = head[i].subNode[j];
		}
		free(head);
		head = newSpace;
    }
    struct ASTNode* newNode = &head[size];
    newNode->type = type;
    newNode->way = -1;
    newNode->numOfSubNode = 0;
    int i;
    for(i = 0; i < MAX_SUBNODE; ++i)
	newNode->subNode[i] = -1;
    if(NULL == content) {
		newNode->content = NULL;
    } 
	else {
		newNode->content = (char*)malloc(sizeof(char) * (strlen(content) + 1));
		strcpy(newNode->content, content);
		//printf("%s\n", newNode->content);
    }
    return size++;
}

int yywrap() {
    return 1;
}
