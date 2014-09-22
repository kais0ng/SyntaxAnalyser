%{

#include <stdio.h>
#include <stdarg.h>
#include "AST.h"

struct ASTNode* head;
int size, capacity;

int addASTNonterminal(enum ASTNodeType type, int way, int numOfSubNode, ...);

%}

/* declare tokens */

/* brace bracket and parenthesis */
%token LBRACKET RBRACKET LPARENTHESIS RPARENTHESIS LBRACE RBRACE

/* ambiguity operators */
POS_OR_ADD NEG_OR_SUB DEREF_MUL ADDRESS_BIT_AND

/* ++, -- and unary operators */
%token PLUS_PLUS MINUS_MINUS NOT
%token BIT_REVERSE

/* binary operators */
%token DIV MOD
%token LSHIFT RSHIFT BIT_AND BIT_XOR BIT_OR
%token ASSIGN DIV_ASSIGN MUL_ASSIGN MOD_ASSIGN ADD_ASSIGN SUB_ASSIGN LSHIFT_ASSIGN RSHIFT_ASSIGN BIT_AND_ASSIGN BIT_XOR_ASSIGN BIT_OR_ASSIGN
%token GREATER_THAN GREATER_EQUAL LESS_THAN LESS_EQUAL EQUAL NOT_EQUAL
%token OR AND
%token DOT ARROW

/* Question expression */
%token QUESTION_MARK

/* punctuations */

%token COMMA COLON SEMICOLON

/* keywords */
%token KEYWORD_AUTO KEYWORD_BREAK KEYWORD_CASE KEYWORD_CHAR KEYWORD_CONST KEYWORD_CONTINUE KEYWORD_DEFAULT KEYWORD_DO KEYWORD_DOUBLE KEYWORD_ELSE KEYWORD_ENUM KEYWORD_EXTERN KEYWORD_FLOAT KEYWORD_FOR KEYWORD_GOTO KEYWORD_IF KEYWORD_INT KEYWORD_LONG KEYWORD_REGISTER KEYWORD_RETURN KEYWORD_SHORT KEYWORD_SIGNED KEYWORD_SIZEOF KEYWORD_STATIC KEYWORD_STRUCT KEYWORD_SWITCH KEYWORD_TYPEDEF KEYWORD_UNION KEYWORD_UNSIGNED KEYWORD_VOID KEYWORD_VOLATILE KEYWORD_WHILE KEYWORD_INCLUDE KEYWORD_DEFINE

/* constants */
%token INTEGER_CONSTANT DOUBLE_CONSTANT FLOAT_CONSTANT LONG_CONSTANT CHARACTER_CONSTANT LITERAL_CONSTANT

/* identifier */
%token IDENTIFIER USER_DEFINED_TYPE
%%

program: program functionDefinition { $$ = addASTNonterminal(ASTNode_PROGRAM, 0, 2, $1, $2); }
    | program declarationStatement { $$ = addASTNonterminal(ASTNode_PROGRAM, 1, 2, $1, $2); }
    | { $$ = addASTNonterminal(ASTNode_PROGRAM, 3, 0); };

functionDefinition: oneOrMoreDeclarationSpecifier declarator compoundStatement { $$ = addASTNonterminal(ASTNode_FUNCTION_DEFINITION, 0, 3, $1, $2, $3 ); };

declarationStatement: declaration SEMICOLON { $$ = addASTNonterminal(ASTNode_DECLARATION_STATEMENT, 0, 2, $1, $2); };

parameter: oneOrMoreDeclarationSpecifier declarator { $$ = addASTNonterminal(ASTNode_PARAMETER, 0, 2, $1, $2); };

oneOrMoreParameter: oneOrMoreParameter COMMA parameter { $$ = addASTNonterminal(ASTNode_ONE_OR_MORE_PARAMETER, 0, 3, $1, $2, $3); }
    | parameter { $$ = addASTNonterminal(ASTNode_ONE_OR_MORE_PARAMETER, 1, 1, $1); };

expression: assignmentExpression { $$ = addASTNonterminal(ASTNode_EXPRESSION, 0, 1, $1); }
    | expression COMMA assignmentExpression { $$ = addASTNonterminal(ASTNode_EXPRESSION, 1, 3, $1, $2, $3); };

assignmentExpression: conditionalExpression { $$ = addASTNonterminal(ASTNode_ASSIGNMENT_EXPRESSION, 0, 1, $1); }
    | unaryExpression assignmentOperator assignmentExpression { $$ = addASTNonterminal(ASTNode_ASSIGNMENT_EXPRESSION, 1, 3, $1, $2, $3); };

constantExpression: conditionalExpression { $$ = addASTNonterminal(ASTNode_CONSTANT_EXPRESSION, 0, 1, $1); };

conditionalExpression: orExpression { $$ = addASTNonterminal(ASTNode_CONDITIONAL_EXPRESSION, 0, 1, $1); }
    | orExpression QUESTION_MARK expression COLON conditionalExpression { $$ = addASTNonterminal(ASTNode_CONDITIONAL_EXPRESSION, 1, 5, $1, $2, $3, $4, $5); };

orExpression: andExpression { $$ = addASTNonterminal(ASTNode_OR_EXPRESSION, 0, 1, $1); }
    | orExpression OR andExpression { $$ = addASTNonterminal(ASTNode_OR_EXPRESSION, 1, 3, $1, $2, $3); };

andExpression: bitOrExpression { $$ = addASTNonterminal(ASTNode_AND_EXPRESSION, 0, 1, $1); }
    | andExpression AND bitOrExpression { $$ = addASTNonterminal(ASTNode_AND_EXPRESSION, 1, 3, $1, $2, $3); };

bitOrExpression: bitXorExpression { $$ = addASTNonterminal(ASTNode_BIT_OR_EXPRESSION, 0, 1, $1); }
    | bitOrExpression BIT_OR bitXorExpression { $$ = addASTNonterminal(ASTNode_BIT_OR_EXPRESSION, 1, 3, $1, $2, $3); };

bitXorExpression: bitAndExpression { $$ = addASTNonterminal(ASTNode_BIT_XOR_EXPRESSION, 0, 1, $1); }
    | bitXorExpression BIT_XOR bitAndExpression { $$ = addASTNonterminal(ASTNode_BIT_XOR_EXPRESSION, 1, 3, $1, $2, $3); };

bitAndExpression: equalityExpression { $$ = addASTNonterminal(ASTNode_BIT_AND_EXPRESSION, 0, 1, $1); }
    | bitAndExpression ADDRESS_BIT_AND equalityExpression { $$ = addASTNonterminal(ASTNode_BIT_AND_EXPRESSION, 1, 3, $1, $2, $3); };

equalityExpression: relationalExpression { $$ = addASTNonterminal(ASTNode_EQUALITY_EXPRESSION, 0, 1, $1); }
    | equalityExpression EQUAL relationalExpression { $$ = addASTNonterminal(ASTNode_EQUALITY_EXPRESSION, 1, 3, $1, $2, $3); }
    | equalityExpression NOT_EQUAL relationalExpression { $$ = addASTNonterminal(ASTNode_EQUALITY_EXPRESSION, 2, 3, $1, $2, $3); };

relationalExpression: shiftExpression { $$ = addASTNonterminal(ASTNode_RELATIONAL_EXPRESSION, 0, 1, $1); }
    | relationalExpression LESS_THAN shiftExpression { $$ = addASTNonterminal(ASTNode_RELATIONAL_EXPRESSION, 1, 3, $1, $2, $3); }
    | relationalExpression GREATER_THAN shiftExpression { $$ = addASTNonterminal(ASTNode_RELATIONAL_EXPRESSION, 2, 3, $1, $2, $3); }
    | relationalExpression LESS_EQUAL shiftExpression { $$ = addASTNonterminal(ASTNode_RELATIONAL_EXPRESSION, 3, 3, $1, $2, $3); }
    | relationalExpression GREATER_EQUAL shiftExpression { $$ = addASTNonterminal(ASTNode_RELATIONAL_EXPRESSION, 4, 3, $1, $2, $3); };

shiftExpression: additiveExpression { $$ = addASTNonterminal(ASTNode_SHIFT_EXPRESSION, 0, 1, $1); }
    | shiftExpression LSHIFT additiveExpression { $$ = addASTNonterminal(ASTNode_SHIFT_EXPRESSION, 1, 3, $1, $2, $3); }
    | shiftExpression RSHIFT additiveExpression { $$ = addASTNonterminal(ASTNode_SHIFT_EXPRESSION, 2, 3, $1, $2, $3); };

additiveExpression: multiplicativeExpression { $$ = addASTNonterminal(ASTNode_ADDITIVE_EXPRESSION, 0, 1, $1); }
    | additiveExpression POS_OR_ADD multiplicativeExpression { $$ = addASTNonterminal(ASTNode_ADDITIVE_EXPRESSION, 1, 3, $1, $2, $3); }
    | additiveExpression NEG_OR_SUB multiplicativeExpression { $$ = addASTNonterminal(ASTNode_ADDITIVE_EXPRESSION, 2, 3, $1, $2, $3); };

multiplicativeExpression: castExpression { $$ = addASTNonterminal(ASTNode_MULTIPLICATIVE_EXPRESSION, 0, 1, $1); }
    | multiplicativeExpression DEREF_MUL castExpression { $$ = addASTNonterminal(ASTNode_MULTIPLICATIVE_EXPRESSION, 1, 3, $1, $2, $3); }
    | multiplicativeExpression DIV castExpression { $$ = addASTNonterminal(ASTNode_MULTIPLICATIVE_EXPRESSION, 2, 3, $1, $2, $3); }
    | multiplicativeExpression MOD castExpression { $$ = addASTNonterminal(ASTNode_MULTIPLICATIVE_EXPRESSION, 3, 3, $1, $2, $3); };

castExpression: unaryExpression { $$ = addASTNonterminal(ASTNode_CAST_EXPRESSION, 0, 1, $1); }
    | LPARENTHESIS typeName RPARENTHESIS castExpression { $$ = addASTNonterminal(ASTNode_CAST_EXPRESSION, 1, 4, $1, $2, $3, $4); };

unaryExpression: postfixExpression { $$ = addASTNonterminal(ASTNode_UNARY_EXPRESSION, 0, 1, $1); }
    | PLUS_PLUS unaryExpression { $$ = addASTNonterminal(ASTNode_UNARY_EXPRESSION, 1, 2, $1, $2); }
    | MINUS_MINUS unaryExpression { $$ = addASTNonterminal(ASTNode_UNARY_EXPRESSION, 2, 2, $1, $2); }
    | unaryOperator castExpression { $$ = addASTNonterminal(ASTNode_UNARY_EXPRESSION, 3, 2, $1, $2); }
    | KEYWORD_SIZEOF LPARENTHESIS unaryExpression RPARENTHESIS { $$ = addASTNonterminal(ASTNode_UNARY_EXPRESSION, 4, 4, $1, $2, $3, $4); }
    | KEYWORD_SIZEOF LPARENTHESIS typeName RPARENTHESIS { $$ = addASTNonterminal(ASTNode_UNARY_EXPRESSION, 5, 4, $1, $2, $3, $4); };

postfixExpression: primaryExpression { $$ = addASTNonterminal(ASTNode_POSTFIX_EXPRESSION, 0, 1, $1); }
    | postfixExpression LBRACKET expression RBRACKET { $$ = addASTNonterminal(ASTNode_POSTFIX_EXPRESSION, 1, 4, $1, $2, $3, $4); }
    | postfixExpression LPARENTHESIS RPARENTHESIS { $$ = addASTNonterminal(ASTNode_POSTFIX_EXPRESSION, 2, 3, $1, $2, $3); }
    | postfixExpression LPARENTHESIS oneOrMoreArgumentExpression RPARENTHESIS { $$ = addASTNonterminal(ASTNode_POSTFIX_EXPRESSION, 3, 4, $1, $2, $3, $4); }
    | postfixExpression DOT IDENTIFIER { $$ = addASTNonterminal(ASTNode_POSTFIX_EXPRESSION, 4, 3, $1, $2, $3); }
    | postfixExpression ARROW IDENTIFIER { $$ = addASTNonterminal(ASTNode_POSTFIX_EXPRESSION, 5, 3, $1, $2, $3); }
    | postfixExpression PLUS_PLUS { $$ = addASTNonterminal(ASTNode_POSTFIX_EXPRESSION, 6, 2, $1, $2); }
    | postfixExpression MINUS_MINUS { $$ = addASTNonterminal(ASTNode_POSTFIX_EXPRESSION, 7, 2, $1, $2); };

primaryExpression: IDENTIFIER { $$ = addASTNonterminal(ASTNode_PRIMARY_EXPRESSION, 0, 1, $1); }
    | constant { $$ = addASTNonterminal(ASTNode_PRIMARY_EXPRESSION, 1, 1, $1); }
    | LPARENTHESIS expression RPARENTHESIS { $$ = addASTNonterminal(ASTNode_PRIMARY_EXPRESSION, 2, 3, $1, $2, $3); };

constant: INTEGER_CONSTANT { $$ = addASTNonterminal(ASTNode_CONSTANT, 0, 1, $1); }
    | LONG_CONSTANT { $$ = addASTNonterminal(ASTNode_CONSTANT, 1, 1, $1); }
    | DOUBLE_CONSTANT { $$ = addASTNonterminal(ASTNode_CONSTANT, 2, 1, $1); }
    | FLOAT_CONSTANT { $$ = addASTNonterminal(ASTNode_CONSTANT, 3, 1, $1); }
    | CHARACTER_CONSTANT { $$ = addASTNonterminal(ASTNode_CONSTANT, 4, 1, $1); }
    | LITERAL_CONSTANT { $$ = addASTNonterminal(ASTNode_CONSTANT, 5, 1, $1); };

oneOrMoreArgumentExpression: oneOrMoreArgumentExpression COMMA assignmentExpression { $$ = addASTNonterminal(ASTNode_ONE_OR_MORE_ARGUMENT_EXPRESSION, 0, 3, $1, $2, $3); }
    | assignmentExpression { $$ = addASTNonterminal(ASTNode_ONE_OR_MORE_ARGUMENT_EXPRESSION, 1, 1, $1); };

declarationOrStatement: declarationStatement { $$ = addASTNonterminal(ASTNode_DECLARATION_OR_STATEMENT, 0, 1, $1); }
    | statement { $$ = addASTNonterminal(ASTNode_DECLARATION_OR_STATEMENT, 1, 1, $1); };

oneOrMoreDeclarationOrStatement: oneOrMoreDeclarationOrStatement declarationOrStatement { $$ = addASTNonterminal(ASTNode_ONE_OR_MORE_DECLARATION_OR_STATEMENT, 0, 2, $1, $2); }
    | declarationOrStatement { $$ = addASTNonterminal(ASTNode_ONE_OR_MORE_DECLARATION_OR_STATEMENT, 1, 1, $1); };

declarationSpecifier: storageClassSpecifier { $$ = addASTNonterminal(ASTNode_DECLARATION_SPECIFIER, 0, 1, $1); }
    | typeSpecifier { $$ = addASTNonterminal(ASTNode_DECLARATION_SPECIFIER, 1, 1, $1); }
    | typeQualifier { $$ = addASTNonterminal(ASTNode_DECLARATION_SPECIFIER, 2, 1, $1); };

oneOrMoreDeclarationSpecifier: oneOrMoreDeclarationSpecifier declarationSpecifier { $$ = addASTNonterminal(ASTNode_ONE_OR_MORE_DECLARATION_SPECIFIER, 0, 2, $1, $2); }
    | declarationSpecifier { $$ = addASTNonterminal(ASTNode_ONE_OR_MORE_DECLARATION_SPECIFIER, 1, 1, $1); };

declaration: oneOrMoreDeclarationSpecifier oneOrMoreInitDeclarator { $$ = addASTNonterminal(ASTNode_DECLARATION, 0, 2, $1, $2); };

oneOrMoreInitDeclarator: oneOrMoreInitDeclarator COMMA initDeclarator { $$ = addASTNonterminal(ASTNode_ONE_OR_MORE_INIT_DECLARATOR, 0, 3, $1, $2, $3); }
    | initDeclarator { $$ = addASTNonterminal(ASTNode_ONE_OR_MORE_INIT_DECLARATOR, 1, 1, $1); };

initDeclarator: declarator { $$ = addASTNonterminal(ASTNode_INIT_DECLARATOR, 0, 1, $1); }
    | declarator ASSIGN initializer { $$ = addASTNonterminal(ASTNode_INIT_DECLARATOR, 1, 3, $1, $2, $3); };

declarator: pointer directDeclarator { $$ = addASTNonterminal(ASTNode_DECLARATOR, 0, 2, $1, $2); }
    | directDeclarator { $$ = addASTNonterminal(ASTNode_DECLARATOR, 1, 1, $1); };

directDeclarator: IDENTIFIER { $$ = addASTNonterminal(ASTNode_DIRECT_DECLARATOR, 0, 1, $1); }
    | directDeclarator LBRACKET constantExpression RBRACKET { $$ = addASTNonterminal(ASTNode_DIRECT_DECLARATOR, 1, 4, $1, $2, $3, $4); }
    | directDeclarator LPARENTHESIS oneOrMoreParameter RPARENTHESIS { $$ = addASTNonterminal(ASTNode_DIRECT_DECLARATOR, 2, 4, $1, $2, $3, $4); }
    | directDeclarator LPARENTHESIS RPARENTHESIS { $$ = addASTNonterminal(ASTNode_DIRECT_DECLARATOR, 3, 3, $1, $2, $3); }
	| directDeclarator LPARENTHESIS KEYWORD_VOID RPARENTHESIS{ $$ = addASTNonterminal(ASTNode_DIRECT_DECLARATOR, 4, 4, $1, $2, $3, $4); };

pointer: DEREF_MUL { $$ = addASTNonterminal(ASTNode_POINTER, 0, 1, $1); }
	| DEREF_MUL oneOrMoreTypeQualifier { $$ = addASTNonterminal(ASTNode_POINTER, 1, 2, $1, $2); }
	| DEREF_MUL pointer { $$ = addASTNonterminal(ASTNode_POINTER, 2, 2, $1, $2); };

oneOrMoreInitializer: oneOrMoreInitializer COMMA initializer { $$ = addASTNonterminal(ASTNode_ONE_OR_MORE_INITIALIZER, 0, 3, $1, $2, $3); }
    | initializer { $$ = addASTNonterminal(ASTNode_ONE_OR_MORE_INITIALIZER, 1, 1, $1); };

initializer: assignmentExpression { $$ = addASTNonterminal(ASTNode_INITIALIZER, 0, 1, $1); }
    | LBRACE oneOrMoreInitializer RBRACE { $$ = addASTNonterminal(ASTNode_INITIALIZER, 1, 3, $1, $2, $3); }

compoundStatement: LBRACE RBRACE { $$ = addASTNonterminal(ASTNode_COMPOUND_STATEMENT, 0, 2, $1, $2); }
    | LBRACE oneOrMoreDeclarationOrStatement RBRACE { $$ = addASTNonterminal(ASTNode_COMPOUND_STATEMENT, 1, 3, $1, $2, $3); };

statement: labeledStatement { $$ = addASTNonterminal(ASTNode_STATEMENT, 0, 1, $1); }
    | expressionStatement { $$ = addASTNonterminal(ASTNode_STATEMENT, 1, 1, $1); }
    | selectionStatement { $$ = addASTNonterminal(ASTNode_STATEMENT, 2, 1, $1); }
    | iterationStatement { $$ = addASTNonterminal(ASTNode_STATEMENT, 3, 1, $1); }
    | jumpStatement { $$ = addASTNonterminal(ASTNode_STATEMENT, 4, 1, $1); }
    | compoundStatement { $$ = addASTNonterminal(ASTNode_STATEMENT, 5, 1, $1); };


labeledStatement: IDENTIFIER COLON statement { $$ = addASTNonterminal(ASTNode_LABELED_STATEMENT, 0, 3, $1, $2, $3); }
    | caseStatement { $$ = addASTNonterminal(ASTNode_LABELED_STATEMENT, 1, 1, $1); }
    | defaultStatement { $$ = addASTNonterminal(ASTNode_LABELED_STATEMENT, 2, 1, $1); };

caseStatement: KEYWORD_CASE constantExpression COLON statement { $$ = addASTNonterminal(ASTNode_CASE_STATEMENT, 0, 4, $1, $2, $3, $4); };

defaultStatement: KEYWORD_DEFAULT COLON statement { $$ = addASTNonterminal(ASTNode_DEFAULT_STATEMENT, 0, 3, $1, $2, $3); };

expressionStatement: expression SEMICOLON { $$ = addASTNonterminal(ASTNode_EXPRESSION_STATEMENT, 0, 2, $1, $2); }
    | SEMICOLON { $$ = addASTNonterminal(ASTNode_EXPRESSION_STATEMENT, 1, 1, $1); };

selectionStatement: ifStatement { $$ = addASTNonterminal(ASTNode_SELECTION_STATEMENT, 0, 1, $1); }
    | switchStatement { $$ = addASTNonterminal(ASTNode_SELECTION_STATEMENT, 1, 1, $1); };

ifStatement: KEYWORD_IF LPARENTHESIS expression RPARENTHESIS statement { $$ = addASTNonterminal(ASTNode_IF_STATEMENT, 0, 5, $1, $2, $3, $4, $5); }
    | KEYWORD_IF LPARENTHESIS expression RPARENTHESIS statement KEYWORD_ELSE statement { $$ = addASTNonterminal(ASTNode_IF_STATEMENT, 1, 7, $1, $2, $3, $4, $5, $6, $7); }

switchStatement: KEYWORD_SWITCH LPARENTHESIS expression RPARENTHESIS statement { $$ = addASTNonterminal(ASTNode_SWITCH_STATEMENT, 0, 5, $1, $2, $3, $4, $5); };

iterationStatement: whileStatement { $$ = addASTNonterminal(ASTNode_ITERATION_STATEMENT, 0, 1, $1); }
    | doWhileStatement { $$ = addASTNonterminal(ASTNode_ITERATION_STATEMENT, 1, 1, $1); }
    | forStatement { $$ = addASTNonterminal(ASTNode_ITERATION_STATEMENT, 2, 1, $1); };

whileStatement: KEYWORD_WHILE LPARENTHESIS expression RPARENTHESIS statement { $$ = addASTNonterminal(ASTNode_WHILE_STATEMENT, 0, 5, $1, $2, $3, $4, $5); };

doWhileStatement: KEYWORD_DO statement KEYWORD_WHILE LPARENTHESIS expression RPARENTHESIS SEMICOLON { $$ = addASTNonterminal(ASTNode_DO_WHILE_STATEMENT, 0, 7, $1, $2, $3, $4, $5, $6, $7); };

forStatement: KEYWORD_FOR LPARENTHESIS SEMICOLON SEMICOLON RPARENTHESIS statement { $$ = addASTNonterminal(ASTNode_FOR_STATEMENT, 0, 6, $1, $2, $3, $4, $5, $6); }
    | KEYWORD_FOR LPARENTHESIS SEMICOLON SEMICOLON expression RPARENTHESIS statement { $$ = addASTNonterminal(ASTNode_FOR_STATEMENT, 1, 7, $1, $2, $3, $4, $5, $6, $7); }
    | KEYWORD_FOR LPARENTHESIS SEMICOLON expression SEMICOLON RPARENTHESIS statement { $$ = addASTNonterminal(ASTNode_FOR_STATEMENT, 2, 7, $1, $2, $3, $4, $5, $6, $7); }
    | KEYWORD_FOR LPARENTHESIS SEMICOLON expression SEMICOLON expression RPARENTHESIS statement { $$ = addASTNonterminal(ASTNode_FOR_STATEMENT, 3, 8, $1, $2, $3, $4, $5, $6, $7, $8); }
    | KEYWORD_FOR LPARENTHESIS expression SEMICOLON SEMICOLON RPARENTHESIS statement { $$ = addASTNonterminal(ASTNode_FOR_STATEMENT, 4, 7, $1, $2, $3, $4, $5, $6, $7); }
    | KEYWORD_FOR LPARENTHESIS expression SEMICOLON SEMICOLON expression RPARENTHESIS statement { $$ = addASTNonterminal(ASTNode_FOR_STATEMENT, 5, 8, $1, $2, $3, $4, $5, $6, $7, $8); }
    | KEYWORD_FOR LPARENTHESIS expression SEMICOLON expression SEMICOLON RPARENTHESIS statement { $$ = addASTNonterminal(ASTNode_FOR_STATEMENT, 6, 8, $1, $2, $3, $4, $5, $6, $7, $8); }
    | KEYWORD_FOR LPARENTHESIS expression SEMICOLON expression SEMICOLON expression RPARENTHESIS statement { $$ = addASTNonterminal(ASTNode_FOR_STATEMENT, 7, 9, $1, $2, $3, $4, $5, $6, $7, $8, $9); }
    | KEYWORD_FOR LPARENTHESIS declaration SEMICOLON SEMICOLON RPARENTHESIS statement { $$ = addASTNonterminal(ASTNode_FOR_STATEMENT, 8, 7, $1, $2, $3, $4, $5, $6, $7); }
    | KEYWORD_FOR LPARENTHESIS declaration SEMICOLON SEMICOLON expression RPARENTHESIS statement { $$ = addASTNonterminal(ASTNode_FOR_STATEMENT, 9, 8, $1, $2, $3, $4, $5, $6, $7, $8); }
    | KEYWORD_FOR LPARENTHESIS declaration SEMICOLON expression SEMICOLON RPARENTHESIS statement { $$ = addASTNonterminal(ASTNode_FOR_STATEMENT, 10, 8, $1, $2, $3, $4, $5, $6, $7, $8); }
    | KEYWORD_FOR LPARENTHESIS declaration SEMICOLON expression SEMICOLON expression RPARENTHESIS statement { $$ = addASTNonterminal(ASTNode_FOR_STATEMENT, 11, 9, $1, $2, $3, $4, $5, $6, $7, $8, $9); }


jumpStatement: gotoStatement { $$ = addASTNonterminal(ASTNode_JUMP_STATEMENT, 0, 1, $1); }
    | continueStatement { $$ = addASTNonterminal(ASTNode_JUMP_STATEMENT, 1, 1, $1); }
    | breakStatement { $$ = addASTNonterminal(ASTNode_JUMP_STATEMENT, 2, 1, $1); }
    | returnStatement { $$ = addASTNonterminal(ASTNode_JUMP_STATEMENT, 3, 1, $1); };

gotoStatement: KEYWORD_GOTO IDENTIFIER SEMICOLON { $$ = addASTNonterminal(ASTNode_GOTO_STATEMENT, 0, 3, $1, $2, $3); };

continueStatement: KEYWORD_CONTINUE SEMICOLON { $$ = addASTNonterminal(ASTNode_CONTINUE_STATEMENT, 0, 2, $1, $2); };

breakStatement: KEYWORD_BREAK SEMICOLON { $$ = addASTNonterminal(ASTNode_BREAK_STATEMENT, 0, 2, $1, $2); };

returnStatement: KEYWORD_RETURN SEMICOLON { $$ = addASTNonterminal(ASTNode_RETURN_STATEMENT, 0, 2, $1, $2); }
    | KEYWORD_RETURN expression SEMICOLON { $$ = addASTNonterminal(ASTNode_RETURN_STATEMENT, 0, 3, $1, $2, $3); };

assignmentOperator: ASSIGN { $$ = addASTNonterminal(ASTNode_ASSIGNMENT_OPERATOR, 0, 1, $1); }
    | MUL_ASSIGN { $$ = addASTNonterminal(ASTNode_ASSIGNMENT_OPERATOR, 1, 1, $1); }
    | DIV_ASSIGN { $$ = addASTNonterminal(ASTNode_ASSIGNMENT_OPERATOR, 2, 1, $1); }
    | MOD_ASSIGN { $$ = addASTNonterminal(ASTNode_ASSIGNMENT_OPERATOR, 3, 1, $1); }
    | ADD_ASSIGN { $$ = addASTNonterminal(ASTNode_ASSIGNMENT_OPERATOR, 4, 1, $1); }
    | SUB_ASSIGN { $$ = addASTNonterminal(ASTNode_ASSIGNMENT_OPERATOR, 5, 1, $1); }
    | LSHIFT_ASSIGN { $$ = addASTNonterminal(ASTNode_ASSIGNMENT_OPERATOR, 6, 1, $1); }
    | RSHIFT_ASSIGN { $$ = addASTNonterminal(ASTNode_ASSIGNMENT_OPERATOR, 7, 1, $1); }
    | BIT_AND_ASSIGN { $$ = addASTNonterminal(ASTNode_ASSIGNMENT_OPERATOR, 8, 1, $1); }
    | BIT_XOR_ASSIGN { $$ = addASTNonterminal(ASTNode_ASSIGNMENT_OPERATOR, 9, 1, $1); }
    | BIT_OR_ASSIGN { $$ = addASTNonterminal(ASTNode_ASSIGNMENT_OPERATOR, 10, 1, $1); };

unaryOperator: ADDRESS_BIT_AND { $$ = addASTNonterminal(ASTNode_UNARY_OPERATOR, 0, 1, $1); }
    | DEREF_MUL { $$ = addASTNonterminal(ASTNode_UNARY_OPERATOR, 1, 1, $1); }
    | POS_OR_ADD { $$ = addASTNonterminal(ASTNode_UNARY_OPERATOR, 2, 1, $1); }
    | NEG_OR_SUB { $$ = addASTNonterminal(ASTNode_UNARY_OPERATOR, 3, 1, $1); }
    | BIT_REVERSE { $$ = addASTNonterminal(ASTNode_UNARY_OPERATOR, 4, 1, $1); }
    | NOT { $$ = addASTNonterminal(ASTNode_UNARY_OPERATOR, 5, 1, $1); };

typeSpecifier: KEYWORD_VOID { $$ = addASTNonterminal(ASTNode_TYPE_SPECIFIER, 0, 1, $1); }
    | KEYWORD_CHAR { $$ = addASTNonterminal(ASTNode_TYPE_SPECIFIER, 1, 1, $1); }
    | KEYWORD_SHORT { $$ = addASTNonterminal(ASTNode_TYPE_SPECIFIER, 2, 1, $1); }
    | KEYWORD_INT { $$ = addASTNonterminal(ASTNode_TYPE_SPECIFIER, 3, 1, $1); }
    | KEYWORD_LONG { $$ = addASTNonterminal(ASTNode_TYPE_SPECIFIER, 4, 1, $1); }
    | KEYWORD_FLOAT { $$ = addASTNonterminal(ASTNode_TYPE_SPECIFIER, 5, 1, $1); }
    | KEYWORD_DOUBLE { $$ = addASTNonterminal(ASTNode_TYPE_SPECIFIER, 6, 1, $1); }
    | KEYWORD_SIGNED { $$ = addASTNonterminal(ASTNode_TYPE_SPECIFIER, 7, 1, $1); }
    | KEYWORD_UNSIGNED { $$ = addASTNonterminal(ASTNode_TYPE_SPECIFIER, 8, 1, $1); }
    | structOrUnionSpecifier { $$ = addASTNonterminal(ASTNode_TYPE_SPECIFIER, 9, 1, $1); };

storageClassSpecifier: KEYWORD_AUTO { $$ = addASTNonterminal(ASTNode_STORAGE_CLASS_SPECIFIER, 0, 1, $1); }
    | KEYWORD_EXTERN { $$ = addASTNonterminal(ASTNode_STORAGE_CLASS_SPECIFIER, 1, 1, $1); }
    | KEYWORD_STATIC { $$ = addASTNonterminal(ASTNode_STORAGE_CLASS_SPECIFIER, 2, 1, $1); }
    | KEYWORD_REGISTER { $$ = addASTNonterminal(ASTNode_STORAGE_CLASS_SPECIFIER, 3, 1, $1); }
    | KEYWORD_TYPEDEF { $$ = addASTNonterminal(ASTNode_STORAGE_CLASS_SPECIFIER, 4, 1, $1); };

typeQualifier: KEYWORD_CONST { $$ = addASTNonterminal(ASTNode_TYPE_QUALIFIER, 0, 1, $1); }
    | KEYWORD_VOLATILE { $$ = addASTNonterminal(ASTNode_TYPE_QUALIFIER, 1, 1, $1); };
    
oneOrMoreTypeQualifier: oneOrMoreTypeQualifier typeQualifier { $$ = addASTNonterminal(ASTNode_ONE_OR_MORE_TYPE_QUALIFIER, 0, 2, $1, $2); }
	| typeQualifier { $$ = addASTNonterminal(ASTNode_ONE_OR_MORE_TYPE_QUALIFIER, 1, 1, $1); };

specifierQualifier: typeSpecifier { $$ = addASTNonterminal(ASTNode_SPECIFIER_QUALIFIER, 0, 1, $1); }
    | typeQualifier { $$ = addASTNonterminal(ASTNode_SPECIFIER_QUALIFIER, 1, 1, $1); };

oneOrMoreSpecifierQualifier: oneOrMoreSpecifierQualifier specifierQualifier { $$ = addASTNonterminal(ASTNode_ONE_OR_MORE_SPECIFIER_QUALIFIER, 0, 2, $1, $2); }
    | specifierQualifier { $$ = addASTNonterminal(ASTNode_ONE_OR_MORE_SPECIFIER_QUALIFIER, 1, 1, $1); };

typeName: oneOrMoreSpecifierQualifier { $$ = addASTNonterminal(ASTNode_TYPE_NAME, 0, 1, $1); }
	| oneOrMoreSpecifierQualifier pointer { $$ = addASTNonterminal(ASTNode_TYPE_NAME, 1, 2, $1, $2); };
structOrUnion: KEYWORD_STRUCT { $$ = addASTNonterminal(ASTNode_STRUCT_OR_UNION, 0, 1, $1); }
    | KEYWORD_UNION { $$ = addASTNonterminal(ASTNode_STRUCT_OR_UNION, 1, 1, $1); };

structOrUnionSpecifier: IDENTIFIER { $$ = addASTNonterminal(ASTNode_STRUCT_OR_UNION_SPECIFIER, 0, 1, $1); }
	| structOrUnion IDENTIFIER { $$ = addASTNonterminal(ASTNode_STRUCT_OR_UNION_SPECIFIER, 1, 2, $1, $2); }
	| structOrUnion LBRACE oneOrMoreStructDeclaration RBRACE{ $$ = addASTNonterminal(ASTNode_STRUCT_OR_UNION_SPECIFIER, 2, 4, $1, $2, $3, $4);}
	| structOrUnion IDENTIFIER LBRACE oneOrMoreStructDeclaration RBRACE{ $$ = addASTNonterminal(ASTNode_STRUCT_OR_UNION_SPECIFIER, 3, 5, $1, $2, $3, $4, $5);};

oneOrMoreStructDeclaration: structDeclaration { $$ = addASTNonterminal(ASTNode_ONE_OR_MORE_STRUCT_DECLARATION, 0, 1, $1);}
	| oneOrMoreStructDeclaration structDeclaration { $$ = addASTNonterminal(ASTNode_ONE_OR_MORE_STRUCT_DECLARATION, 1, 2, $1, $2);};
	
structDeclaration: oneOrMoreSpecifierQualifier oneOrMoreStructDeclarator SEMICOLON { $$ = addASTNonterminal(ASTNode_STRUCT_DECLARATION, 0, 3, $1, $2, $3);};

oneOrMoreStructDeclarator: structDeclarator { $$ = addASTNonterminal(ASTNode_ONE_OR_MORE_STRUCT_DECLARATOR, 0, 1, $1);}
	| oneOrMoreStructDeclarator COMMA structDeclarator { $$ = addASTNonterminal(ASTNode_ONE_OR_MORE_STRUCT_DECLARATOR, 1, 3, $1, $2, $3);};
	
structDeclarator: declarator { $$ = addASTNonterminal(ASTNode_STRUCT_DECLARATOR, 0 , 1, $1);}
	| COLON constantExpression { $$ = addASTNonterminal(ASTNode_STRUCT_DECLARATOR, 1 , 2, $1, $2);}
	| declarator COLON constantExpression { $$ = addASTNonterminal(ASTNode_STRUCT_DECLARATOR, 2 , 3, $1, $2, $3);};
%%
char* EnumNames[] = {
	"ASTNode_Terminal_LBRACKET",
	"ASTNode_Terminal_RBRACKET",
	"ASTNode_Terminal_LPARENTHESIS",
	"ASTNode_Terminal_RPARENTHESIS",
	"ASTNode_Terminal_LBRACE",
	"ASTNode_Terminal_RBRACE",
	"ASTNode_Terminal_POS_OR_ADD",
	"ASTNode_Terminal_NEG_OR_SUB",
	"ASTNode_Terminal_DEREF_MUL",
	"ASTNode_Terminal_ADDRESS_BIT_AND",
	"ASTNode_Terminal_PLUS_PLUS",
	"ASTNode_Terminal_MINUS_MINUS",
	"ASTNode_Terminal_NOT",
	"ASTNode_Terminal_BIT_REVERSE",
	"ASTNode_Terminal_DIV",
	"ASTNode_Terminal_MOD",
	"ASTNode_Terminal_LSHIFT",
	"ASTNode_Terminal_RSHIFT",
	"ASTNode_Terminal_BIT_XOR",
	"ASTNode_Terminal_BIT_OR",
	"ASTNode_Terminal_ASSIGN",
	"ASTNode_Terminal_DIV_ASSIGN",
	"ASTNode_Terminal_MUL_ASSIGN",
	"ASTNode_Terminal_MOD_ASSIGN",
	"ASTNode_Terminal_ADD_ASSIGN",
	"ASTNode_Terminal_SUB_ASSIGN",
	"ASTNode_Terminal_LSHIFT_ASSIGN",
	"ASTNode_Terminal_RSHIFT_ASSIGN",
	"ASTNode_Terminal_BIT_AND_ASSIGN",
	"ASTNode_Terminal_BIT_XOR_ASSIGN",
	"ASTNode_Terminal_BIT_OR_ASSIGN",
	"ASTNode_Terminal_GREATER_THAN",
	"ASTNode_Terminal_GREATER_EQUAL",
	"ASTNode_Terminal_LESS_THAN",
	"ASTNode_Terminal_LESS_EQUAL",
	"ASTNode_Terminal_EQUAL",
	"ASTNode_Terminal_NOT_EQUAL",
	"ASTNode_Terminal_OR",
	"ASTNode_Terminal_AND",
	"ASTNode_Terminal_DOT",
	"ASTNode_Terminal_DOMAIN",
	"ASTNode_Terminal_QUESTION_MARK",
	"ASTNode_Terminal_COMMA",
	"ASTNode_Terminal_COLON",
	"ASTNode_Terminal_SEMICOLON",
	"ASTNode_Terminal_KEYWORD_AUTO",
	"ASTNode_Terminal_KEYWORD_BREAK",
	"ASTNode_Terminal_KEYWORD_CASE",
	"ASTNode_Terminal_KEYWORD_CHAR",
	"ASTNode_Terminal_KEYWORD_CONST",
	"ASTNode_Terminal_KEYWORD_CONTINUE",
	"ASTNode_Terminal_KEYWORD_DEFAULT",
	"ASTNode_Terminal_KEYWORD_DO",
	"ASTNode_Terminal_KEYWORD_DOUBLE",
	"ASTNode_Terminal_KEYWORD_ELSE",
	"ASTNode_Terminal_KEYWORD_ENUM",
	"ASTNode_Terminal_KEYWORD_EXTERN",
	"ASTNode_Terminal_KEYWORD_FLOAT",
	"ASTNode_Terminal_KEYWORD_FOR",
	"ASTNode_Terminal_KEYWORD_GOTO",
	"ASTNode_Terminal_KEYWORD_IF",
	"ASTNode_Terminal_KEYWORD_INT",
	"ASTNode_Terminal_KEYWORD_LONG",
	"ASTNode_Terminal_KEYWORD_REGISTER",
	"ASTNode_Terminal_KEYWORD_RETURN",
	"ASTNode_Terminal_KEYWORD_SHORT",
	"ASTNode_Terminal_KEYWORD_SIGNED",
	"ASTNode_Terminal_KEYWORD_SIZEOF",
	"ASTNode_Terminal_KEYWORD_STATIC",
	"ASTNode_Terminal_KEYWORD_STRUCT",
	"ASTNode_Terminal_KEYWORD_SWITCH",
	"ASTNode_Terminal_KEYWORD_TYPEDEF",
	"ASTNode_Terminal_KEYWORD_UNION",
	"ASTNode_Terminal_KEYWORD_UNSIGNED",
	"ASTNode_Terminal_KEYWORD_VOID",
	"ASTNode_Terminal_KEYWORD_VOLATILE",
	"ASTNode_Terminal_KEYWORD_WHILE",
	"ASTNode_Terminal_KEYWORD_INCLUDE",
	"ASTNode_Terminal_KEYWORD_DEFINE",
	"ASTNode_Terminal_INTEGER_CONSTANT",
	"ASTNode_Terminal_DOUBLE_CONSTANT",
	"ASTNode_Terminal_FLOAT_CONSTANT",
	"ASTNode_Terminal_LONG_CONSTANT",
	"ASTNode_Terminal_CHARACTER_CONSTANT", 
	"ASTNode_Terminal_LITERAL_CONSTANT", 
	"ASTNode_Terminal_IDENTIFIER",

	"ASTNode_NonTerminal_PROGRAM",
	"ASTNode_NonTerminal_FUNCTION_DEFINITION",
	"ASTNode_NonTerminal_PARAMETER",
	"ASTNode_NonTerminal_ONE_OR_MORE_PARAMETER",
	"ASTNode_NonTerminal_EXPRESSION",
	"ASTNode_NonTerminal_ASSIGNMENT_EXPRESSION",
	"ASTNode_NonTerminal_CONSTANT_EXPRESSION",
	"ASTNode_NonTerminal_CONDITIONAL_EXPRESSION",
	"ASTNode_NonTerminal_OR_EXPRESSION",
	"ASTNode_NonTerminal_AND_EXPRESSION",
	"ASTNode_NonTerminal_BIT_OR_EXPRESSION",
	"ASTNode_NonTerminal_BIT_XOR_EXPRESSION",
	"ASTNode_NonTerminal_BIT_AND_EXPRESSION",
	"ASTNode_NonTerminal_EQUALITY_EXPRESSION",
	"ASTNode_NonTerminal_RELATIONAL_EXPRESSION",
	"ASTNode_NonTerminal_SHIFT_EXPRESSION",
	"ASTNode_NonTerminal_ADDITIVE_EXPRESSION",
	"ASTNode_NonTerminal_MULTIPLICATIVE_EXPRESSION",
	"ASTNode_NonTerminal_CAST_EXPRESSION",
	"ASTNode_NonTerminal_UNARY_EXPRESSION",
	"ASTNode_NonTerminal_POSTFIX_EXPRESSION",
	"ASTNode_NonTerminal_PRIMARY_EXPRESSION",
	"ASTNode_NonTerminal_CONSTANT",
	"ASTNode_NonTerminal_ONE_OR_MORE_ARGUMENT_EXPRESSION",
	"ASTNode_NonTerminal_DECLARATION_OR_STATEMENT",
	"ASTNode_NonTerminal_ONE_OR_MORE_DECLARATION_OR_STATEMENT",
	"ASTNode_NonTerminal_DECLARATION_SPECIFIER",
	"ASTNode_NonTerminal_ONE_OR_MORE_DECLARATION_SPECIFIER",
	"ASTNode_NonTerminal_DECLARATION",
	"ASTNode_NonTerminal_DECLARATION_STATEMENT", 
	"ASTNode_NonTerminal_ONE_OR_MORE_DECLARATION_STATEMENT", 
	"ASTNode_NonTerminal_ONE_OR_MORE_INIT_DECLARATOR",
	"ASTNode_NonTerminal_INIT_DECLARATOR",
	"ASTNode_NonTerminal_DECLARATOR",
	"ASTNode_NonTerminal_DIRECT_DECLARATOR",
	"ASTNode_NonTerminal_POINTER",
	"ASTNode_NonTerminal_ONE_OR_MORE_INITIALIZER",
	"ASTNode_NonTerminal_INITIALIZER",
	"ASTNode_NonTerminal_COMPOUND_STATEMENT",
	"ASTNode_NonTerminal_STATEMENT",
	"ASTNode_NonTerminal_LABELED_STATEMENT",
	"ASTNode_NonTerminal_CASE_STATEMENT", 
	"ASTNode_NonTerminal_DEFAULT_STATEMENT", 
	"ASTNode_NonTerminal_EXPRESSION_STATEMENT",
	"ASTNode_NonTerminal_SELECTION_STATEMENT",
	"ASTNode_NonTerminal_IF_STATEMENT",
	"ASTNode_NonTerminal_SWITCH_STATEMENT",
	"ASTNode_NonTerminal_ITERATION_STATEMENT",
	"ASTNode_NonTerminal_WHILE_STATEMENT",
	"ASTNode_NonTerminal_DO_WHILE_STATEMENT",
	"ASTNode_NonTerminal_FOR_STATEMENT",
	"ASTNode_NonTerminal_JUMP_STATEMENT",
	"ASTNode_NonTerminal_GOTO_STATEMENT",
	"ASTNode_NonTerminal_CONTINUE_STATEMENT",
	"ASTNode_NonTerminal_BREAK_STATEMENT",
	"ASTNode_NonTerminal_RETURN_STATEMENT",
	"ASTNode_NonTerminal_ASSIGNMENT_OPERATOR",
	"ASTNode_NonTerminal_UNARY_OPERATOR",
	"ASTNode_NonTerminal_TYPE_SPECIFIER",
	"ASTNode_NonTerminal_STORAGE_CLASS_SPECIFIER",
	"ASTNode_NonTerminal_TYPE_QUALIFIER",
	"ASTNode_NonTerminal_ONE_OR_MORE_TYPE_QUALIFIER",
	"ASTNode_NonTerminal_SPECIFIER_QUALIFIER",
	"ASTNode_NonTerminal_ONE_OR_MORE_SPECIFIER_QUALIFIER",
	"ASTNode_NonTerminal_ONE_OR_MORE_IDENTIFIER", 
	"ASTNode_NonTerminal_TYPE_NAME", 
	"ASTNode_NonTerminal_STRUCT_OR_UNION", 
	"ASTNode_NonTerminal_STRUCT_OR_UNION_SPECIFIER", 
	"ASTNode_NonTerminal_ONE_OR_MORE_STRUCT_DECLARATION",
	"ASTNode_NonTerminal_STRUCT_DECLARATION",
	"ASTNode_NonTerminal_ONE_OR_MORE_STRUCT_DECLARATOR",
	"ASTNode_NonTerminal_STRUCT_DECLARATOR" 
};

int addASTNonterminal(enum ASTNodeType type, int way, int numOfSubNode, ...) {
	int i;
	int j;
    if(size == capacity) {
	capacity *= 2;
	struct ASTNode *newSpace = (struct ASTNode*)malloc(sizeof(struct ASTNode) * capacity);
	for(i = 0; i < size; i++) {
	    newSpace[i].type = head[i].type;
	    newSpace[i].way = head[i].way;
	    newSpace[i].numOfSubNode = head[i].numOfSubNode;
	    newSpace[i].content = head[i].content;
	    for(j = 0; j < MAX_SUBNODE; j++)
			newSpace[i].subNode[j] = head[i].subNode[j];
	}
	free(head);
	head = newSpace;
    }
	
    struct ASTNode *newNode = &head[size];
    newNode->type = type;
    newNode->way = way;
    newNode->numOfSubNode = numOfSubNode;
    newNode->content = NULL;

    int val = 0;
    va_list argPtr;
    va_start(argPtr, numOfSubNode);
    for(i = 0; i < numOfSubNode; i++)
		newNode->subNode[i] = va_arg(argPtr, int);
    return size++;
}

int main(int argc, char **argv)
{
    size = 0;
    capacity = 1;
	int i;
	int j;
    head = (struct ASTNode*)malloc(sizeof(struct ASTNode) * capacity);
    yyparse();
    for(i = 0; i < size; i++) {
	printf("%s\n", EnumNames[head[i].type]);
	printf("%d\n", head[i].way);
	printf("%d\n", head[i].numOfSubNode);
	if(head[i].numOfSubNode > 0) {
	    for(j = 0; j < head[i].numOfSubNode; j++)
		printf("%d ", head[i].subNode[j]);
	    puts("");
	}
	if(NULL == head[i].content) puts("0");
	else {
	    puts("1");
	    printf("%s\n", head[i].content);
	}
    }
    return 0;
}

yyerror(char *s)
{
    fprintf(stderr, "error: %s\n", s);
}

