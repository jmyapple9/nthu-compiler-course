%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include <stdarg.h>
    int intLen(int val){
        if(val==0) return 1;
        int len = 0;
        if(val<0) len++;
        for(; val; val /= 10) len++;
        return len;
    }
    char* itoa(int val){
        int len = intLen(val);
        char *buf = calloc(len+1, sizeof(char));
        if(val==0){
            buf[0] = '0';
            return &buf[0];
        }
        snprintf(buf, len+1, "%d", val);
        return &buf[0];
    }
    char* ftoa(double val){
        int len = intLen((int)val) + 7;
        char *buf = calloc(len+1, sizeof(char));
        snprintf(buf, len+7, "%f", val);
        return &buf[0];
    }
    char* process(int n, ...){
        int size = 0;
        char* bucket[10];
        va_list valist;
        va_start(valist, n);
        for(int i = 0; i<n; i++){
            bucket[i] = va_arg(valist, char*);
            size += strlen(bucket[i]);
        }
        va_end(valist);

        char* ret = calloc(size+1, sizeof(char));
        for(int i = 0; i<n; i++){
            char* str = bucket[i];
            ret = strcat(ret, str);
            free(str);
        }
        return ret;
    }
    char* process_tag(char* tag, int n, ...){
        int size = 0;
        char* bucket[10];
        va_list valist;
        va_start(valist, n);
        for(int i = 0; i<n; i++){
            bucket[i] = va_arg(valist, char*);
            size += strlen(bucket[i]);
        }
        va_end(valist);

        char* ret = calloc(size+2*(strlen(tag))+6, sizeof(char));
        ret = strcat(ret, "<");
        ret = strcat(ret, tag);
        ret = strcat(ret, ">");

        for(int i = 0; i<n; i++){
            char* str = bucket[i];
            ret = strcat(ret, str);
            free(str);
        }
        
        ret = strcat(ret, "</");
        ret = strcat(ret, tag);
        ret = strcat(ret, ">");

        return ret;
    }
    char* Empty(){ return (char*) calloc(1, sizeof(char)); }
    char* Zero(){ char* zero = (char*) calloc(2, sizeof(char)); zero[0]='0'; return zero;}
%}

%union{
    int intVal;
    double dval;
    char *charv;
}

%token <intVal> NUM
%token <dval>   DNUM
%token <charv> ID CHAR STR TYPE 
%token <charv> ',' ':' ';' '.' '[' ']' '(' ')' '{' '}'             // Punctuations
%token <charv> '+' '-' '*' '/' '%' '<' '>' '=' '!' '~' '^' '&' '|' // Operator
%token <charv> SR SL INC DEC NE // Operator
%token <charv> LE GE EQ AND OR  // Operator
%token <charv> IF ELSE SWCH CASE DEFT
%token <charv> WIL DO FOR RTN BRK CTN NUL



%type  <charv> program root top
%type  <charv> var_decl func_decl func_def
%type  <charv> sclr_decl arr_decl scalar
%type  <charv> idents ident
%type  <charv> arrays array arr_dims arr_contents arr_ctnts
%type  <charv> expr exprs
%type  <charv> para_or_not parameters para ids
%type  <charv> compound_stmt stmt_or_var_decl stmt
%type  <charv> condition_stmt switch_stmt while_stmt for_stmt jump_stmt
%type  <charv> switch_clauses clause stmts expr_or_not
%type  <charv> types cast func_call pass_para
%type  <charv> rprimary lunary runary lprimary

%right '='
%left OR
%left AND
%left '|'
%left '^'
%left '&'
%left EQ NE
%left '>' GE
%left '<' LE
%left SR SL
%left '+' '-'
%left '*' '/' '%'
%right '!' '~'
%right CAST PREINC PREDEC
%left FUNC_CALL ARR_SUB POSTINC POSTDEC

%start program

%%

program: root { printf("%s", $1); free($1);}

root:root top            {$$ = process(2, $1, $2);}
    | /* match nothing */{$$ = Empty(); }
    
top : var_decl
    | func_decl
    | func_def

func_def: TYPE ids para_or_not compound_stmt {$$ = process_tag("func_def", 4, $1, $2, $3, $4);}

compound_stmt   : '{' '}'                  {$$ = process(2, $1, $2);}
                | '{' stmt_or_var_decl '}' {$$ = process(3, $1, $2, $3);}

stmt_or_var_decl: stmt
                | var_decl
                | stmt stmt_or_var_decl     {$$ = process(2, $1, $2);}
                | var_decl stmt_or_var_decl {$$ = process(2, $1, $2);}

stmt: expr ';'      {$$ = process_tag("stmt", 2, $1, $2);}
    | condition_stmt{$$ = process_tag("stmt", 1, $1);}
    | switch_stmt   {$$ = process_tag("stmt", 1, $1);}
    | while_stmt    {$$ = process_tag("stmt", 1, $1);}
    | for_stmt      {$$ = process_tag("stmt", 1, $1);}
    | jump_stmt     {$$ = process_tag("stmt", 1, $1);}
    | compound_stmt {$$ = process_tag("stmt", 1, $1);}

condition_stmt  : IF '(' expr ')' compound_stmt                    {$$ = process(5, $1, $2, $3, $4, $5);}
                | IF '(' expr ')' compound_stmt ELSE compound_stmt {$$ = process(7, $1, $2, $3, $4, $5, $6, $7);}

switch_stmt : SWCH '(' expr ')' '{' switch_clauses'}' {$$ = process(7, $1, $2, $3, $4, $5, $6, $7);}
            | SWCH '(' expr ')' '{' '}'               {$$ = process(6, $1, $2, $3, $4, $5, $6);}

switch_clauses  : clause
                | clause switch_clauses {$$ = process(2, $1, $2);}

clause  : CASE expr ':' stmts {$$ = process(4, $1, $2, $3, $4);}
        | DEFT ':' stmts      {$$ = process(3, $1, $2, $3);}

stmts   : stmt stmts    {$$ = process(2, $1, $2);}
        | /* nothing */ {$$ = Empty(); }

while_stmt  : WIL '(' expr ')' stmt        {$$ = process(5, $1, $2, $3, $4, $5);}
            | DO stmt WIL '(' expr ')' ';' {$$ = process(7, $1, $2, $3, $4, $5, $6, $7);}

for_stmt: FOR '(' expr_or_not ';' expr_or_not ';' expr_or_not ')' stmt {$$ = process(9, $1, $2, $3, $4, $5, $6, $7, $8, $9);}
        
jump_stmt   : RTN ';'     {$$ = process(2, $1, $2);}
            | BRK ';'     {$$ = process(2, $1, $2);}
            | CTN ';'     {$$ = process(2, $1, $2);}
            | RTN expr ';'{$$ = process(3, $1, $2, $3);}

expr_or_not : expr
            | /* nothing */ {$$ = Empty(); }

func_decl   : TYPE ids para_or_not ';' {$$ = process_tag("func_decl", 4, $1, $2, $3, $4);};

para_or_not : '(' parameters ')' {$$ = process(3, $1, $2, $3);}
            | '(' ')'            {$$ = process(2, $1, $2);}

ids : ID
    | '*' ID {$$ = process(2, $1, $2);}

parameters  : para
            | para ',' parameters {$$ = process(3, $1, $2, $3);}

para: TYPE ID     {$$ = process(2, $1, $2);}
    | TYPE '*' ID {$$ = process(3, $1, $2, $3);}

var_decl: sclr_decl
        | arr_decl

arr_decl: TYPE arrays ';' {$$ = process_tag("array_decl", 3, $1, $2, $3);}

arrays  : array
        | array ',' arrays {$$ = process(3, $1, $2, $3);}
    

array   : ID arr_dims                  {$$ = process(2, $1, $2);}
        | ID arr_dims '=' arr_contents {$$ = process(4, $1, $2, $3, $4);}
    

arr_dims: '[' expr ']'          {$$ = process(3, $1, $2, $3);}
        | '[' expr ']' arr_dims {$$ = process(4, $1, $2, $3, $4);}
     

arr_contents: '{' arr_ctnts '}' {$$ = process(3, $1, $2, $3);}

arr_ctnts   : expr
            | arr_contents
            | expr ',' arr_ctnts         {$$ = process(3, $1, $2, $3);}
            | arr_contents ',' arr_ctnts {$$ = process(3, $1, $2, $3);}

sclr_decl   : TYPE idents ';'{$$ = process_tag("scalar_decl", 3, $1, $2, $3); }

idents  : scalar
        | '*' scalar {$$ = process(2, $1, $2); }
    
scalar  : ident
        | ident ',' idents {$$ = process(3, $1, $2, $3); }

ident   : ID
        | ID '=' expr {$$ = process(3, $1, $2, $3); }

expr    : runary
        | expr SL expr  {$$ = process_tag("expr", 3, $1, $2, $3);}
        | expr SR expr  {$$ = process_tag("expr", 3, $1, $2, $3);}
        | expr GE expr  {$$ = process_tag("expr", 3, $1, $2, $3);}
        | expr LE expr  {$$ = process_tag("expr", 3, $1, $2, $3);}
        | expr EQ expr  {$$ = process_tag("expr", 3, $1, $2, $3);}
        | expr NE expr  {$$ = process_tag("expr", 3, $1, $2, $3);}
        | expr OR expr  {$$ = process_tag("expr", 3, $1, $2, $3);}
        | expr '=' expr {$$ = process_tag("expr", 3, $1, $2, $3);}
        | expr '+' expr {$$ = process_tag("expr", 3, $1, $2, $3);}
        | expr '-' expr {$$ = process_tag("expr", 3, $1, $2, $3);}
        | expr '*' expr {$$ = process_tag("expr", 3, $1, $2, $3);}
        | expr '/' expr {$$ = process_tag("expr", 3, $1, $2, $3);}
        | expr '%' expr {$$ = process_tag("expr", 3, $1, $2, $3);}
        | expr '<' expr {$$ = process_tag("expr", 3, $1, $2, $3);}
        | expr '>' expr {$$ = process_tag("expr", 3, $1, $2, $3);}
        | expr '&' expr {$$ = process_tag("expr", 3, $1, $2, $3);}
        | expr '|' expr {$$ = process_tag("expr", 3, $1, $2, $3);}
        | expr '^' expr {$$ = process_tag("expr", 3, $1, $2, $3);}
        | expr AND expr {$$ = process_tag("expr", 3, $1, $2, $3);}

runary  : lunary
        | rprimary
        | '!' runary            {$$ = process_tag("expr", 2, $1, $2);}
        | '~' runary            {$$ = process_tag("expr", 2, $1, $2);}
        | '-' runary            {$$ = process_tag("expr", 2, $1, $2);}
        | '+' runary            {$$ = process_tag("expr", 2, $1, $2);}
        | '&' runary            {$$ = process_tag("expr", 2, $1, $2);}
        | INC expr %prec PREINC {$$ = process_tag("expr", 2, $1, $2);}
        | DEC expr %prec PREDEC {$$ = process_tag("expr", 2, $1, $2);}

rprimary: NUM          {$$ = process_tag("expr", 1, itoa($1));}
        | DNUM         {$$ = process_tag("expr", 1, ftoa($1));}
        | CHAR         {$$ = process_tag("expr", 1, $1);}
        | STR          {$$ = process_tag("expr", 1, $1);}
        | NUL          {$$ = process_tag("expr", 1, Zero());}
        | '(' expr ')' {$$ = process_tag("expr", 3, $1, $2, $3);}

lprimary: func_call
        | ID                         {$$ = process_tag("expr", 1, $1);}
        | ID arr_dims  %prec ARR_SUB {$$ = process_tag("expr", 2, $1, $2); }
        | lprimary INC %prec POSTINC {$$ = process_tag("expr", 2, $1, $2);}
        | lprimary DEC %prec POSTDEC {$$ = process_tag("expr", 2, $1, $2);}
        | rprimary INC %prec POSTINC {$$ = process_tag("expr", 2, $1, $2);}
        | rprimary DEC %prec POSTDEC {$$ = process_tag("expr", 2, $1, $2);}

lunary  : lprimary
        | '*' runary             {$$ = process_tag("expr", 2, $1, $2);}
        | cast runary %prec CAST {$$ = process_tag("expr", 2, $1, $2);}

cast: '(' types ')' {$$ = process(3, $1, $2, $3);}

types   : TYPE
        | TYPE '*' {$$ = process(2, $1, $2);}

func_call   : lprimary '(' pass_para ')' %prec FUNC_CALL {$$ = process_tag("expr", 4, $1, $2, $3, $4);}
            | rprimary '(' pass_para ')' %prec FUNC_CALL {$$ = process_tag("expr", 4, $1, $2, $3, $4);}

pass_para   : exprs
            | /* nothing */{ $$ = Empty(); }

exprs   : expr
        | exprs ',' expr {$$ = process(3, $1, $2, $3);}
%%

int main(void) {
    yyparse();
    return 0;
}
int yyerror(char *s) {
    fprintf(stderr, "%s\n", s);
    return 0;
}