%{
    #include<iostream>
    #include<cstdlib>
    #include<cstring>
    #include<vector>
    #define BUFLEN 50
    using namespace std;
    int yylex();
    int yyerror(const char* s);
    int cur_scope = 0;
    int cur_fp = 0;
    FILE *file;
    int arg = 0;
    int varCnt = 0;
    
    void Prologue(){
        fprintf(file, "   // BEGIN PROLOGUE\n");
        fprintf(file, "   sw s0, -4(sp)\n");
        fprintf(file, "   addi sp, sp, -4\n");
        fprintf(file, "   addi s0, sp, 0\n");
        fprintf(file, "   sw sp, -4(s0)\n");
        fprintf(file, "   sw s1, -8(s0)\n");
        fprintf(file, "   sw s2, -12(s0)\n");
        fprintf(file, "   sw s3, -16(s0)\n");
        fprintf(file, "   sw s4, -20(s0)\n");
        fprintf(file, "   sw s5, -24(s0)\n");
        fprintf(file, "   sw s6, -28(s0)\n");
        fprintf(file, "   sw s7, -32(s0)\n");
        fprintf(file, "   sw s8, -36(s0)\n");
        fprintf(file, "   sw s9, -40(s0)\n");
        fprintf(file, "   sw s10, -44(s0)\n");
        fprintf(file, "   sw s11, -48(s0)\n");
        fprintf(file, "   addi sp, s0, -48\n");
        fprintf(file,"   // END PROLOGUE\n\n");
    }
    void Epilogue(){
        fprintf(file,"   // BEGIN EPILOGUE\n");
        fprintf(file,"   lw s11, -48(s0)\n");
        fprintf(file,"   lw s10, -44(s0)\n");
        fprintf(file,"   lw s9, -40(s0)\n");
        fprintf(file,"   lw s8, -36(s0)\n");
        fprintf(file,"   lw s7, -32(s0)\n");
        fprintf(file,"   lw s6, -28(s0)\n");
        fprintf(file,"   lw s5, -24(s0)\n");
        fprintf(file,"   lw s4, -20(s0)\n");
        fprintf(file,"   lw s3, -16(s0)\n");
        fprintf(file,"   lw s2, -12(s0)\n");
        fprintf(file,"   lw s1, -8(s0)\n");
        fprintf(file,"   lw sp, -4(s0)\n");
        fprintf(file,"   addi sp, sp, 4\n");
        fprintf(file,"   lw s0, -4(sp)\n");
        fprintf(file,"   // END EPILOGUE\n\n");
    }
    
    int intLen(int val){
        if(val==0) return 1;
        int len = 0;
        if(val<0) len++;
        for(; val; val /= 10) len++;
        return len;
    }
    char* itoa(int val){
        int len = intLen(val);
        char *buf = (char*) calloc(len+1, sizeof(char));
        if(val==0){
            buf[0] = '0';
            return &buf[0];
        }
        snprintf(buf, len+1, "%d", val);
        return &buf[0];
    }
    char* ftoa(double val){
        int len = intLen((int)val) + 7;
        char *buf = (char*) calloc(len+1, sizeof(char));
        snprintf(buf, len+7, "%f", val);
        return &buf[0];
    }
    
    class entry{
        public:
            int scope;
            // int fp;
            int offset;
            char* name;
            // string type;
        entry (int s = 8787, int o = 8787, char* n = (char*) 0):
            scope(s), offset(o), name(n) {}
    };
    vector<entry*> table;
    int tableLookUp(char* varName){
        int i, off;
        for(i = table.size()-1; i>=0; i--){
            if(!strcmp(varName, table[i]->name)){
                off = table[i]->offset;
                break;
            }
        }
        if(i==-1){
            fprintf(file, "   // Error! No such variable %s in table !\n", varName);
            return 8787;
        }
        return off;
    }
%}

%union{
    char* str;
    int intVal;
    double dval;
}

%token <intVal> NUM
%token <dval>   DNUM
%token <str> ID CHAR STR TYPE 
%token <str> ',' ':' ';' '.' '[' ']' '(' ')' '{' '}' // Punctuations
%token <str> '+' '-' '*' '/' '%' '<' '>' '=' '!' '~' '^' '&' '|' // Operator
%token <str> SR SL INC DEC NE// Operator
%token <str> LE GE EQ AND OR // Operator
%token <str> IF ELSE SWCH CASE DEFT
%token <str> WIL DO FOR RTN BRK CTN NUL HI LO



%type  <str> program root top
%type  <str> var_decl func_decl func_def
%type  <str> sclr_decl arr_decl scalar
%type  <str> idents ident
%type  <str> arrays array arr_dims arr_contents arr_ctnts
%type  <str> expr exprs
%type  <str> para_or_not parameters para ids
%type  <str> compound_stmt stmt_or_var_decl stmt
%type  <str> condition_stmt switch_stmt while_stmt for_stmt jump_stmt
%type  <str> switch_clauses clause stmts expr_or_not
%type  <str> types cast func_call pass_para
%type  <str> rprimary lunary runary lprimary

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

program: root { }

root:top root { }
    | /* match nothing */{ }
    
top : var_decl
    | func_decl
    | func_def

func_def: TYPE ids para_or_not 
        {
            fprintf(file, "%s:\n",$2);
            Prologue();
        }
        compound_stmt
        {
            Epilogue();
            fprintf(file, "   jalr zero, 0(ra)\n");
        }

compound_stmt   : '{' '}' {}
                | '{' {cur_scope++;} stmt_or_var_decl '}' { cur_scope--; }
stmt_or_var_decl: stmt
                | var_decl
                | stmt stmt_or_var_decl     { }
                | var_decl stmt_or_var_decl { }

stmt: expr ';'      { }
    | condition_stmt{ }
    | switch_stmt   { }
    | while_stmt    { }
    | for_stmt      { }
    | jump_stmt     { }
    | compound_stmt { }

condition_stmt  : IF '(' expr ')' compound_stmt                    { }
                | IF '(' expr ')' compound_stmt ELSE compound_stmt { }

switch_stmt : SWCH '(' expr ')' '{' switch_clauses'}' { }
            | SWCH '(' expr ')' '{' '}'               { }

switch_clauses  : clause
                | clause switch_clauses { }

clause  : CASE expr ':' stmts { }
        | DEFT ':' stmts      { }

stmts   : stmt stmts    { }
        | /* nothing */ { }

while_stmt  : WIL '(' expr ')' stmt        { }
            | DO stmt WIL '(' expr ')' ';' { }

for_stmt: FOR '(' expr_or_not ';' expr_or_not ';' expr_or_not ')' stmt { }
        
jump_stmt   : RTN ';'     { }
            | BRK ';'     { }
            | CTN ';'     { }
            | RTN expr ';'{ }

expr_or_not : expr
            | /* nothing */ { }

func_decl   : TYPE ids para_or_not ';' 
            {
                fprintf(file, ".global %s\n",$2);
            }

para_or_not : '(' parameters ')' { }
            | '(' ')'            { }

ids : ID
    | '*' ID { }

parameters  : para
            | para ',' parameters { }

para: TYPE ID     { }
    | TYPE '*' ID { }

var_decl: sclr_decl
        | arr_decl

arr_decl: TYPE arrays ';' { }

arrays  : array
        | array ',' arrays { }
    

array   : ID arr_dims                  { }
        | ID arr_dims '=' arr_contents { }
    

arr_dims: '[' expr ']'          { }
        | '[' expr ']' arr_dims { }
     

arr_contents: '{' arr_ctnts '}' { }

arr_ctnts   : expr
            | arr_contents
            | expr ',' arr_ctnts         { }
            | arr_contents ',' arr_ctnts { }

sclr_decl   : TYPE idents ';' { }

idents  : scalar
        | '*' scalar { }
    
scalar  : ident
        | ident ',' idents { }

ident   : ID
        {
            varCnt++;
            int off = -48 - varCnt*4;
            table.push_back(new entry(cur_scope, off, $1));
            // printf("##########  var %s: scope = %d, offset = %d  ############\n", $1, cur_scope, off);
        }
        | ID '=' expr
        {
            varCnt++;
            int off = -48 - varCnt*4;
            table.push_back(new entry(cur_scope, off, $1));
            // printf("##########  var %s: scope = %d, offset = %d  ############\n", $1, cur_scope, off);
        }

expr    : runary
        | expr SL expr  { }
        | expr SR expr  { }
        | expr GE expr  { }
        | expr LE expr  { }
        | expr EQ expr  { }
        | expr NE expr  { }
        | expr OR expr  { }
        /* | expr '=' expr { } */
        | '*' ID '=' expr {
            int off = tableLookUp($2);
            fprintf(file, "   // assign pointer var %s\n", $2);
            fprintf(file, "   lw t0, 0(sp)\n");
            fprintf(file, "   lw t1, %d(s0)\n", off);
            // fprintf(file, "   add t1, t1, s0\n");
            fprintf(file, "   add t1, s0, t1\n");
            fprintf(file, "   sw t0, 0(t1)\n");
            fprintf(file, "   addi sp, sp, 4\n");
        }
        | ID '=' expr {
            int off = tableLookUp($1);
            fprintf(file, "   // assign var %s\n", $1);
            fprintf(file, "   lw t0, 0(sp)\n");
            fprintf(file, "   sw t0, %d(s0)\n", off);
        }
        | expr '+' expr
        {
            fprintf(file, "   // Begin ADD\n");
            fprintf(file, "   lw t0, 0(sp)\n");
            fprintf(file, "   addi sp, sp, 4\n");
            fprintf(file, "   lw t1, 0(sp)\n");
            fprintf(file, "   addi sp, sp, 4\n");
            fprintf(file, "   add t0, t0, t1\n");
            fprintf(file, "   sw t0, -4(sp)\n");
            fprintf(file, "   addi sp, sp, -4\n");
            fprintf(file, "   // END ADD\n\n");
        }
        | expr '-' expr
        {
            fprintf(file, "   // Begin SUB\n");
            fprintf(file, "   lw t0, 0(sp)\n");
            fprintf(file, "   addi sp, sp, 4\n");
            fprintf(file, "   lw t1, 0(sp)\n");
            fprintf(file, "   addi sp, sp, 4\n");
            fprintf(file, "   sub t0, t1, t0\n");
            fprintf(file, "   sw t0, -4(sp)\n");
            fprintf(file, "   addi sp, sp, -4\n");
            fprintf(file, "   // END SUB\n\n");
        }
        | expr '*' expr
        {
            fprintf(file, "   // Begin MUL\n");
            fprintf(file, "   lw t0, 0(sp)\n");
            fprintf(file, "   addi sp, sp, 4\n");
            fprintf(file, "   lw t1, 0(sp)\n");
            fprintf(file, "   addi sp, sp, 4\n");
            fprintf(file, "   mul t0, t0, t1\n");
            fprintf(file, "   sw t0, -4(sp)\n");
            fprintf(file, "   addi sp, sp, -4\n");
            fprintf(file, "   // END MUL\n\n");
        }
        | expr '/' expr
        {
            fprintf(file, "   // Begin DIV\n");
            fprintf(file, "   lw t0, 0(sp)\n");
            fprintf(file, "   addi sp, sp, 4\n");
            fprintf(file, "   lw t1, 0(sp)\n");
            fprintf(file, "   addi sp, sp, 4\n");
            fprintf(file, "   div t0, t1, t0\n");
            fprintf(file, "   sw t0, -4(sp)\n");
            fprintf(file, "   addi sp, sp, -4\n");
            fprintf(file, "   // END DIV\n\n");
        }
        | expr '%' expr { }
        | expr '<' expr { }
        | expr '>' expr { }
        | expr '&' expr { }
        | expr '|' expr { }
        | expr '^' expr { }
        | expr AND expr { }

runary  : lunary
        | rprimary
        | '!' runary            { }
        | '~' runary            { }
        | '-' runary
        {
            fprintf(file, "   lw t0, 0(sp)\n");
            fprintf(file, "   sub t0, zero, t0\n");
            fprintf(file, "   sw t0, 0(sp)\n");
        }
        | '+' runary            { }
        | '&' runary            {
            int off = tableLookUp($2);
            fprintf(file, "   // Get ref\n");
            fprintf(file, "   li t0, %d\n", off);
            // printf("offset: %d\n",off);
            // fprintf(file, "   add t0, t0, s0\n");
            fprintf(file, "   sw t0, 0(sp)\n");
            // fprintf(file, "   addi sp, sp, -4\n");
        }
        | INC expr %prec PREINC { }
        | DEC expr %prec PREDEC { }

rprimary: NUM
        {
            fprintf(file, "   // Getting NUM!\n");
            fprintf(file, "   li t0, %s\n", itoa($1));
            fprintf(file, "   sw t0, -4(sp)\n");
            fprintf(file, "   addi sp, sp, -4\n");
        }
        | DNUM
        {
            fprintf(file, "   // Getting DNUM!\n");
            fprintf(file, "   li t0, %s\n", ftoa($1));
            fprintf(file, "   sw t0, -4(sp)\n");
            fprintf(file, "   addi sp, sp, -4\n");
        }
        | CHAR
        {
            fprintf(file, "Error! Char has no implementation !\n");
        }
        | STR
        {
            fprintf(file, "Error! String has no implementation !\n");
        }
        | NUL
        {
            fprintf(file, "Error! NULL has no implementation !\n");
        }
        | LO 
        {
            fprintf(file, "   // Getting LOW!\n");
            fprintf(file, "   li t0, 0\n");
            fprintf(file, "   sw t0, -4(sp)\n");
            fprintf(file, "   addi sp, sp, -4\n");
        }
        | HI
        {
            fprintf(file, "   // Getting HIGH!\n");
            fprintf(file, "   li t0, 1\n");
            fprintf(file, "   sw t0, -4(sp)\n");
            fprintf(file, "   addi sp, sp, -4\n");
        }
        | '(' expr ')'
        {
            fprintf(file, "// Noting so far in \'(\'expr\')\'...\n");
        }

lprimary: func_call
        | ID
        {
            int off = tableLookUp($1);
            fprintf(file, "   // tableLookUp var \n");
            fprintf(file, "   lw t0, %d(s0)\n", off);
            fprintf(file, "   sw t0, -4(sp)\n");
            fprintf(file, "   addi sp, sp, -4\n");
        }
        | ID arr_dims  %prec ARR_SUB { }
        | lprimary INC %prec POSTINC { }
        | lprimary DEC %prec POSTDEC { }
        | rprimary INC %prec POSTINC { }
        | rprimary DEC %prec POSTDEC { }

lunary  : lprimary
        | '*' runary             {
            int off = tableLookUp($2);
            fprintf(file, "   // deRef: %s\n", $2);
            fprintf(file, "   lw t0, %d(s0)\n", off);
            fprintf(file, "   add t0, s0, t0\n");
            fprintf(file, "   lw t0, 0(t0)\n");
            fprintf(file, "   sw t0, -4(sp)\n");
            fprintf(file, "   addi sp, sp, -4\n");
        }
        | cast runary %prec CAST { }

cast: '(' types ')' { }

types   : TYPE
        | TYPE '*' { }

func_call   : ID '(' pass_para ')' %prec FUNC_CALL 
            {
                fprintf(file, "   // function call\n");
                fprintf(file, "   sw ra, -4(sp)\n");
                fprintf(file, "   addi sp, sp, -4\n");
                fprintf(file, "   jal ra, %s\n",$1);
                fprintf(file, "   lw ra, 0(sp)\n");
                fprintf(file, "   addi sp, sp, 4\n");
                fprintf(file, "\n");
            }

pass_para   : exprs { arg = 0; }
            | /* nothing */{ }

exprs   : expr
        {
            fprintf(file, "   // loading para\n");
            fprintf(file, "   lw a%d, 0(sp)\n", arg++);
            fprintf(file, "   addi sp, sp, 4\n");
        }
        | exprs ',' expr
        {
            fprintf(file, "   // loading para\n");
            fprintf(file, "   lw a%d, 0(sp)\n", arg++);
            fprintf(file, "   addi sp, sp, 4\n");
        }
%%

int main(void) {
    file = fopen("codegen.S","w");
    yyparse();
    fclose(file);
    return 0;
}
int yyerror(const char* s) {
    fprintf(stderr, "%s\n", s);
    return 0;
}