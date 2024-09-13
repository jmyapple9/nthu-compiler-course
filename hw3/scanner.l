%{
    #include<stdio.h>
    #include<stdbool.h>
    #include<stdlib.h>
    #include "y.tab.h"
    int lineCount = 0;

    int countNoSpace(char* text){
        int total = strlen(text);
        int noSpaceLen = 0;
        for(int i = 0; i<total; i++){
            if(text[i]!=' ' && text[i]!='\t')
                noSpaceLen++;
        }
        return noSpaceLen;
    }
    char* rm_blank(char* raw_type){
        int len = countNoSpace(raw_type);
        char* ret = (char*) calloc(len+1,sizeof(char));
        int l=0;
        for(int i = 0; i<strlen(raw_type); i++){
            if(raw_type[i]!=' ' && raw_type[i]!='\t')
                ret[l++] = raw_type[i];
        }
        return ret;

    }
    
%}


Type (("const")?[ \t]*("signed"|"unsigned")?[ \t]*("long long"|"long"|"short")?[ \t]*"int")|(("const")?[ \t]*("signed"|"unsigned")?[ \t]*("long long"|"long"|"short"|"char"))|(("const")?[ \t]*("signed"|"unsigned"|"float"|"double"|"void"))|("const")
Macro __COUNTER__|__LINE__|INT_MAX|INT_MIN|CHAR_MAX|CHAR_MIN|MAX|MIN
Identifier [a-zA-Z_][a-zA-Z0-9_]*
Operator ">>"|"<<"|"^"|"~"|"&&"|"||"|"<="|">="|"<"|">"|"=="|"!="|"+"|"-"|"*"|"/"|"="|"%"|"!"|"&"|"|"
Punctuation ":"|";"|","|"."|"["|"]"|"("|")"|"{"|"}"
Integer [0-9]*
Floating ([0-9]*[\.][0-9]+|[0-9]+[\.][0-9]*)
Char ('(.|\\([\\?'"abefnrtv]|[0-9]{1,3}|n[0-7]{1,3}|x[0-9a-fA-F]*|u[0-9a-fA-F]{4}|U[0-9a-fA-F]{8}))')
String \"([^\\\"]|\\.)*\"
%x COMMENT
%x CMT2
%%


"/*"          { BEGIN(COMMENT);}
<COMMENT>"*/" { BEGIN(0);}
<COMMENT>.    {}
<COMMENT>\n   {}

"//"          {BEGIN(CMT2);}
<CMT2>.*\\\n  {}
<CMT2>.       {}
<CMT2>[^\\]\n {BEGIN(0);}

[ \t]         {}
[\n]          {}

"if"            { yylval.str=strdup(yytext); return IF;  }
"else"          { yylval.str=strdup(yytext); return ELSE;}
"switch"        { yylval.str=strdup(yytext); return SWCH;}
"case"          { yylval.str=strdup(yytext); return CASE;}
"default"       { yylval.str=strdup(yytext); return DEFT;}
"while"         { yylval.str=strdup(yytext); return WIL; }
"do"            { yylval.str=strdup(yytext); return DO;  }
"for"           { yylval.str=strdup(yytext); return FOR; }
"return"        { yylval.str=strdup(yytext); return RTN; }
"break"         { yylval.str=strdup(yytext); return BRK; }
"continue"      { yylval.str=strdup(yytext); return CTN; }
"NULL"          { yylval.str=strdup(yytext); return NUL; }
"HIGH"          { yylval.str=strdup(yytext); return HI; }
"LOW"           { yylval.str=strdup(yytext); return LO; }
"++"            { yylval.str=strdup(yytext); return INC; }
"--"            { yylval.str=strdup(yytext); return DEC; }
"!="            { yylval.str=strdup(yytext); return NE; }
"<<"            { yylval.str=strdup(yytext); return SL; }
">>"            { yylval.str=strdup(yytext); return SR; }
"<="            { yylval.str=strdup(yytext); return LE; }
">="            { yylval.str=strdup(yytext); return GE; }
"=="            { yylval.str=strdup(yytext); return EQ; }
"&&"            { yylval.str=strdup(yytext); return AND; }
"||"            { yylval.str=strdup(yytext); return OR; }
{Type}          { yylval.str=strdup(rm_blank(yytext)); return TYPE;}
{Macro}         { yylval.str=strdup(yytext); return yytext[0];}
{Operator}      { yylval.str=strdup(yytext); return yytext[0];}
{Punctuation}   { yylval.str=strdup(yytext); return yytext[0];}
{Identifier}    { yylval.str=strdup(yytext); return ID;}
{Integer}       { yylval.intVal=atoi(yytext); return NUM;}
{Floating}      { yylval.dval  =atof(yytext); return DNUM;}
{String}        {/* printf("string\n");  */yylval.str=strdup(yytext); return STR;}
{Char}          {/* printf("char\n");    */yylval.str=strdup(yytext); return CHAR;}
%%

int yywrap(){
    return 1;
}