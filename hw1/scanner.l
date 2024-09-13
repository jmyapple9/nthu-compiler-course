%{
    #include<stdio.h>
    #include<stdbool.h>
    int lineCount = 0;
    char line[300] = {'\0'};
    bool pragmaSource = true;
    bool pragmaToken  = true;

    void concat(char* added){
        size_t len1 = strlen(line);
        size_t len2 = strlen(added);
        memcpy(line, line, len1);
        memcpy(line + len1, added, len2 + 1);
    }
    void Free(){
        line[0] = '\0';
    }
    void PrintLine(){
        lineCount++;
        if(pragmaSource)
            printf("%d:%s\n", lineCount,line);
        Free();
    }
    void PrintToken(char* token){
        if(pragmaToken)
            printf("#%s:%s\n",token,yytext);
        concat(yytext);
    }
    void EWTL(){ // End With Change Line
        yytext[yyleng-1]='\0'; 
        concat(yytext);
        PrintLine();
    }
    
%}

Keyword for|do|while|break|continue|if|else|return|struct|switch|case|default|void|int|double|float|char|const|signed|unsigned|short|long
Macro NULL|__COUNTER__|__LINE__|INT_MAX|INT_MIN|CHAR_MAX|CHAR_MIN|MAX|MIN
Identifier [a-zA-Z_][a-zA-Z0-9_]*
Operator "&&"|"||"|"++"|"--"|"<="|">="|"<"|">"|"=="|"!="|"+"|"-"|"*"|"/"|"%"|"="|"!"|"&"|"|"
Punctuation ":"|";"|","|"."|"["|"]"|"("|")"|"{"|"}"
Integer [+-]?[0-9]*
Floating [+-]?([0-9]*[\.][0-9]+|[0-9]+[\.][0-9]*)
CC (.|\\([?"'\\abefnrtv]|[0-9]{1,3}|n[0-7]{1,3}|x[0-9a-fA-F]*|u[0-9a-fA-F]{4}|U[0-9a-fA-F]{8}))
Char '{CC}'
String \"{CC}*\"
%x COMMENT
%x CMT2
%%

"/*"          {concat(yytext); BEGIN(COMMENT);}
<COMMENT>"*/" {concat(yytext); BEGIN(0);}
<COMMENT>.    {concat(yytext);}
<COMMENT>\n   {EWTL();}

"//"          {concat(yytext); BEGIN(CMT2);}
<CMT2>.*\\\n  {EWTL();}
<CMT2>.       {concat(yytext);}
<CMT2>[^\\]\n {EWTL(); BEGIN(0);}

[ \t]         {concat(yytext);}
[\n]          {PrintLine();}

#pragma([ \t]+)source([ \t]+)on  {concat(yytext); pragmaSource=true; }
#pragma([ \t]+)source([ \t]+)off {concat(yytext); pragmaSource=false;}
#pragma([ \t]+)token([ \t]+)on   {concat(yytext); pragmaToken=true;  }
#pragma([ \t]+)token([ \t]+)off  {concat(yytext); pragmaToken=false; }

{Keyword}       {PrintToken("key");}
{Macro}         {PrintToken("macro");}
{Identifier}    {PrintToken("id");}
{Operator}      {PrintToken("op");}
{Punctuation}   {PrintToken("punc");}
{Integer}       {PrintToken("integer");}
{Floating}      {PrintToken("float");}
{Char}          {PrintToken("char");}
{String}        {PrintToken("string");}
%%

int main(void){
    /*Golden_scanner的問題
    1. 把printf認成id
    2. 不會印出最後一行 */
    /* Further improve:
    1. 檢查有無漏掉 keywords
    1. 檢查有無漏掉 Macro
     */
    yylex();
}