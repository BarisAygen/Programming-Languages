%{
#include <stdio.h>

void yyerror(const char *msg){
    return;
}

%}

%token tSEND tSET tTO tFROM tAT tCOMMA tCOLON tLPR tRPR tLBR tRBR tIDENT  tSTRING tMAIL tENDMAIL tSCHEDULE tENDSCHEDULE tTIME tDATE tADDRESS

%%

program :
    statement_list
;

statement_list :
    statement
    | statement_list statement
;

statement : mail
    | set_statement
    | send_statement
    | schedule_statement
;

set_statement : tSET tIDENT tLPR tSTRING tRPR
;

mail :
    tMAIL tFROM tADDRESS tCOLON statement_list tENDMAIL
    | tMAIL tFROM tADDRESS tCOLON tENDMAIL
;

send_statement :
    tSEND tLBR tSTRING tRBR tTO recipient_list
    | tSEND tLBR tIDENT tRBR tTO recipient_list
;

recipient_list :
    tLBR recipient_list_objects tRBR
;

recipient_list_objects :
    recipient_object
    | recipient_list_objects tCOMMA recipient_object
;

recipient_object :
    tLPR tADDRESS tRPR
    | tLPR tSTRING tCOMMA tADDRESS tRPR
    | tLPR tIDENT tCOMMA tADDRESS tRPR
;

schedule_statement :
    tSCHEDULE tAT tLBR tDATE tCOMMA tTIME tRBR tCOLON send_statements tENDSCHEDULE
;

send_statements :
    send_statement
    | send_statements send_statement
;

%%

int main ()
{
    if (yyparse())
    {
        // parse error
        printf("ERROR\n");
        return 1;
    }
    else
    {
        // successful parsing
        printf("OK\n");

        return 0;
    }
}