%lex

str     \"([^\"]|(\\\"))*\"
float   [-]?[0-9]+\.[0-9]+([E]|[e][+-]?[0-9]+)?
int     [-]?[0-9]+
ident   [a-zA-Z_][a-zA-Z_]*
op      [{}\[\]=*+?^,:|()]

%%

{str}                             {return 'string';}
{float}                           {return 'float';}
{int}                             {return 'integer';}
("true")                          {return 'boolean';}
("false")                         {return 'boolean';}
"null"                            {return 'null';}
("number")                        {return 'ground_type';}
("string")                        {return 'ground_type';}
("boolean")                       {return 'ground_type';}
"_"                               {return 'discard';}
{ident}                           {return 'identifier';}
{op}                              {return yytext;}
[\v\t\s\r\l]                      {/* whitespace */}

/lex

%start TOP

%left '|'

%%

TOP : PATTERN { return $1; };

PATTERN
        : VALUE { $$ = $1; }
        | discard { $$ = yy.discard(); }
        | ground_type { $$ = yy.ground(yytext); }
        | INTERLEAVE { $$ = $1; }
        | OBJECT { $$ = $1; }
        | PATTERN '|' PATTERN { $$ = $1.or($3); }
        | '(' PATTERN ')' { $$ = $2; }
        ;

VALUE
        : integer { $$ = yy.literal(new Number(yytext).valueOf()); }
        | float { $$ = yy.literal(new Number(yytext).valueOf()); }
        | string { $$ = yy.literal(yytext.slice(1, -1)); }
        | boolean { $$ = yy.literal(yytext === "true"); }
        | null { $$ = yy.literal(null); }
        ;

INTERLEAVE
        : ARRAY '^' ARRAY { $$ = yy.interleave($1, $3); }
        | ARRAY { $$ = $1; }
        ;

ARRAY
        : '[' ']' { $$ = yy.sequence([]); }
        | '[' ARRAY_PAT ']' { $$ = yy.sequence($2); }
        ;

ARRAY_PAT
        : ARRAY_VAL { $$ = [$1]; }
        | ARRAY_PAT ',' ARRAY_VAL { $$ = $1; $1.push($3); }
        ;

ARRAY_VAL
        : PATTERN { $$ = $1; }
        | PATTERN '*' { $$ = yy.star($1); }
        | PATTERN '+' { $$ = yy.plus($1); }
        ;

OBJECT
        : '{' '}' { $$ = yy.object({}); }
        | '{' discard '}' { $$ = yy.object({}).etc(); }
        | '{' OBJECT_PAT '}' { $$ = yy.object($2); }
        | '{' OBJECT_PAT ',' discard '}' { $$ = yy.object($2).etc(); }
        ;

OBJECT_PAT
        : OBJECT_VAL { $$ = {}; $$[$1[0]] = $1[1]; }
        | OBJECT_PAT ',' OBJECT_VAL { $$ = $1; $1[$3[0]] = $3[1]; }
        ;

OBJECT_VAL
        : string ':' PATTERN { $$ = [$1.slice(1, -1), $3]; }
        ;
