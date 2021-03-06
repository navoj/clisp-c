; -*- Mode: Lisp; Package: CPARSE; Base: 10 -*-
; File: ZCTOKEN-PARSED.LISP
;
;   This code has been placed in the public domain.
;
(ZETA-C:ZCENV>CLEAR-FILE-DECLARATIONS '|zctoken.c|)
(DECL+ (|zctoken.c| 43.)
       (|unsigned| |short|)
       (= ([] |ctype| 257.)
          (LIST+ 0. 16. 16. 16. 16. 16. 16. 16. 16. 16. 16. 16. 16. 16. 16. 16. 16. 16. 16.
           16. 16. 16. 16. 16. 16. 16. 16. 16. 16. 16. 16. 16. 16. 8. 16. 16. 16. 144. 16. 16.
           16. 16. 16. 16. 16. 16. 16. 16. 16. 260. 260. 260. 260. 260. 260. 260. 260. 4. 4.
           16. 16. 16. 16. 16. 16. 16. 65. 65. 65. 65. 65. 65. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1.
           1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 16. 16. 16. 16. 144. 16. 66. 66. 66. 66. 66. 66. 2.
           2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 2. 16. 16. 16. 16. 32. 32.
           32. 32. 32. 32. 32. 32. 32. 8. 8. 8. 32. 8. 8. 32. 32. 32. 32. 32. 32. 32. 32. 32.
           32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32.
           32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32.
           32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32.
           32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32.
           32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32. 32.
           32.)))
(ZETA-C:ZCENV>#DEFINE 'LOW '(ZETA-C:NO-PARAMS . " 257  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'NUMBER '(ZETA-C:NO-PARAMS . " 258  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'CHARCONST '(ZETA-C:NO-PARAMS . " 259  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'STRING '(ZETA-C:NO-PARAMS . " 260  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'STORAGECLASS '(ZETA-C:NO-PARAMS . " 261  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'TYPE '(ZETA-C:NO-PARAMS . " 262  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE '|TYPE_ADJECTIVE| '(ZETA-C:NO-PARAMS . " 263  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE '|TYPEDEF_TYPE| '(ZETA-C:NO-PARAMS . " 264  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'ELLIPSIS '(ZETA-C:NO-PARAMS . " 265  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'STRUCT '(ZETA-C:NO-PARAMS . " 266  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'ENUM '(ZETA-C:NO-PARAMS . " 267  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'IF '(ZETA-C:NO-PARAMS . " 268  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'ELSE '(ZETA-C:NO-PARAMS . " 269  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'WHILE '(ZETA-C:NO-PARAMS . " 270  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'DO '(ZETA-C:NO-PARAMS . " 271  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'FOR '(ZETA-C:NO-PARAMS . " 272  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'SWITCH '(ZETA-C:NO-PARAMS . " 273  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'CASE '(ZETA-C:NO-PARAMS . " 274  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'BREAK '(ZETA-C:NO-PARAMS . " 275  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'CONTINUE '(ZETA-C:NO-PARAMS . " 276  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'RETURN '(ZETA-C:NO-PARAMS . " 277  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'GOTO '(ZETA-C:NO-PARAMS . " 278  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'DEFAULT '(ZETA-C:NO-PARAMS . " 279  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'SYMBOL '(ZETA-C:NO-PARAMS . " 280  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'SALLOC '(ZETA-C:NO-PARAMS . " 281  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'AALLOC '(ZETA-C:NO-PARAMS . " 282  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'SEMI '(ZETA-C:NO-PARAMS . " 283  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'COMMA '(ZETA-C:NO-PARAMS . " 284  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'ASSIGN '(ZETA-C:NO-PARAMS . " 285  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE '|OP_ASSIGN| '(ZETA-C:NO-PARAMS . " 286  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'QMARK '(ZETA-C:NO-PARAMS . " 287  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'COLON '(ZETA-C:NO-PARAMS . " 288  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'OR '(ZETA-C:NO-PARAMS . " 289  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'AND '(ZETA-C:NO-PARAMS . " 290  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE '|BIT_OR| '(ZETA-C:NO-PARAMS . " 291  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE '|BIT_XOR| '(ZETA-C:NO-PARAMS . " 292  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE '|AND_ADDRESS| '(ZETA-C:NO-PARAMS . " 293  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'EQUALITY '(ZETA-C:NO-PARAMS . " 294  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'COMPARISON '(ZETA-C:NO-PARAMS . " 295  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'SHIFT '(ZETA-C:NO-PARAMS . " 296  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'PLUSMINUS '(ZETA-C:NO-PARAMS . " 297  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE '|MUL_PTR| '(ZETA-C:NO-PARAMS . " 298  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'DIVMOD '(ZETA-C:NO-PARAMS . " 299  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'UNARY '(ZETA-C:NO-PARAMS . " 300  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'INCREMENT '(ZETA-C:NO-PARAMS . " 301  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'NOT '(ZETA-C:NO-PARAMS . " 302  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE '|BIT_NOT| '(ZETA-C:NO-PARAMS . " 303  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'SIZEOF '(ZETA-C:NO-PARAMS . " 304  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'LBRACKET '(ZETA-C:NO-PARAMS . " 305  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'RBRACKET '(ZETA-C:NO-PARAMS . " 306  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'LPAREN '(ZETA-C:NO-PARAMS . " 307  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'RPAREN '(ZETA-C:NO-PARAMS . " 308  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'LBRACE '(ZETA-C:NO-PARAMS . " 309  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'RBRACE '(ZETA-C:NO-PARAMS . " 310  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'ELEMENT '(ZETA-C:NO-PARAMS . " 311  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'HIGH '(ZETA-C:NO-PARAMS . " 312  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'SHARPIF '(ZETA-C:NO-PARAMS . " 313  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'DEFINED '(ZETA-C:NO-PARAMS . " 314  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE '|LISP_INCLUSION| '(ZETA-C:NO-PARAMS . " 315  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'MACTOKSTR '(ZETA-C:NO-PARAMS . " 316  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'MACTOKCAT '(ZETA-C:NO-PARAMS . " 317  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE 'ENDOFSTREAM '(ZETA-C:NO-PARAMS . " 318  ") '(|zctoken.c| 11.))
(ZETA-C:ZCENV>#DEFINE '_U '(ZETA-C:NO-PARAMS . " 0001  ") '(|zctoken.c| 15.))
(ZETA-C:ZCENV>#DEFINE '_L '(ZETA-C:NO-PARAMS . " 0002  ") '(|zctoken.c| 16.))
(ZETA-C:ZCENV>#DEFINE '_N '(ZETA-C:NO-PARAMS . " 0004  ") '(|zctoken.c| 17.))
(ZETA-C:ZCENV>#DEFINE '_S '(ZETA-C:NO-PARAMS . " 0010  ") '(|zctoken.c| 18.))
(ZETA-C:ZCENV>#DEFINE '_P '(ZETA-C:NO-PARAMS . " 0020  ") '(|zctoken.c| 19.))
(ZETA-C:ZCENV>#DEFINE '_C '(ZETA-C:NO-PARAMS . " 0040  ") '(|zctoken.c| 20.))
(ZETA-C:ZCENV>#DEFINE '_X '(ZETA-C:NO-PARAMS . " 0100  ") '(|zctoken.c| 21.))
(ZETA-C:ZCENV>#DEFINE '_SYM '(ZETA-C:NO-PARAMS . " 0200    ") '(|zctoken.c| 22.))
(ZETA-C:ZCENV>#DEFINE '_O '(ZETA-C:NO-PARAMS . " 0400    ") '(|zctoken.c| 23.))
(ZETA-C:ZCENV>#DEFINE '|isalpha| '(("c") . " ((ctype+1)[c]&(_U|_L))  ") '(|zctoken.c| 25.))
(ZETA-C:ZCENV>#DEFINE '|isupper| '(("c") . " ((ctype+1)[c]&_U)  ") '(|zctoken.c| 26.))
(ZETA-C:ZCENV>#DEFINE '|islower| '(("c") . " ((ctype+1)[c]&_L)  ") '(|zctoken.c| 27.))
(ZETA-C:ZCENV>#DEFINE '|isdigit| '(("c") . " ((ctype+1)[c]&_N)  ") '(|zctoken.c| 28.))
(ZETA-C:ZCENV>#DEFINE '|isxdigit| '(("c") . " ((ctype+1)[c]&(_N|_X))  ") '(|zctoken.c| 29.))
(ZETA-C:ZCENV>#DEFINE '|isodigit| '(("c") . " ((ctype+1)[c]&_O)  ") '(|zctoken.c| 30.))
(ZETA-C:ZCENV>#DEFINE '|isspace| '(("c") . " ((ctype+1)[c]&_S)  ") '(|zctoken.c| 31.))
(ZETA-C:ZCENV>#DEFINE '|ispunct| '(("c") . " ((ctype+1)[c]&_P)  ") '(|zctoken.c| 32.))
(ZETA-C:ZCENV>#DEFINE '|isalnum| '(("c") . " ((ctype+1)[c]&(_U|_L|_N))  ") '(|zctoken.c| 33.))
(ZETA-C:ZCENV>#DEFINE '|issym|
                      '(("c") . " ((ctype+1)[c]&(_U|_L|_N|_SYM))  ")
                      '(|zctoken.c| 34.))
(ZETA-C:ZCENV>#DEFINE '|isprint|
                      '(("c") . " ((ctype+1)[c]&(_P|_U|_L|_N))  ")
                      '(|zctoken.c| 35.))
(ZETA-C:ZCENV>#DEFINE '|iscntrl| '(("c") . " ((ctype+1)[c]&_C)  ") '(|zctoken.c| 36.))
(ZETA-C:ZCENV>#DEFINE '|isascii| '(("c") . " ((unsigned)(c)<=0177)  ") '(|zctoken.c| 37.))
(ZETA-C:ZCENV>#DEFINE '|toascii| '(("c") . " ((c)&0177)  ") '(|zctoken.c| 38.))
(ZETA-C:ZCENV>#DEFINE '|_toupper| '(("c") . " ((c)-'a'+'A')  ") '(|zctoken.c| 39.))
(ZETA-C:ZCENV>#DEFINE '|_tolower| '(("c") . " ((c)-'A'+'a')  ") '(|zctoken.c| 40.))
(DECL+ (|zctoken.c| 119.)
       (|char|)
       (= ([] |dispatch| 256.)
          (LIST+ 127. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0.
           0. 0. 0. 0. 0. 0. 0. 6. 4. 5. 4. 1. 4. 4. 5. 3. 3. 4. 4. 3. 4. 8. 7. 2. 2. 2. 2. 2.
           2. 2. 2. 2. 2. 3. 3. 4. 4. 4. 3. 9. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1.
           1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 3. 0. 3. 4. 1. 0. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1.
           1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 1. 3. 4. 3. 3. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0.
           6. 0. 0. 6. 6. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0.
           0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0.
           0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0.
           0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0.
           0. 0. 0. 0. 0. 0. 0.)))
(ZETA-C:ZCENV>#DEFINE 'DALPHA '(ZETA-C:NO-PARAMS . " 1    ") '(|zctoken.c| 102.))
(ZETA-C:ZCENV>#DEFINE 'DDIGIT '(ZETA-C:NO-PARAMS . " 2    ") '(|zctoken.c| 103.))
(ZETA-C:ZCENV>#DEFINE 'DOPER '(ZETA-C:NO-PARAMS . " 3    ") '(|zctoken.c| 104.))
(ZETA-C:ZCENV>#DEFINE 'DDOPER '(ZETA-C:NO-PARAMS . " 4    ") '(|zctoken.c| 105.))
(ZETA-C:ZCENV>#DEFINE 'DQUOTE '(ZETA-C:NO-PARAMS . " 5    ") '(|zctoken.c| 106.))
(ZETA-C:ZCENV>#DEFINE 'DWHITE '(ZETA-C:NO-PARAMS . " 6    ") '(|zctoken.c| 107.))
(ZETA-C:ZCENV>#DEFINE 'DSLASH '(ZETA-C:NO-PARAMS . " 7    ") '(|zctoken.c| 108.))
(ZETA-C:ZCENV>#DEFINE 'DDOT '(ZETA-C:NO-PARAMS . " 8    ") '(|zctoken.c| 109.))
(ZETA-C:ZCENV>#DEFINE 'DATSIGN '(ZETA-C:NO-PARAMS . " 9    ") '(|zctoken.c| 110.))
(ZETA-C:ZCENV>#DEFINE 'DEOF '(ZETA-C:NO-PARAMS . " 127  ") '(|zctoken.c| 111.))
(DECL+ (|zctoken.c| 145.)
       (|unsigned| |short|)
       (= ([] |single_lex| 128.)
          (LIST+ 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0.
           0. 0. 0. 0. 0. 0. 0. 302. 0. 316. 0. 299. 293. 0. 307. 308. 298. 297. 284. 297.
           311. 299. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 288. 283. 295. 285. 295. 287. 0. 0. 0. 0.
           0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 305. 0. 306.
           292. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0.
           0. 0. 309. 291. 310. 303. 0.)))
(DECL+ (|zctoken.c| 162.)
       (|lispval|)
       (= ([] |single_lval| 128.)
          (LIST+
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             '!)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             '%)
           (|#LISP| ((|lispval|))
             '&)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             '*)
           (|#LISP| ((|lispval|))
             '+)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             '-)
           (|#LISP| ((|lispval|))
             '|.|)
           (|#LISP| ((|lispval|))
             '/)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             '<)
           (|#LISP| ((|lispval|))
             '=)
           (|#LISP| ((|lispval|))
             '>)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             '^)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             '\|)
           (|#LISP| ((|lispval|))
             'CL:NIL)
           (|#LISP| ((|lispval|))
             '~)
           (|#LISP| ((|lispval|))
             'CL:NIL))))
(DECL+ (|zctoken.c| 182.)
       (|unsigned| |short|)
       (= ([] |double_lex| 128.)
          (LIST+ 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0.
           0. 0. 0. 0. 0. 0. 0. 0. 0. 317. 0. 0. 290. 0. 0. 0. 0. 301. 0. 301. 0. 0. 0. 0. 0.
           0. 0. 0. 0. 0. 0. 0. 0. 0. 296. 294. 296. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0.
           0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0.
           0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 289. 0. 0. 0.)))
(DECL+ (|zctoken.c| 199.)
       (|lispval|)
       (= ([] |double_lval| 128.)
          (LIST+
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             '&&)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             '++)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             '--)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             '<<)
           (|#LISP| ((|lispval|))
             '==)
           (|#LISP| ((|lispval|))
             '>>)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             '\|\|)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL))))
(DECL+ (|zctoken.c| 219.)
       (|unsigned| |short|)
       (= ([] |assign_lex| 128.)
          (LIST+ 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0.
           0. 0. 0. 0. 0. 0. 0. 294. 0. 0. 0. 286. 286. 0. 0. 0. 286. 286. 0. 286. 0. 286. 0.
           0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 295. 0. 295. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0.
           0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 286. 0. 0. 0. 0. 0. 0. 0.
           0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 286. 0. 0. 0.)))
(DECL+ (|zctoken.c| 236.)
       (|lispval|)
       (= ([] |assign_lval| 128.)
          (LIST+
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             '!=)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             '%=)
           (|#LISP| ((|lispval|))
             '&=)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             '*=)
           (|#LISP| ((|lispval|))
             '+=)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             '-=)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             '/=)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             '<=)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             '>=)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             '^=)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             '\|=)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL)
           (|#LISP| ((|lispval|))
             CL:NIL))))
(DECL+ (|zctoken.c| 260.) (|typedef| |char|) FLAG)
(ZETA-C:ZCENV>#DEFINE 'EOF '(ZETA-C:NO-PARAMS . " 0    ") '(|zctoken.c| 255.))
(ZETA-C:ZCENV>#DEFINE 'NUL '(ZETA-C:NO-PARAMS . " '\\0'  ") '(|zctoken.c| 256.))
(ZETA-C:ZCENV>#DEFINE 'TRUE '(ZETA-C:NO-PARAMS . " ((char) 1)  ") '(|zctoken.c| 257.))
(ZETA-C:ZCENV>#DEFINE 'FALSE '(ZETA-C:NO-PARAMS . " ((char) 0)  ") '(|zctoken.c| 258.))
(ZETA-C:ZCENV>#DEFINE 'NULL '(ZETA-C:NO-PARAMS . " ((char *) 0)  ") '(|zctoken.c| 259.))
(DECL+ (|zctoken.c| 287.)
       ((|struct| |lexerstate| ((|char|) |curchar|) ((|char|) (* |bufptr|))
         ((|lispval|) |yylval|) ((|lispval|) (FCN+ (* |cpstream|))) ((|char|) (* |tokbuf|))
         ((|char|) (* |savbuf|) (* |savbufp|)) ((|char|) (* |lastbufptr|))
         ((|lispval|) |ifstack|) ((|int|) |ifstate|) ((|int|) |macrostate|)
         ((|lispval|) |macro| |macparams| |macargs|) ((FLAG) |macargstr| |mactokcat|)
         ((|char|) (* |macexpstart|)))))
(ZETA-C:ZCENV>#DEFINE 'ENDIF '(ZETA-C:NO-PARAMS . " 16384  ") '(|zctoken.c| 261.))
(ZETA-C:ZCENV>#DEFINE 'TOKBUFSIZE '(ZETA-C:NO-PARAMS . " 4096  ") '(|zctoken.c| 263.))
(ZETA-C:ZCENV>#DEFINE 'SAVBUFSIZE '(ZETA-C:NO-PARAMS . " 6144  ") '(|zctoken.c| 264.))
(ZETA-C:ZCENV>#DEFINE '|car| '(("l") . " GL$CAR(l)  ") '(|zctoken.c| 267.))
(ZETA-C:ZCENV>#DEFINE '|cdr| '(("l") . " GL$CDR(l)  ") '(|zctoken.c| 268.))
(ZETA-C:ZCENV>#DEFINE '|cadr| '(("l") . " GL$CADR(l)  ") '(|zctoken.c| 269.))
(ZETA-C:ZCENV>#DEFINE '|cdar| '(("l") . " GL$CDAR(l)  ") '(|zctoken.c| 270.))
(ZETA-C:ZCENV>#DEFINE '|caar| '(("l") . " GL$CAAR(l)  ") '(|zctoken.c| 271.))
(ZETA-C:ZCENV>#DEFINE '|cddr| '(("l") . " GL$CDDR(l)  ") '(|zctoken.c| 272.))
(ZETA-C:ZCENV>#DEFINE '|cadar| '(("l") . " GL$CADAR(l)  ") '(|zctoken.c| 273.))
(ZETA-C:ZCENV>#DEFINE '|caddr| '(("l") . " GL$CADDR(l)  ") '(|zctoken.c| 274.))
(ZETA-C:ZCENV>#DEFINE '|cdddr| '(("l") . " GL$CDDDR(l)  ") '(|zctoken.c| 275.))
(ZETA-C:ZCENV>#DEFINE '|cadddr| '(("l") . " GL$CADDDR(l)  ") '(|zctoken.c| 276.))
(ZETA-C:ZCENV>#DEFINE '|push| '(("x" "l") . " GL$PUSH(x,l)  ") '(|zctoken.c| 277.))
(ZETA-C:ZCENV>#DEFINE '|pop| '(("l") . " GL$POP(l)  ") '(|zctoken.c| 278.))
(ZETA-C:ZCENV>#DEFINE '|cons| '(("a" "d") . " GL$CONS(a,d)  ") '(|zctoken.c| 279.))
(ZETA-C:ZCENV>#DEFINE '|rplaca| '(("l" "x") . " GL$RPLACA(l,x)  ") '(|zctoken.c| 280.))
(ZETA-C:ZCENV>#DEFINE '|rplacd| '(("l" "x") . " GL$RPLACD(l,x)  ") '(|zctoken.c| 281.))
(ZETA-C:ZCENV>#DEFINE '|nreverse| '(("l") . " GL$NREVERSE(l)  ") '(|zctoken.c| 282.))
(ZETA-C:ZCENV>#DEFINE '|list| '(ZETA-C:NO-PARAMS . " GL$LIST  ") '(|zctoken.c| 283.))
(DECL+ (|zctoken.c| 320.) ((|struct| |lexerstate|)) (* (FCN+ |LNew|)))
(ZETA-C:ZCENV>#DEFINE 'IFNONE '(ZETA-C:NO-PARAMS . " 0    ") '(|zctoken.c| 307.))
(ZETA-C:ZCENV>#DEFINE 'IFSTART '(ZETA-C:NO-PARAMS . " 1    ") '(|zctoken.c| 308.))
(ZETA-C:ZCENV>#DEFINE 'IFPARSE '(ZETA-C:NO-PARAMS . " 2    ") '(|zctoken.c| 309.))
(ZETA-C:ZCENV>#DEFINE 'IFDEFINED '(ZETA-C:NO-PARAMS . " 3    ") '(|zctoken.c| 310.))
(ZETA-C:ZCENV>#DEFINE 'MNONE '(ZETA-C:NO-PARAMS . " 0    ") '(|zctoken.c| 313.))
(ZETA-C:ZCENV>#DEFINE 'MDEFINING '(ZETA-C:NO-PARAMS . " 1    ") '(|zctoken.c| 314.))
(ZETA-C:ZCENV>#DEFINE 'MARGGING '(ZETA-C:NO-PARAMS . " 2    ") '(|zctoken.c| 315.))
(ZETA-C:ZCENV>#DEFINE 'MEXPANDING '(ZETA-C:NO-PARAMS . " 3    ") '(|zctoken.c| 316.))
(DECL+ (|zctoken.c| 321.) (|void|) (FCN+ |LReset|))
(DECL+ (|zctoken.c| 322.) (|lispval|) (FCN+ |Lyyval|))
(DECL+ (|zctoken.c| 323.) (|char|) (FCN+ |LTyi|))
(DECL+ (|zctoken.c| 324.) (|int|) (FCN+ |LCurLine|))
(DECL+ (|zctoken.c| 325.) (FLAG) (FCN+ |LCommentsNest|))
(DECL+ (|zctoken.c| 326.) (|int|) (FCN+ |LToken|))
(DECL+ (|zctoken.c| 327.) (FLAG) (FCN+ |LComment|))
(DECL+ (|zctoken.c| 328.) (|int|) (FCN+ |LSymbol|) (FCN+ |LSymbol1|))
(DECL+ (|zctoken.c| 329.)
       (|lispval|)
       (FCN+ |LInt|)
       (FCN+ |LFloat|)
       (FCN+ |LString|)
       (FCN+ |LChar|))
(DECL+ (|zctoken.c| 330.) (|int|) (FCN+ |LPreProc|))
(DECL+ (|zctoken.c| 331.)
       (|void|)
       (FCN+ |LDefine|)
       (FCN+ |LUndef|)
       (FCN+ |LIfdef|)
       (FCN+ |LElse|)
       (FCN+ |LEndif|)
       (FCN+ |LInclude|))
(DECL+ (|zctoken.c| 332.) (|int|) (FCN+ |LLisp|))
(DECL+ (|zctoken.c| 333.) (|lispval|) (FCN+ |LCondP|) (FCN+ |LCondCheck|) (FCN+ |LGetSym|))
(DECL+ (|zctoken.c| 334.)
       (|void|)
       (FCN+ |LSkipToEOL|)
       (FCN+ |LMacArgs|)
       (FCN+ |LMacExpand|)
       (FCN+ |LMacroSymbol|))
(DECL+ (|zctoken.c| 335.) (|lispval|) (FCN+ |LMacArg|))
(DECL+ (|zctoken.c| 336.) (|void|) (FCN+ |LScanExpansion|) (FCN+ |LSaveText|))
(DECL+ (|zctoken.c| 337.) (|char|) (FCN+ |LNextLine|))
(DECL+ (|zctoken.c| 338.) (|void|) (FCN+ |LSetBuf|))
(DECL+ (|zctoken.c| 339.) (|int|) (FCN+ |LBufIndex|))
(DECL+ (|zctoken.c| 342.) (|static| |char|) (* (FCN+ |strcpy|)) (* (FCN+ |memcpy|)))
(DECL+ (|zctoken.c| 343.) (|static| |int|) (FCN+ |strlen|) (FCN+ |strcmp|) (FCN+ |strncmp|))
(DECL+ (|zctoken.c| 344.) (|static| |lispval|) (FCN+ |str2lisp|))
(DECL+ (|zctoken.c| 345.) (|static| |int|) (FCN+ |toupper|) (FCN+ |tolower|))
(DEFUNC+ (|zctoken.c| 349.)
         (((|struct| |lexerstate|)) (* (FCN+ |LNew|)))
         CL:NIL
         (BLOCK+ ((((|struct| |lexerstate|)) (* |lstate|)) ((|char|) (* |tokbuf|))
                  ((|int|) (= |tokbufsize| 4096.)))
                 (= |lstate|
                    (CAST+ (((|struct| |lexerstate|)) (* CL:NIL))
                           (|calloc| 1. (|sizeof| (((|struct| |lexerstate|)) CL:NIL) CL:NIL))))
                 (= (-> |lstate| |tokbuf|)
                    (= |tokbuf| (CAST+ ((|char|) (* CL:NIL)) (|calloc| 4096. 1.))))
                 (|#LISP| CL:NIL
                   (GL:SETF (ZETA-C:ZCPRIM>ARRAY-FREELIST-LINK |tokbuf.array|)
                            (GL:MAKE-ARRAY |tokbufsize|
                                           :TYPE
                                           SYS:ART-STRING
                                           :FILL-POINTER
                                           0.
                                           :DISPLACED-TO
                                           |tokbuf.array|
                                           :DISPLACED-INDEX-OFFSET
                                           0.)))
                 (= (-> |lstate| |savbuf|) (CAST+ ((|char|) (* CL:NIL)) (|calloc| 6144. 1.)))
                 (|return| |lstate|)))
(DEFUNC+ (|zctoken.c| 374.)
         ((|lispval|) (FCN+ |LLispTok| |tokstart| |tokend|))
         (((|char|) (* |tokstart|) (* |tokend|)))
         (BLOCK+ (((|lispval|) |lisptok|) ((|int|) (= |length| (- |tokend| |tokstart|))))
                 (= |lisptok|
                    (|#LISP| ((|lispval|))
                      (ZETA-C:ZCPRIM>ARRAY-FREELIST-LINK |tokstart.array|)))
                 (|#LISP| CL:NIL
                   (GL:SETF (SYS:ARRAY-INDEX-OFFSET-FIELD |lisptok|) |tokstart.index|)
                   (GL:SETF (CL:FILL-POINTER |lisptok|) |length|))
                 (|return| |lisptok|)))
(DEFUNC+ (|zctoken.c| 393.)
         (CL:NIL (FCN+ |LInit| |lstate| |cpstream|))
         ((((|struct| |lexerstate|)) (* |lstate|)) ((|lispval|) (FCN+ (* |cpstream|))))
         (BLOCK+ CL:NIL
                 (= (-> |lstate| |bufptr|)
                    (= (-> |lstate| |savbufp|) (CAST+ ((|char|) (* CL:NIL)) 0.)))
                 (= (-> |lstate| |cpstream|) |cpstream|)
                 (= (-> |lstate| |curchar|) (QUOTE+ 0. :CHAR))
                 (= (-> |lstate| |ifstack|)
                    (|#LISP| ((|lispval|))
                      CL:NIL))
                 (= (-> |lstate| |ifstate|) 0.)
                 (= (-> |lstate| |macrostate|) 0.)
                 (= (-> |lstate| |macro|)
                    (|#LISP| ((|lispval|))
                      CL:NIL))))
(DEFUNC+ (|zctoken.c| 409.)
         ((|void|) (FCN+ |LReset| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ CL:NIL
                 (= (-> |lstate| |curchar|) (QUOTE+ 0. :CHAR))
                 (= (-> |lstate| |bufptr|)
                    (= (-> |lstate| |savbufp|) (CAST+ ((|char|) (* CL:NIL)) 0.)))
                 (= (-> |lstate| |ifstack|)
                    (|#LISP| ((|lispval|))
                      CL:NIL))
                 (= (-> |lstate| |ifstate|) 0.)
                 (= (-> |lstate| |macrostate|) 0.)
                 (= (-> |lstate| |macro|)
                    (|#LISP| ((|lispval|))
                      CL:NIL))))
(DEFUNC+ (|zctoken.c| 423.)
         ((|lispval|) (FCN+ |Lyylval| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ CL:NIL (|return| (-> |lstate| |yylval|))))
(DEFUNC+ (|zctoken.c| 457.)
         ((|char|) (FCN+ |LTyi| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ (((|char|) |c| (* |cptr|) |t|))
                 (= |c|
                    (|?:| (= |cptr| (-> |lstate| |bufptr|))
                          (|?:| (= |c| (-> |lstate| |curchar|))
                                |c|
                                (|LNextLine| |lstate| (& |cptr|)))
                          (PROGN+ (= |cptr| (STRING+ "")) (QUOTE+ 141. :CHAR))))
                 (= |t| |c|)
                 (|?:| (= |c| (* (X++ |cptr|))) |c| (= |c| (|LNextLine| |lstate| (& |cptr|))))
                 (PROGN+ (= (-> |lstate| |curchar|) |c|) (= (-> |lstate| |bufptr|) |cptr|))
                 (|return| |t|)))
(ZETA-C:ZCENV>#DEFINE 'LOADSTATE
                      '(ZETA-C:NO-PARAMS
                        . " ( c = (cptr = lstate->bufptr) ? ((c = lstate->curchar) ? c : LNextLine (lstate, &cptr)) : (cptr = \"\", '\\n') )  "
                        )
                      '(|zctoken.c| 440.))
(ZETA-C:ZCENV>#DEFINE 'SAVESTATE
                      '(ZETA-C:NO-PARAMS . " ( lstate->curchar = c, lstate->bufptr = cptr )  ")
                      '(|zctoken.c| 444.))
(ZETA-C:ZCENV>#DEFINE 'SKIP
                      '(ZETA-C:NO-PARAMS
                        . " ( (c = *cptr++) ? c : (c = LNextLine (lstate, &cptr)) )  ")
                      '(|zctoken.c| 446.))
(ZETA-C:ZCENV>#DEFINE 'HALFSKIP '(ZETA-C:NO-PARAMS . " ( c = *cptr++ )  ") '(|zctoken.c| 449.))
(ZETA-C:ZCENV>#DEFINE 'NEXT
                      '(ZETA-C:NO-PARAMS . " ( *token++ = c, SKIP )  ")
                      '(|zctoken.c| 451.))
(DEFUNC+ (|zctoken.c| 472.)
         ((|void|) (FCN+ |LUnTyi| |lstate| |unc|))
         ((((|struct| |lexerstate|)) (* |lstate|)) ((|char|) |unc|))
         (BLOCK+ (((|char|) |c| (* |cptr|)))
                 (= |c|
                    (|?:| (= |cptr| (-> |lstate| |bufptr|))
                          (|?:| (= |c| (-> |lstate| |curchar|))
                                |c|
                                (|LNextLine| |lstate| (& |cptr|)))
                          (PROGN+ (= |cptr| (STRING+ "")) (QUOTE+ 141. :CHAR))))
                 (--X |cptr|)
                 (= |c| |unc|)
                 (PROGN+ (= (-> |lstate| |curchar|) |c|) (= (-> |lstate| |bufptr|) |cptr|))))
(DEFUNC+ (|zctoken.c| 490.)
         ((|lispval|) (FCN+ |LCurFile| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ CL:NIL
                 (|return| ((-> |lstate| |cpstream|)
                            (|#LISP| ((|lispval|))
                              :CURRENT-FILE)))))
(DEFUNC+ (|zctoken.c| 499.)
         ((|int|) (FCN+ |LCurLine| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ CL:NIL
                 (|return| (CAST+ ((|int|) CL:NIL)
                                  ((-> |lstate| |cpstream|)
                                   (|#LISP| ((|lispval|))
                                     :CURRENT-LINE))))))
(DEFUNC+ (|zctoken.c| 508.)
         ((FLAG) (FCN+ |LCommentsNest| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ CL:NIL
                 (|return| (|?:| ((-> |lstate| |cpstream|)
                                  (|#LISP| ((|lispval|))
                                    :COMMENTS-NEST-P))
                                 (CAST+ ((|char|) CL:NIL) 1.)
                                 (CAST+ ((|char|) CL:NIL) 0.)))))
(DEFUNC+ (|zctoken.c| 518.)
         ((FLAG) (FCN+ |LAtEnd| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ CL:NIL (|return| (== (-> |lstate| |curchar|) (QUOTE+ 0. :CHAR)))))
(DEFUNC+ (|zctoken.c| 531.)
         ((|int|) (FCN+ |LToken| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ (((|char|) |c| (* |cptr|) (* |token|) |t| |ch| (* |tsavbufp|) (* |tcptr|))
                  ((|int|) |l| |i| (FCN+ GL:LENGTH))
                  ((FLAG) |painted| |floatp| |hexp| |shortp| |longp| |unsignedp|)
                  ((|lispval|) |skipping|)
                  ((|lispval|) (FCN+ |LFloat|) (FCN+ |LInt|) (FCN+ |LString|) (FCN+ |LChar|)))
                 (|if| (== (-> |lstate| |ifstate|) 1.)
                       (BLOCK+ CL:NIL (= (-> |lstate| |ifstate|) 2.) (|return| 313.)))
                 (= |c|
                    (|?:| (= |cptr| (-> |lstate| |bufptr|))
                          (|?:| (= |c| (-> |lstate| |curchar|))
                                |c|
                                (|LNextLine| |lstate| (& |cptr|)))
                          (PROGN+ (= |cptr| (STRING+ "")) (QUOTE+ 141. :CHAR))))
                 (|for| CL:NIL
                        CL:NIL
                        CL:NIL
                        (BLOCK+ CL:NIL
                                (= |floatp| (CAST+ ((|char|) CL:NIL) 0.))
                                (= |token| (-> |lstate| |tokbuf|))
                                (|switch| ([] |dispatch| |c|)
                                          (BLOCK+ CL:NIL
                                                  (|case| 127.)
                                                  (|if| (!= (-> |lstate| |macrostate|) 0.)
                                                        (BLOCK+
                                                         CL:NIL
                                                         (PROGN+
                                                          (= (-> |lstate| |curchar|) |c|)
                                                          (= (-> |lstate| |bufptr|) |cptr|))
                                                         (|return| 318.)))
                                                  (|for| CL:NIL
                                                         (&&
                                                          (-> |lstate| |ifstack|)
                                                          (==
                                                           (CL:CADDR
                                                            (CL:CAR (-> |lstate| |ifstack|)))
                                                           (|LCurFile| |lstate|)))
                                                         (=
                                                          (-> |lstate| |ifstack|)
                                                          (CL:CDR (-> |lstate| |ifstack|)))
                                                         (|LWarnNoCtx|
                                                          (|#LISP|
                                                           ((|lispval|))
                                                           "Missing #endif to ~A on line ~A")
                                                          (CL:CAR
                                                           (CL:CDAR (-> |lstate| |ifstack|)))
                                                          (CL:CADR
                                                           (CL:CDAR (-> |lstate| |ifstack|)))))
                                                  (|if| (!
                                                         ((-> |lstate| |cpstream|)
                                                          (|#LISP|
                                                           ((|lispval|))
                                                           :POP-INPUT-SOURCE)))
                                                        (BLOCK+
                                                         CL:NIL
                                                         (PROGN+
                                                          (= (-> |lstate| |curchar|) |c|)
                                                          (= (-> |lstate| |bufptr|) |cptr|))
                                                         (|return| 318.)))
                                                  (= |c|
                                                     (|?:| (= |cptr| (-> |lstate| |bufptr|))
                                                           (|?:|
                                                            (= |c| (-> |lstate| |curchar|))
                                                            |c|
                                                            (|LNextLine| |lstate| (& |cptr|)))
                                                           (PROGN+
                                                            (= |cptr| (STRING+ ""))
                                                            (QUOTE+ 141. :CHAR))))
                                                  (|break|)
                                                  (|case| 6.)
                                                  (|if| (&&
                                                         (== |c| (QUOTE+ 141. :CHAR))
                                                         (== (-> |lstate| |macrostate|) 1.))
                                                        (BLOCK+
                                                         CL:NIL
                                                         (PROGN+
                                                          (= (-> |lstate| |curchar|) |c|)
                                                          (= (-> |lstate| |bufptr|) |cptr|))
                                                         (|return| 318.)))
                                                  (|if| (&&
                                                         (== |c| (QUOTE+ 141. :CHAR))
                                                         (== (-> |lstate| |ifstate|) 2.))
                                                        (BLOCK+
                                                         CL:NIL
                                                         (PROGN+
                                                          (= (-> |lstate| |curchar|) |c|)
                                                          (= (-> |lstate| |bufptr|) |cptr|))
                                                         (|return| 313.)))
                                                  (= |t| |c|)
                                                  (|?:| (= |c| (* (X++ |cptr|)))
                                                        |c|
                                                        (=
                                                         |c|
                                                         (|LNextLine| |lstate| (& |cptr|))))
                                                  (|if| (== |t| (QUOTE+ 141. :CHAR))
                                                        (BLOCK+
                                                         CL:NIL
                                                         (=
                                                          (-> |lstate| |macexpstart|)
                                                          (- |cptr| 1.))
                                                         (|if|
                                                          (== |c| (QUOTE+ 35. :CHAR))
                                                          (BLOCK+
                                                           CL:NIL
                                                           (|?:|
                                                            (= |c| (* (X++ |cptr|)))
                                                            |c|
                                                            (=
                                                             |c|
                                                             (|LNextLine| |lstate| (& |cptr|)))
                                                            )
                                                           (PROGN+
                                                            (= (-> |lstate| |curchar|) |c|)
                                                            (= (-> |lstate| |bufptr|) |cptr|))
                                                           (|if|
                                                            (= |l| (|LPreProc| |lstate|))
                                                            (|return| |l|))
                                                           (=
                                                            |skipping|
                                                            (! (|LCondP| |lstate|)))
                                                           (=
                                                            |c|
                                                            (|?:|
                                                             (= |cptr| (-> |lstate| |bufptr|))
                                                             (|?:|
                                                              (= |c| (-> |lstate| |curchar|))
                                                              |c|
                                                              (|LNextLine| |lstate| (& |cptr|))
                                                              )
                                                             (PROGN+
                                                              (= |cptr| (STRING+ ""))
                                                              (QUOTE+ 141. :CHAR)))))))
                                                        (BLOCK+
                                                         CL:NIL
                                                         (PROGN+
                                                          (= (-> |lstate| |curchar|) |c|)
                                                          (= (-> |lstate| |bufptr|) |cptr|))
                                                         (|LSaveText| |lstate|)
                                                         (= |tsavbufp| (-> |lstate| |savbufp|))
                                                         (|while|
                                                          (\|\|
                                                           (== |c| (QUOTE+ 32. :CHAR))
                                                           (== |c| (QUOTE+ 137. :CHAR)))
                                                          (|?:|
                                                           (= |c| (* (X++ |cptr|)))
                                                           |c|
                                                           (=
                                                            |c|
                                                            (|LNextLine| |lstate| (& |cptr|))))
                                                          )
                                                         (|if|
                                                          |tsavbufp|
                                                          (BLOCK+
                                                           CL:NIL
                                                           (|if|
                                                            (&&
                                                             (== (-> |lstate| |macrostate|) 3.)
                                                             (-> |lstate| |mactokcat|))
                                                            (--X |tsavbufp|)
                                                            (=
                                                             ([] |tsavbufp| (- 1.))
                                                             (QUOTE+ 32. :CHAR)))
                                                           (=
                                                            (-> |lstate| |savbufp|)
                                                            |tsavbufp|)
                                                           (=
                                                            (-> |lstate| |lastbufptr|)
                                                            (- |cptr| 1.))))))
                                                  (|break|)
                                                  (|case| 1.)
                                                  (= |tcptr| (- |cptr| 1.))
                                                  (|while| (&
                                                            ([]
                                                             (+ |ctype| 1.)
                                                             (PROGN+
                                                              (= (* (X++ |token|)) |c|)
                                                              (|?:|
                                                               (= |c| (* (X++ |cptr|)))
                                                               |c|
                                                               (=
                                                                |c|
                                                                (|LNextLine|
                                                                 |lstate|
                                                                 (& |cptr|))))))
                                                            (\| (\| (\| 1. 2.) 4.) 128.))
                                                           CL:NIL)
                                                  (= (* |token|) (QUOTE+ 0. :CHAR))
                                                  (|if| (== |c| (QUOTE+ 5. :CHAR))
                                                        (BLOCK+
                                                         CL:NIL
                                                         (|?:|
                                                          (= |c| (* (X++ |cptr|)))
                                                          |c|
                                                          (=
                                                           |c|
                                                           (|LNextLine| |lstate| (& |cptr|))))
                                                         (=
                                                          |painted|
                                                          (CAST+ ((|char|) CL:NIL) 1.))
                                                         (PROGN+
                                                          (= (-> |lstate| |curchar|) |c|)
                                                          (= (-> |lstate| |bufptr|) |cptr|))
                                                         (|LSaveText| |lstate|)
                                                         (--X (-> |lstate| |savbufp|)))
                                                        (=
                                                         |painted|
                                                         (CAST+ ((|char|) CL:NIL) 0.)))
                                                  (|if| |skipping| (|break|))
                                                  (PROGN+ (= (-> |lstate| |curchar|) |c|)
                                                          (= (-> |lstate| |bufptr|) |cptr|))
                                                  (|if| (== (-> |lstate| |macrostate|) 0.)
                                                        (BLOCK+
                                                         CL:NIL
                                                         (|if|
                                                          (=
                                                           |l|
                                                           (|LSymbol|
                                                            |lstate|
                                                            |token|
                                                            |painted|))
                                                          (|return| |l|))
                                                         (=
                                                          (-> |lstate| |macexpstart|)
                                                          |tcptr|)
                                                         (|LMacExpand| |lstate|)
                                                         (=
                                                          |c|
                                                          (|?:|
                                                           (= |cptr| (-> |lstate| |bufptr|))
                                                           (|?:|
                                                            (= |c| (-> |lstate| |curchar|))
                                                            |c|
                                                            (|LNextLine| |lstate| (& |cptr|)))
                                                           (PROGN+
                                                            (= |cptr| (STRING+ ""))
                                                            (QUOTE+ 141. :CHAR)))))
                                                        (|if|
                                                         (== (-> |lstate| |macrostate|) 1.)
                                                         (BLOCK+
                                                          CL:NIL
                                                          (|LSymbol|
                                                           |lstate|
                                                           |token|
                                                           (CAST+ ((|char|) CL:NIL) 1.))
                                                          (|if|
                                                           (==
                                                            (-> |lstate| |macro|)
                                                            (-> |lstate| |yylval|))
                                                           (=
                                                            (* (X++ (-> |lstate| |savbufp|)))
                                                            (QUOTE+ 5. :CHAR))))
                                                         (|if|
                                                          (== (-> |lstate| |macrostate|) 3.)
                                                          (BLOCK+
                                                           CL:NIL
                                                           (|LMacroSymbol| |lstate|)
                                                           (=
                                                            |c|
                                                            (|?:|
                                                             (= |cptr| (-> |lstate| |bufptr|))
                                                             (|?:|
                                                              (= |c| (-> |lstate| |curchar|))
                                                              |c|
                                                              (|LNextLine| |lstate| (& |cptr|))
                                                              )
                                                             (PROGN+
                                                              (= |cptr| (STRING+ ""))
                                                              (QUOTE+ 141. :CHAR)))))
                                                          (|return| 280.))))
                                                  (|break|)
                                                  (|case| 4.)
                                                  (= |t| |c|)
                                                  (|?:| (= |c| (* (X++ |cptr|)))
                                                        |c|
                                                        (=
                                                         |c|
                                                         (|LNextLine| |lstate| (& |cptr|))))
                                                  (|if| (&&
                                                         (== |c| |t|)
                                                         (= |l| ([] |double_lex| |t|)))
                                                        (BLOCK+
                                                         CL:NIL
                                                         (|?:|
                                                          (= |c| (* (X++ |cptr|)))
                                                          |c|
                                                          (=
                                                           |c|
                                                           (|LNextLine| |lstate| (& |cptr|))))
                                                         (|if|
                                                          (&&
                                                           (== |l| 296.)
                                                           (== |c| (QUOTE+ 61. :CHAR)))
                                                          (BLOCK+
                                                           CL:NIL
                                                           (|?:|
                                                            (= |c| (* (X++ |cptr|)))
                                                            |c|
                                                            (=
                                                             |c|
                                                             (|LNextLine| |lstate| (& |cptr|)))
                                                            )
                                                           (|if| |skipping| (|break|))
                                                           (PROGN+
                                                            (= (-> |lstate| |curchar|) |c|)
                                                            (= (-> |lstate| |bufptr|) |cptr|))
                                                           (=
                                                            (-> |lstate| |yylval|)
                                                            (|?:|
                                                             (== |t| (QUOTE+ 60. :CHAR))
                                                             (|#LISP|
                                                              ((|lispval|))
                                                              '<<=)
                                                             (|#LISP|
                                                              ((|lispval|))
                                                              '>>=)))
                                                           (|return| 286.))
                                                          (BLOCK+
                                                           CL:NIL
                                                           (|if| |skipping| (|break|))
                                                           (PROGN+
                                                            (= (-> |lstate| |curchar|) |c|)
                                                            (= (-> |lstate| |bufptr|) |cptr|))
                                                           (=
                                                            (-> |lstate| |yylval|)
                                                            ([] |double_lval| |t|))
                                                           (|return| |l|))))
                                                        (|if|
                                                         (&&
                                                          (== |c| (QUOTE+ 61. :CHAR))
                                                          (= |l| ([] |assign_lex| |t|)))
                                                         (BLOCK+
                                                          CL:NIL
                                                          (|?:|
                                                           (= |c| (* (X++ |cptr|)))
                                                           |c|
                                                           (=
                                                            |c|
                                                            (|LNextLine| |lstate| (& |cptr|))))
                                                          (|if| |skipping| (|break|))
                                                          (PROGN+
                                                           (= (-> |lstate| |curchar|) |c|)
                                                           (= (-> |lstate| |bufptr|) |cptr|))
                                                          (=
                                                           (-> |lstate| |yylval|)
                                                           ([] |assign_lval| |t|))
                                                          (|return| |l|))
                                                         (|if|
                                                          (&&
                                                           (== |t| (QUOTE+ 45. :CHAR))
                                                           (== |c| (QUOTE+ 62. :CHAR)))
                                                          (BLOCK+
                                                           CL:NIL
                                                           (|?:|
                                                            (= |c| (* (X++ |cptr|)))
                                                            |c|
                                                            (=
                                                             |c|
                                                             (|LNextLine| |lstate| (& |cptr|)))
                                                            )
                                                           (|if| |skipping| (|break|))
                                                           (PROGN+
                                                            (= (-> |lstate| |curchar|) |c|)
                                                            (= (-> |lstate| |bufptr|) |cptr|))
                                                           (=
                                                            (-> |lstate| |yylval|)
                                                            (|#LISP|
                                                             ((|lispval|))
                                                             '->))
                                                           (|return| 311.))
                                                          (BLOCK+
                                                           CL:NIL
                                                           (|if| |skipping| (|break|))
                                                           (PROGN+
                                                            (= (-> |lstate| |curchar|) |c|)
                                                            (= (-> |lstate| |bufptr|) |cptr|))
                                                           (=
                                                            (-> |lstate| |yylval|)
                                                            ([] |single_lval| |t|))
                                                           (|return| ([] |single_lex| |t|))))))
                                                  (|break|)
                                                  (|case| 3.)
                                                  (= |t| |c|)
                                                  (|if| |skipping|
                                                        (BLOCK+
                                                         CL:NIL
                                                         (|?:|
                                                          (= |c| (* (X++ |cptr|)))
                                                          |c|
                                                          (=
                                                           |c|
                                                           (|LNextLine| |lstate| (& |cptr|))))
                                                         (|break|)))
                                                  (= |c| (* (X++ |cptr|)))
                                                  (PROGN+ (= (-> |lstate| |curchar|) |c|)
                                                          (= (-> |lstate| |bufptr|) |cptr|))
                                                  (= (-> |lstate| |yylval|)
                                                     ([] |single_lval| |t|))
                                                  (|return| ([] |single_lex| |t|))
                                                  (|case| 7.)
                                                  (PROGN+ (= (-> |lstate| |curchar|) |c|)
                                                          (= (-> |lstate| |bufptr|) |cptr|))
                                                  (|LSaveText| |lstate|)
                                                  (= |tsavbufp| (-> |lstate| |savbufp|))
                                                  (|?:| (= |c| (* (X++ |cptr|)))
                                                        |c|
                                                        (=
                                                         |c|
                                                         (|LNextLine| |lstate| (& |cptr|))))
                                                  (|if| (!= |c| (QUOTE+ 42. :CHAR))
                                                        (BLOCK+
                                                         CL:NIL
                                                         (|if|
                                                          (== |c| (QUOTE+ 61. :CHAR))
                                                          (BLOCK+
                                                           CL:NIL
                                                           (|?:|
                                                            (= |c| (* (X++ |cptr|)))
                                                            |c|
                                                            (=
                                                             |c|
                                                             (|LNextLine| |lstate| (& |cptr|)))
                                                            )
                                                           (|if| |skipping| (|break|))
                                                           (PROGN+
                                                            (= (-> |lstate| |curchar|) |c|)
                                                            (= (-> |lstate| |bufptr|) |cptr|))
                                                           (=
                                                            (-> |lstate| |yylval|)
                                                            (|#LISP|
                                                             ((|lispval|))
                                                             '/=))
                                                           (|return| 286.)))
                                                         (|if| |skipping| (|break|))
                                                         (PROGN+
                                                          (= (-> |lstate| |curchar|) |c|)
                                                          (= (-> |lstate| |bufptr|) |cptr|))
                                                         (=
                                                          (-> |lstate| |yylval|)
                                                          (|#LISP|
                                                           ((|lispval|))
                                                           '/))
                                                         (|return| 299.)))
                                                  (|?:| (= |c| (* (X++ |cptr|)))
                                                        |c|
                                                        (=
                                                         |c|
                                                         (|LNextLine| |lstate| (& |cptr|))))
                                                  (PROGN+ (= (-> |lstate| |curchar|) |c|)
                                                          (= (-> |lstate| |bufptr|) |cptr|))
                                                  (BLOCK+ (((|int|)
                                                            (= |nullp| (|LComment| |lstate|))))
                                                          (|if|
                                                           |tsavbufp|
                                                           (BLOCK+
                                                            CL:NIL
                                                            (=
                                                             (* (X++ |tsavbufp|))
                                                             (QUOTE+ 32. :CHAR))
                                                            (|if|
                                                             (&&
                                                              |nullp|
                                                              (==
                                                               (-> |lstate| |macrostate|)
                                                               1.))
                                                             (BLOCK+
                                                              CL:NIL
                                                              (|strcpy|
                                                               |tsavbufp|
                                                               (STRING+ "## "))
                                                              (+= |tsavbufp| 3.)))
                                                            (=
                                                             (-> |lstate| |savbufp|)
                                                             |tsavbufp|)
                                                            (=
                                                             (-> |lstate| |lastbufptr|)
                                                             (- (-> |lstate| |bufptr|) 1.)))))
                                                  (= |c|
                                                     (|?:| (= |cptr| (-> |lstate| |bufptr|))
                                                           (|?:|
                                                            (= |c| (-> |lstate| |curchar|))
                                                            |c|
                                                            (|LNextLine| |lstate| (& |cptr|)))
                                                           (PROGN+
                                                            (= |cptr| (STRING+ ""))
                                                            (QUOTE+ 141. :CHAR))))
                                                  (|break|)
                                                  (|case| 8.)
                                                  (|if| (==
                                                         (PROGN+
                                                          (= (* (X++ |token|)) |c|)
                                                          (|?:|
                                                           (= |c| (* (X++ |cptr|)))
                                                           |c|
                                                           (=
                                                            |c|
                                                            (|LNextLine| |lstate| (& |cptr|))))
                                                          )
                                                         (QUOTE+ 46. :CHAR))
                                                        (BLOCK+
                                                         CL:NIL
                                                         (|if|
                                                          (!=
                                                           (|?:|
                                                            (= |c| (* (X++ |cptr|)))
                                                            |c|
                                                            (=
                                                             |c|
                                                             (|LNextLine| |lstate| (& |cptr|)))
                                                            )
                                                           (QUOTE+ 46. :CHAR))
                                                          (BLOCK+
                                                           CL:NIL
                                                           (PROGN+
                                                            (= (-> |lstate| |curchar|) |c|)
                                                            (= (-> |lstate| |bufptr|) |cptr|))
                                                           (|LError|
                                                            (-> |lstate| |cpstream|)
                                                            (- 1.)
                                                            (|#LISP|
                                                             ((|lispval|))
                                                             "Illegal token `..'"))))
                                                         (|if| |skipping| (|break|))
                                                         (PROGN+
                                                          (= (-> |lstate| |curchar|) |c|)
                                                          (= (-> |lstate| |bufptr|) |cptr|))
                                                         (=
                                                          (-> |lstate| |yylval|)
                                                          (|#LISP|
                                                           ((|lispval|))
                                                           '|...|))
                                                         (|return| 265.))
                                                        (|if|
                                                         (! (& ([] (+ |ctype| 1.) |c|) 4.))
                                                         (BLOCK+
                                                          CL:NIL
                                                          (|if| |skipping| (|break|))
                                                          (PROGN+
                                                           (= (-> |lstate| |curchar|) |c|)
                                                           (= (-> |lstate| |bufptr|) |cptr|))
                                                          (=
                                                           (-> |lstate| |yylval|)
                                                           (|#LISP|
                                                            ((|lispval|))
                                                            '|.|))
                                                          (|return| 311.))))
                                                  (= |floatp| (CAST+ ((|char|) CL:NIL) 1.))
                                                  (|case| 2.)
                                                  (= |hexp|
                                                     (= |shortp|
                                                        (=
                                                         |longp|
                                                         (=
                                                          |unsignedp|
                                                          (CAST+ ((|char|) CL:NIL) 0.)))))
                                                  (|if| (&&
                                                         (&&
                                                          (! |floatp|)
                                                          (== |c| (QUOTE+ 48. :CHAR)))
                                                         (==
                                                          (|toupper|
                                                           (PROGN+
                                                            (= (* (X++ |token|)) |c|)
                                                            (|?:|
                                                             (= |c| (* (X++ |cptr|)))
                                                             |c|
                                                             (=
                                                              |c|
                                                              (|LNextLine| |lstate| (& |cptr|))
                                                              ))))
                                                          (QUOTE+ 88. :CHAR)))
                                                        (BLOCK+
                                                         CL:NIL
                                                         (|?:|
                                                          (= |c| (* (X++ |cptr|)))
                                                          |c|
                                                          (=
                                                           |c|
                                                           (|LNextLine| |lstate| (& |cptr|))))
                                                         (=
                                                          |hexp|
                                                          (CAST+ ((|char|) CL:NIL) 1.))
                                                         (|while|
                                                          (&
                                                           ([]
                                                            (+ |ctype| 1.)
                                                            (PROGN+
                                                             (= (* (X++ |token|)) |c|)
                                                             (|?:|
                                                              (= |c| (* (X++ |cptr|)))
                                                              |c|
                                                              (=
                                                               |c|
                                                               (|LNextLine|
                                                                |lstate|
                                                                (& |cptr|))))))
                                                           (\| 4. 64.))
                                                          CL:NIL))
                                                        (|while|
                                                         (& ([] (+ |ctype| 1.) |c|) 4.)
                                                         (PROGN+
                                                          (= (* (X++ |token|)) |c|)
                                                          (|?:|
                                                           (= |c| (* (X++ |cptr|)))
                                                           |c|
                                                           (=
                                                            |c|
                                                            (|LNextLine| |lstate| (& |cptr|))))
                                                          )))
                                                  (|if| (&&
                                                         (! |hexp|)
                                                         (== |c| (QUOTE+ 46. :CHAR)))
                                                        (BLOCK+
                                                         CL:NIL
                                                         (=
                                                          |floatp|
                                                          (CAST+ ((|char|) CL:NIL) 1.))
                                                         (|while|
                                                          (&
                                                           ([]
                                                            (+ |ctype| 1.)
                                                            (PROGN+
                                                             (= (* (X++ |token|)) |c|)
                                                             (|?:|
                                                              (= |c| (* (X++ |cptr|)))
                                                              |c|
                                                              (=
                                                               |c|
                                                               (|LNextLine|
                                                                |lstate|
                                                                (& |cptr|))))))
                                                           4.)
                                                          CL:NIL)))
                                                  (|if| (&&
                                                         (! |hexp|)
                                                         (==
                                                          (|toupper| |c|)
                                                          (QUOTE+ 69. :CHAR)))
                                                        (BLOCK+
                                                         CL:NIL
                                                         (=
                                                          |floatp|
                                                          (CAST+ ((|char|) CL:NIL) 1.))
                                                         (PROGN+
                                                          (= (* (X++ |token|)) |c|)
                                                          (|?:|
                                                           (= |c| (* (X++ |cptr|)))
                                                           |c|
                                                           (=
                                                            |c|
                                                            (|LNextLine| |lstate| (& |cptr|))))
                                                          )
                                                         (|if|
                                                          (\|\|
                                                           (== |c| (QUOTE+ 43. :CHAR))
                                                           (== |c| (QUOTE+ 45. :CHAR)))
                                                          (PROGN+
                                                           (= (* (X++ |token|)) |c|)
                                                           (|?:|
                                                            (= |c| (* (X++ |cptr|)))
                                                            |c|
                                                            (=
                                                             |c|
                                                             (|LNextLine| |lstate| (& |cptr|)))
                                                            )))
                                                         (|while|
                                                          (&
                                                           ([]
                                                            (+ |ctype| 1.)
                                                            (PROGN+
                                                             (= (* (X++ |token|)) |c|)
                                                             (|?:|
                                                              (= |c| (* (X++ |cptr|)))
                                                              |c|
                                                              (=
                                                               |c|
                                                               (|LNextLine|
                                                                |lstate|
                                                                (& |cptr|))))))
                                                           4.)
                                                          CL:NIL)))
                                                  (|if| |floatp|
                                                        (BLOCK+
                                                         CL:NIL
                                                         (|if|
                                                          (==
                                                           (|toupper| |c|)
                                                           (QUOTE+ 70. :CHAR))
                                                          (BLOCK+
                                                           CL:NIL
                                                           (|?:|
                                                            (= |c| (* (X++ |cptr|)))
                                                            |c|
                                                            (=
                                                             |c|
                                                             (|LNextLine| |lstate| (& |cptr|)))
                                                            )
                                                           (=
                                                            |shortp|
                                                            (CAST+ ((|char|) CL:NIL) 1.)))
                                                          (|if|
                                                           (==
                                                            (|toupper| |c|)
                                                            (QUOTE+ 76. :CHAR))
                                                           (BLOCK+
                                                            CL:NIL
                                                            (|?:|
                                                             (= |c| (* (X++ |cptr|)))
                                                             |c|
                                                             (=
                                                              |c|
                                                              (|LNextLine| |lstate| (& |cptr|))
                                                              ))
                                                            (=
                                                             |longp|
                                                             (CAST+ ((|char|) CL:NIL) 1.))))))
                                                        (|for|
                                                         CL:NIL
                                                         CL:NIL
                                                         CL:NIL
                                                         (BLOCK+
                                                          CL:NIL
                                                          (|if|
                                                           (==
                                                            (|toupper| |c|)
                                                            (QUOTE+ 85. :CHAR))
                                                           (BLOCK+
                                                            CL:NIL
                                                            (|?:|
                                                             (= |c| (* (X++ |cptr|)))
                                                             |c|
                                                             (=
                                                              |c|
                                                              (|LNextLine| |lstate| (& |cptr|))
                                                              ))
                                                            (=
                                                             |unsignedp|
                                                             (CAST+ ((|char|) CL:NIL) 1.)))
                                                           (|if|
                                                            (==
                                                             (|toupper| |c|)
                                                             (QUOTE+ 76. :CHAR))
                                                            (BLOCK+
                                                             CL:NIL
                                                             (|?:|
                                                              (= |c| (* (X++ |cptr|)))
                                                              |c|
                                                              (=
                                                               |c|
                                                               (|LNextLine|
                                                                |lstate|
                                                                (& |cptr|))))
                                                             (=
                                                              |longp|
                                                              (CAST+ ((|char|) CL:NIL) 1.)))
                                                            (|break|))))))
                                                  (|if| |skipping| (|break|))
                                                  (PROGN+ (= (-> |lstate| |curchar|) |c|)
                                                          (= (-> |lstate| |bufptr|) |cptr|))
                                                  (= (-> |lstate| |yylval|)
                                                     (|?:| |floatp|
                                                           (|LFloat|
                                                            (-> |lstate| |tokbuf|)
                                                            |token|
                                                            |shortp|
                                                            |longp|)
                                                           (|LInt|
                                                            |lstate|
                                                            (-> |lstate| |tokbuf|)
                                                            |token|
                                                            |hexp|
                                                            |unsignedp|
                                                            |longp|)))
                                                  (|return| 258.)
                                                  (|case| 5.)
                                                  (= |t| |c|)
                                                  (|?:| (= |c| (* (X++ |cptr|)))
                                                        |c|
                                                        (=
                                                         |c|
                                                         (|LNextLine| |lstate| (& |cptr|))))
                                                  (|while| (!= |c| |t|)
                                                           (BLOCK+
                                                            CL:NIL
                                                            (|if|
                                                             (== |c| (QUOTE+ 92. :CHAR))
                                                             (BLOCK+
                                                              CL:NIL
                                                              (|?:|
                                                               (= |c| (* (X++ |cptr|)))
                                                               |c|
                                                               (=
                                                                |c|
                                                                (|LNextLine|
                                                                 |lstate|
                                                                 (& |cptr|))))
                                                              (|if|
                                                               (& ([] (+ |ctype| 1.) |c|) 256.)
                                                               (BLOCK+
                                                                CL:NIL
                                                                (|for|
                                                                 (PROGN+
                                                                  (= |i| 0.)
                                                                  (= |ch| 0.))
                                                                 (&&
                                                                  (< |i| 3.)
                                                                  (&
                                                                   ([] (+ |ctype| 1.) |c|)
                                                                   256.))
                                                                 (PROGN+
                                                                  (++X |i|)
                                                                  (|?:|
                                                                   (= |c| (* (X++ |cptr|)))
                                                                   |c|
                                                                   (=
                                                                    |c|
                                                                    (|LNextLine|
                                                                     |lstate|
                                                                     (& |cptr|)))))
                                                                 (=
                                                                  |ch|
                                                                  (-
                                                                   (+ (* 8. |ch|) |c|)
                                                                   (QUOTE+ 48. :CHAR))))
                                                                (= (* (X++ |token|)) |ch|))
                                                               (|if|
                                                                (== |c| (QUOTE+ 120. :CHAR))
                                                                (BLOCK+
                                                                 CL:NIL
                                                                 (|for|
                                                                  (PROGN+
                                                                   (= |i| 0.)
                                                                   (= |ch| 0.))
                                                                  (&&
                                                                   (< |i| 2.)
                                                                   (&
                                                                    ([] (+ |ctype| 1.) |c|)
                                                                    (\| 4. 64.)))
                                                                  (PROGN+
                                                                   (++X |i|)
                                                                   (|?:|
                                                                    (= |c| (* (X++ |cptr|)))
                                                                    |c|
                                                                    (=
                                                                     |c|
                                                                     (|LNextLine|
                                                                      |lstate|
                                                                      (& |cptr|)))))
                                                                  (=
                                                                   |ch|
                                                                   (|?:|
                                                                    (+
                                                                     (* 16. |ch|)
                                                                     (<=
                                                                      |c|
                                                                      (QUOTE+ 57. :CHAR)))
                                                                    (- |c| (QUOTE+ 48. :CHAR))
                                                                    (+
                                                                     (-
                                                                      (\| |c| 32.)
                                                                      (QUOTE+ 97. :CHAR))
                                                                     10.))))
                                                                 (= (* (X++ |token|)) |ch|))
                                                                (BLOCK+
                                                                 CL:NIL
                                                                 (|switch|
                                                                  |c|
                                                                  (BLOCK+
                                                                   CL:NIL
                                                                   (|case| (QUOTE+ 114. :CHAR))
                                                                   (=
                                                                    (* (X++ |token|))
                                                                    (QUOTE+ 141. :CHAR))
                                                                   (|break|)
                                                                   (|case| (QUOTE+ 110. :CHAR))
                                                                   (=
                                                                    (* (X++ |token|))
                                                                    (QUOTE+ 141. :CHAR))
                                                                   (|break|)
                                                                   (|case| (QUOTE+ 116. :CHAR))
                                                                   (=
                                                                    (* (X++ |token|))
                                                                    (QUOTE+ 137. :CHAR))
                                                                   (|break|)
                                                                   (|case| (QUOTE+ 98. :CHAR))
                                                                   (=
                                                                    (* (X++ |token|))
                                                                    (QUOTE+ 136. :CHAR))
                                                                   (|break|)
                                                                   (|case| (QUOTE+ 102. :CHAR))
                                                                   (=
                                                                    (* (X++ |token|))
                                                                    (QUOTE+ 140. :CHAR))
                                                                   (|break|)
                                                                   (|case| (QUOTE+ 118. :CHAR))
                                                                   (=
                                                                    (* (X++ |token|))
                                                                    (QUOTE+ 140. :CHAR))
                                                                   (|break|)
                                                                   |default|
                                                                   (= (* (X++ |token|)) |c|)
                                                                   (|break|)))
                                                                 (|?:|
                                                                  (= |c| (* (X++ |cptr|)))
                                                                  |c|
                                                                  (=
                                                                   |c|
                                                                   (|LNextLine|
                                                                    |lstate|
                                                                    (& |cptr|))))))))
                                                             (|if|
                                                              (== |c| (QUOTE+ 141. :CHAR))
                                                              (BLOCK+
                                                               CL:NIL
                                                               (PROGN+
                                                                (= (-> |lstate| |curchar|) |c|)
                                                                (=
                                                                 (-> |lstate| |bufptr|)
                                                                 |cptr|))
                                                               (|LError|
                                                                (-> |lstate| |cpstream|)
                                                                0.
                                                                (|#LISP|
                                                                 ((|lispval|))
                                                                 "Missing closing ~C")
                                                                |t|)
                                                               (|break|))
                                                              (PROGN+
                                                               (= (* (X++ |token|)) |c|)
                                                               (|?:|
                                                                (= |c| (* (X++ |cptr|)))
                                                                |c|
                                                                (=
                                                                 |c|
                                                                 (|LNextLine|
                                                                  |lstate|
                                                                  (& |cptr|)))))))))
                                                  (|if| (!= |c| (QUOTE+ 141. :CHAR))
                                                        (|?:|
                                                         (= |c| (* (X++ |cptr|)))
                                                         |c|
                                                         (=
                                                          |c|
                                                          (|LNextLine| |lstate| (& |cptr|)))))
                                                  (|if| |skipping| (|break|))
                                                  (PROGN+ (= (-> |lstate| |curchar|) |c|)
                                                          (= (-> |lstate| |bufptr|) |cptr|))
                                                  (|if| (== |t| (QUOTE+ 34. :CHAR))
                                                        (=
                                                         (-> |lstate| |yylval|)
                                                         (|LString|
                                                          (-> |lstate| |tokbuf|)
                                                          |token|))
                                                        (=
                                                         (-> |lstate| |yylval|)
                                                         (|LChar|
                                                          |lstate|
                                                          (-> |lstate| |tokbuf|)
                                                          |token|)))
                                                  (|return| (|?:|
                                                             (== |t| (QUOTE+ 34. :CHAR))
                                                             260.
                                                             259.))
                                                  (|case| 9.)
                                                  (|?:| (= |c| (* (X++ |cptr|)))
                                                        |c|
                                                        (=
                                                         |c|
                                                         (|LNextLine| |lstate| (& |cptr|))))
                                                  (PROGN+ (= (-> |lstate| |curchar|) |c|)
                                                          (= (-> |lstate| |bufptr|) |cptr|))
                                                  (= (-> |lstate| |yylval|)
                                                     (CL:LIST (|#LISP|
                                                               ((|lispval|))
                                                               '|#LISP|)
                                                              (|#LISP|
                                                               ((|lispval|))
                                                               '((|lispval|)))
                                                              ((-> |lstate| |cpstream|)
                                                               (|#LISP|
                                                                ((|lispval|))
                                                                :READLISP))))
                                                  (= |c|
                                                     (|?:| (= |cptr| (-> |lstate| |bufptr|))
                                                           (|?:|
                                                            (= |c| (-> |lstate| |curchar|))
                                                            |c|
                                                            (|LNextLine| |lstate| (& |cptr|)))
                                                           (PROGN+
                                                            (= |cptr| (STRING+ ""))
                                                            (QUOTE+ 141. :CHAR))))
                                                  (|if| (! |skipping|) (|return| 280.))
                                                  (|break|)
                                                  |default|
                                                  (PROGN+ (= (-> |lstate| |curchar|) |c|)
                                                          (= (-> |lstate| |bufptr|) |cptr|))
                                                  (|LError| (-> |lstate| |cpstream|)
                                                            0.
                                                            (|#LISP|
                                                             ((|lispval|))
                                                             "Illegal character '~C'")
                                                            |c|)
                                                  (|break|)))))))
(DEFUNC+ (|zctoken.c| 834.)
         ((FLAG) (FCN+ |LComment| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ (((|char|) (* |cptr|) |oc|) ((|int|) |c| |nchars| |startline| |commentsnest|))
                 (= |c|
                    (|?:| (= |cptr| (-> |lstate| |bufptr|))
                          (|?:| (= |c| (-> |lstate| |curchar|))
                                |c|
                                (|LNextLine| |lstate| (& |cptr|)))
                          (PROGN+ (= |cptr| (STRING+ "")) (QUOTE+ 141. :CHAR))))
                 (= |startline| (|LCurLine| |lstate|))
                 (= |commentsnest| (|LCommentsNest| |lstate|))
                 (= |oc| (QUOTE+ 0. :CHAR))
                 (= |nchars| (- 1.))
                 (|while| (\|\| (!= |oc| (QUOTE+ 42. :CHAR)) (!= |c| (QUOTE+ 47. :CHAR)))
                          (BLOCK+ CL:NIL
                                  (|if| (== |c| 0.)
                                        (BLOCK+ CL:NIL
                                                (PROGN+ (= (-> |lstate| |curchar|) |c|)
                                                        (= (-> |lstate| |bufptr|) |cptr|))
                                                (|LError| (-> |lstate| |cpstream|)
                                                          0.
                                                          (|#LISP|
                                                           ((|lispval|))
                                                           "Input stream ended in the middle of a comment, which started on line ~D"
                                                           )
                                                          |startline|)))
                                  (|if| (&& (&& |commentsnest| (== |oc| (QUOTE+ 47. :CHAR)))
                                            (== |c| (QUOTE+ 42. :CHAR)))
                                        (BLOCK+ CL:NIL
                                                (PROGN+ (= (-> |lstate| |curchar|) |c|)
                                                        (= (-> |lstate| |bufptr|) |cptr|))
                                                (|LComment| |lstate|)
                                                (= |c|
                                                   (|?:| (= |cptr| (-> |lstate| |bufptr|))
                                                         (|?:|
                                                          (= |c| (-> |lstate| |curchar|))
                                                          |c|
                                                          (|LNextLine| |lstate| (& |cptr|)))
                                                         (PROGN+
                                                          (= |cptr| (STRING+ ""))
                                                          (QUOTE+ 141. :CHAR))))))
                                  (= |oc| |c|)
                                  (|?:| (= |c| (* (X++ |cptr|)))
                                        |c|
                                        (= |c| (|LNextLine| |lstate| (& |cptr|))))
                                  (++X |nchars|)))
                 (|?:| (= |c| (* (X++ |cptr|))) |c| (= |c| (|LNextLine| |lstate| (& |cptr|))))
                 (PROGN+ (= (-> |lstate| |curchar|) |c|) (= (-> |lstate| |bufptr|) |cptr|))
                 (|return| (== |nchars| 0.))))
(DEFUNC+ (|zctoken.c| 867.)
         ((|int|) (FCN+ |LSymbol| |lstate| |tokend| |nomacro|))
         ((((|struct| |lexerstate|)) (* |lstate|)) ((|char|) (* |tokend|)) ((FLAG) |nomacro|))
         (BLOCK+ CL:NIL
                 (|return| (CAST+ ((|int|) CL:NIL)
                                  ((-> |lstate| |cpstream|)
                                   (|#LISP| ((|lispval|))
                                     :PROCESS-SYMBOL)
                                   (-> |lstate| |ifstate|) |nomacro|
                                   (|LLispTok| (-> |lstate| |tokbuf|) |tokend|))))))
(DEFUNC+ (|zctoken.c| 880.)
         ((|int|) (FCN+ |LSymbol1| |lstate| |lexeme| |symbol| |macparams| |macdef|))
         ((((|struct| |lexerstate|)) (* |lstate|)) ((|lispval|) |symbol|) ((|int|) |lexeme|)
          ((|optarg| |lispval|) |macparams| |macdef|))
         (BLOCK+ CL:NIL
                 (|if| |lexeme|
                       (= (-> |lstate| |yylval|) |symbol|)
                       (BLOCK+ CL:NIL
                               (= (-> |lstate| |macro|) |symbol|)
                               (= (-> |lstate| |macparams|) |macparams|)
                               (= (-> |lstate| |yylval|) |macdef|)))
                 (|if| (== |lexeme| 314.)
                       (= (-> |lstate| |ifstate|) 3.)
                       (|if| (== (-> |lstate| |ifstate|) 3.) (= (-> |lstate| |ifstate|) 2.)))
                 (|return| |lexeme|)))
(DEFUNC+ (|zctoken.c| 902.)
         ((|lispval|) (FCN+ |LInt| |lstate| |tokstart| |tokend| |hexp| |unsignedp| |longp|))
         ((((|struct| |lexerstate|)) (* |lstate|)) ((|char|) (* |tokstart|) (* |tokend|))
          ((FLAG) |hexp| |unsignedp| |longp|))
         (BLOCK+ (((|int|) (= |toklen| (- |tokend| |tokstart|)))
                  ((|lispval|)
                   (= |type|
                      (|?:| |unsignedp|
                            (|?:| |longp|
                                  (|#LISP| ((|lispval|))
                                    (ZETA-C:ZCTYPE>UNSIGNED-LONG))
                                  (|#LISP| ((|lispval|))
                                    (ZETA-C:ZCTYPE>UNSIGNED)))
                            (|?:| |longp|
                                  (|#LISP| ((|lispval|))
                                    (ZETA-C:ZCTYPE>LONG))
                                  (|#LISP| ((|lispval|))
                                    CL:NIL)))))
                  ((|char|) (* |ttok|)) ((|lispval|) |lisptok| |value|)
                  ((|int|)
                   (= |base|
                      (|?:| |hexp| 16. (|?:| (== (* |tokstart|) (QUOTE+ 48. :CHAR)) 8. 10.)))))
                 (|if| (== |base| 8.)
                       (|for| (= |ttok| |tokstart|)
                              (< |ttok| |tokend|)
                              (++X |ttok|)
                              (|if| (> (* |ttok|) (QUOTE+ 55. :CHAR))
                                    (|LError| (-> |lstate| |cpstream|)
                                              (- |ttok| |tokend|)
                                              (|#LISP| ((|lispval|))
                                                "Digit '~C' not allowed in octal number.")
                                              (* |ttok|)))))
                 (= |lisptok| (|LLispTok| |tokstart| |tokend|))
                 (= |value|
                    (|#LISP| ((|lispval|))
                      (SI:XR-READ-FIXNUM-INTERNAL |lisptok|
                                                  0.
                                                  (SCL:STRING-LENGTH |lisptok|)
                                                  |base|)))
                 (|return| (|?:| |type|
                                 (|#LISP| ((|lispval|))
                                   `(QUOTE+ ,|value| ,|type|))
                                 |value|))))
(DEFUNC+ (|zctoken.c| 930.)
         ((|lispval|) (FCN+ |LFloat| |tokstart| |tokend| |shortp| |longp|))
         (((|char|) (* |tokstart|) (* |tokend|)) ((FLAG) |shortp| |longp|))
         (BLOCK+ (((|lispval|) (= |lisptok| (|LLispTok| |tokstart| |tokend|))))
                 (CL:IGNORE |longp|)
                 (|if| |shortp|
                       (|return| (|#LISP| ((|lispval|))
                                   (SI:XR-READ-FLONUM |lisptok| :SINGLE-FLOAT)))
                       (|return| (|#LISP| ((|lispval|))
                                   (SI:XR-READ-FLONUM |lisptok| :DOUBLE-FLOAT))))))
(DEFUNC+ (|zctoken.c| 947.)
         ((|lispval|) (FCN+ |LString| |tokstart| |tokend|))
         (((|char|) (* |tokstart|) (* |tokend|)))
         (BLOCK+ (((|int|) (= |toklen| (- |tokend| |tokstart|)) |i| |c|) ((|lispval|) |string|)
                  )
                 (= |string|
                    (|#LISP| ((|lispval|))
                      (GL:MAKE-ARRAY |toklen| :TYPE SYS:ART-STRING)))
                 (|for| (= |i| 0.)
                        (< |i| |toklen|)
                        (++X |i|)
                        (BLOCK+ CL:NIL
                                (= |c| ([] |tokstart| |i|))
                                (|#LISP| ((|lispval|))
                                  (GL:ASET (CL:CODE-CHAR |c|) |string| |i|))))
                 (|return| (|#LISP| ((|lispval|))
                             `(STRING+ ,|string|)))))
(DEFUNC+ (|zctoken.c| 967.)
         ((|lispval|) (FCN+ |LChar| |lstate| |tokstart| |tokend|))
         ((((|struct| |lexerstate|)) (* |lstate|)) ((|char|) (* |tokstart|) (* |tokend|)))
         (BLOCK+ (((|int|) (= |toklen| (- |tokend| |tokstart|))) ((|int|) (= |value| 0.) |i|)
                  ((|lispval|)
                   (= |type|
                      (|?:| (< |toklen| 2.)
                            (|#LISP| ((|lispval|))
                              (ZETA-C:ZCTYPE>CHAR))
                            (|?:| (== |toklen| 2.)
                                  (|#LISP| ((|lispval|))
                                    (ZETA-C:ZCTYPE>UNSIGNED-SHORT))
                                  (|?:| (<= |toklen| 4.)
                                        (|#LISP| ((|lispval|))
                                          (ZETA-C:ZCTYPE>UNSIGNED))
                                        (|#LISP| ((|lispval|))
                                          (ZETA-C:ZCTYPE>UNSIGNED-LONG))))))))
                 (|if| (< |toklen| 1.)
                       (|LError| (-> |lstate| |cpstream|)
                                 0.
                                 (|#LISP| ((|lispval|))
                                   "Char constant must have at least one char")))
                 (|for| (= |i| 0.)
                        (< |i| |toklen|)
                        (++X |i|)
                        (= |value| (+ (<< |value| 8.) ([] |tokstart| |i|))))
                 (|return| (|#LISP| ((|lispval|))
                             `(QUOTE+ ,|value| ,|type|)))))
(DEFUNC+ (|zctoken.c| 995.)
         (CL:NIL (FCN+ |LEOLErr| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ CL:NIL
                 (|LError| (-> |lstate| |cpstream|)
                           0.
                           (|#LISP| ((|lispval|))
                             "Unexpected end of line"))))
(ZETA-C:ZCENV>#DEFINE 'SKIPGRAY
                      '(ZETA-C:NO-PARAMS . " while (c == ' ' || c == '\\t') SKIP;    ")
                      '(|zctoken.c| 991.))
(ZETA-C:ZCENV>#DEFINE 'ENDCHECK
                      '(ZETA-C:NO-PARAMS
                        . " { if (c == '\\n') { SAVESTATE; LEOLErr(lstate); } else if (c == 0) { SAVESTATE; LEOFErr(lstate); } }  "
                        )
                      '(|zctoken.c| 992.))
(DEFUNC+ (|zctoken.c| 1002.)
         (CL:NIL (FCN+ |LEOFErr| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ CL:NIL
                 (|LError| (-> |lstate| |cpstream|)
                           0.
                           (|#LISP| ((|lispval|))
                             "Unexpected end of file"))))
(DEFUNC+ (|zctoken.c| 1014.)
         ((|int|) (FCN+ |LPreProc| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ (((|char|) |c| (* |cptr|) (= (* |token|) (-> |lstate| |tokbuf|))))
                 (= |c|
                    (|?:| (= |cptr| (-> |lstate| |bufptr|))
                          (|?:| (= |c| (-> |lstate| |curchar|))
                                |c|
                                (|LNextLine| |lstate| (& |cptr|)))
                          (PROGN+ (= |cptr| (STRING+ "")) (QUOTE+ 141. :CHAR))))
                 (|while| (\|\| (== |c| (QUOTE+ 32. :CHAR)) (== |c| (QUOTE+ 137. :CHAR)))
                          (|?:| (= |c| (* (X++ |cptr|)))
                                |c|
                                (= |c| (|LNextLine| |lstate| (& |cptr|)))))
                 CL:NIL
                 (|if| (== |c| (QUOTE+ 141. :CHAR))
                       (BLOCK+ CL:NIL
                               (PROGN+ (= (-> |lstate| |curchar|) |c|)
                                       (= (-> |lstate| |bufptr|) |cptr|))
                               (|return| 0.)))
                 (|while| (! (& ([] (+ |ctype| 1.) |c|) 8.))
                          (PROGN+ (= (* (X++ |token|)) |c|)
                                  (|?:| (= |c| (* (X++ |cptr|)))
                                        |c|
                                        (= |c| (|LNextLine| |lstate| (& |cptr|))))))
                 (= (* |token|) (QUOTE+ 0. :CHAR))
                 (PROGN+ (= (-> |lstate| |curchar|) |c|) (= (-> |lstate| |bufptr|) |cptr|))
                 (= |token| (-> |lstate| |tokbuf|))
                 (|if| (! (|strcmp| |token| (STRING+ "define")))
                       (|LDefine| |lstate|)
                       (|if| (! (|strcmp| |token| (STRING+ "undef")))
                             (|LUndef| |lstate|)
                             (|if| (! (|strcmp| |token| (STRING+ "ifdef")))
                                   (|LIfdef| |lstate| (CAST+ ((|char|) CL:NIL) 1.))
                                   (|if| (! (|strcmp| |token| (STRING+ "ifndef")))
                                         (|LIfdef| |lstate| (CAST+ ((|char|) CL:NIL) 0.))
                                         (|if| (! (|strcmp| |token| (STRING+ "if")))
                                               (|LIf| |lstate|)
                                               (|if| (! (|strcmp| |token| (STRING+ "elif")))
                                                     (|LElif| |lstate|)
                                                     (|if| (!
                                                            (|strcmp| |token| (STRING+ "else"))
                                                            )
                                                           (|LElse| |lstate|)
                                                           (|if|
                                                            (!
                                                             (|strcmp|
                                                              |token|
                                                              (STRING+ "endif")))
                                                            (|LEndif| |lstate|)
                                                            (|if|
                                                             (!
                                                              (|strcmp|
                                                               |token|
                                                               (STRING+ "include")))
                                                             (|LInclude| |lstate|)
                                                             (|if|
                                                              (!
                                                               (|strcmp|
                                                                |token|
                                                                (STRING+ "line")))
                                                              (|LLine| |lstate|)
                                                              (|if|
                                                               (!
                                                                (|strcmp|
                                                                 |token|
                                                                 (STRING+ "lisp")))
                                                               (|return| (|LLisp| |lstate|))
                                                               (|LError|
                                                                (-> |lstate| |cpstream|)
                                                                0.
                                                                (|#LISP|
                                                                 ((|lispval|))
                                                                 "Unknown preprocessor directive \"~A\""
                                                                 )
                                                                (|str2lisp| |token|))))))))))))
                       )
                 (|return| 0.)))
(DEFUNC+ (|zctoken.c| 1047.)
         ((|void|) (FCN+ |LDefine| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ (((|char|) |c| (* |cptr|) (* |savbufp|) (* |tokend|))
                  ((|lispval|) |name| |params| |defn|))
                 (|if| (|LCondCheck| |lstate|) (|return|))
                 (= |name| (|LGetSym| |lstate|))
                 (= |c|
                    (|?:| (= |cptr| (-> |lstate| |bufptr|))
                          (|?:| (= |c| (-> |lstate| |curchar|))
                                |c|
                                (|LNextLine| |lstate| (& |cptr|)))
                          (PROGN+ (= |cptr| (STRING+ "")) (QUOTE+ 141. :CHAR))))
                 (|if| (== |c| (QUOTE+ 40. :CHAR))
                       (BLOCK+ CL:NIL
                               (PROGN+ (= (-> |lstate| |curchar|) |c|)
                                       (= (-> |lstate| |bufptr|) |cptr|))
                               (|LMacArgs| |lstate|)
                               (= |c|
                                  (|?:| (= |cptr| (-> |lstate| |bufptr|))
                                        (|?:| (= |c| (-> |lstate| |curchar|))
                                              |c|
                                              (|LNextLine| |lstate| (& |cptr|)))
                                        (PROGN+ (= |cptr| (STRING+ "")) (QUOTE+ 141. :CHAR))))
                               (= |params| (-> |lstate| |macargs|)))
                       (= |params|
                          (|#LISP| ((|lispval|))
                            'ZETA-C:NO-PARAMS)))
                 (|while| (\|\| (== |c| (QUOTE+ 32. :CHAR)) (== |c| (QUOTE+ 137. :CHAR)))
                          (|?:| (= |c| (* (X++ |cptr|)))
                                |c|
                                (= |c| (|LNextLine| |lstate| (& |cptr|)))))
                 CL:NIL
                 (PROGN+ (= (-> |lstate| |curchar|) |c|) (= (-> |lstate| |bufptr|) |cptr|))
                 (= (-> |lstate| |lastbufptr|) (- (-> |lstate| |bufptr|) 1.))
                 (= (-> |lstate| |savbufp|) (-> |lstate| |savbuf|))
                 (= (* (X++ (-> |lstate| |savbufp|))) (QUOTE+ 32. :CHAR))
                 (= (-> |lstate| |macrostate|) 1.)
                 (= (-> |lstate| |macro|) |name|)
                 (|while| (!= (|LToken| |lstate|) 318.) CL:NIL)
                 (|if| (== (-> |lstate| |curchar|) 0.) (|LEOFErr| |lstate|))
                 (|LSaveText| |lstate|)
                 (= |savbufp| (-> |lstate| |savbufp|))
                 (= (-> |lstate| |savbufp|)
                    (= (-> |lstate| |lastbufptr|) (CAST+ ((|char|) (* CL:NIL)) 0.)))
                 (= (* (X++ |savbufp|)) (QUOTE+ 32. :CHAR))
                 (= (* (X++ |savbufp|)) (QUOTE+ 0. :CHAR))
                 (|strcpy| (-> |lstate| |tokbuf|) (-> |lstate| |savbuf|))
                 (= |tokend| (+ (-> |lstate| |tokbuf|) (- |savbufp| (-> |lstate| |savbuf|))))
                 (= |defn| (|LLispTok| (-> |lstate| |tokbuf|) |tokend|))
                 (= |defn|
                    (|#LISP| ((|lispval|))
                      (SCL:STRING-APPEND |defn|)))
                 ((-> |lstate| |cpstream|)
                  (|#LISP| ((|lispval|))
                    :DEFINE)
                  |name| |params| |defn|)
                 (= (-> |lstate| |macrostate|) 0.)))
(DEFUNC+ (|zctoken.c| 1087.)
         ((|void|) (FCN+ |LUndef| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ (((|lispval|) |name|))
                 (|if| (|LCondCheck| |lstate|) (|return|))
                 (= |name| (|LGetSym| |lstate|))
                 ((-> |lstate| |cpstream|)
                  (|#LISP| ((|lispval|))
                    :UNDEF)
                  |name|)
                 (|LSkipToEOL| |lstate|)))
(DEFUNC+ (|zctoken.c| 1113.)
         ((|void|) (FCN+ |LIfdef| |lstate| |polarity|))
         ((((|struct| |lexerstate|)) (* |lstate|)) ((FLAG) |polarity|))
         (BLOCK+ (((|lispval|) |name| |defined|))
                 (GL:PUSH (CL:LIST (|LCondP| |lstate|)
                                   (|?:| |polarity|
                                         (|#LISP| ((|lispval|))
                                           '|#ifdef|)
                                         (|#LISP| ((|lispval|))
                                           '|ifndef|))
                                   (|LCurFile| |lstate|)
                                   (|LCurLine| |lstate|)
                                   (|#LISP| ((|lispval|))
                                     CL:NIL))
                          (-> |lstate| |ifstack|))
                 (|if| (|LCondCheck| |lstate|) (|return|))
                 (= |name| (|LGetSym| |lstate|))
                 (|LSkipToEOL| |lstate|)
                 (= |defined|
                    ((-> |lstate| |cpstream|)
                     (|#LISP| ((|lispval|))
                       :DEFINEDP)
                     |name|))
                 (= |defined| (|?:| |polarity| |defined| (! |defined|)))
                 (CL:RPLACA (CL:CAR (-> |lstate| |ifstack|))
                            (|?:| |defined|
                                  (|#LISP| ((|lispval|))
                                    CL:T)
                                  (|#LISP| ((|lispval|))
                                    :FAILED)))))
(DEFUNC+ (|zctoken.c| 1132.)
         ((|void|) (FCN+ |LIf| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ (((|lispval|) |exptrue|))
                 (GL:PUSH (CL:LIST (|LCondP| |lstate|)
                                   (|#LISP| ((|lispval|))
                                     '|#if|)
                                   (|LCurFile| |lstate|)
                                   (|LCurLine| |lstate|)
                                   (|#LISP| ((|lispval|))
                                     CL:NIL))
                          (-> |lstate| |ifstack|))
                 (|if| (|LCondCheck| |lstate|) (|return|))
                 (= (-> |lstate| |ifstate|) 1.)
                 (= |exptrue|
                    ((-> |lstate| |cpstream|)
                     (|#LISP| ((|lispval|))
                       :IF)))
                 (= (-> |lstate| |ifstate|) 0.)
                 (|LSkipToEOL| |lstate|)
                 (CL:RPLACA (CL:CAR (-> |lstate| |ifstack|))
                            (|?:| |exptrue|
                                  (|#LISP| ((|lispval|))
                                    CL:T)
                                  (|#LISP| ((|lispval|))
                                    :FAILED)))))
(DEFUNC+ (|zctoken.c| 1150.)
         ((|void|) (FCN+ |LElif| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ (((|lispval|) |exptrue| |ifdesc|))
                 (|if| (! (-> |lstate| |ifstack|))
                       (BLOCK+ CL:NIL
                               (|LError| (-> |lstate| |cpstream|)
                                         0.
                                         (|#LISP| ((|lispval|))
                                           "#elif without matching #if"))
                               (|return|)))
                 (= |ifdesc| (CL:CAR (-> |lstate| |ifstack|)))
                 (|if| (CL:CADR (CL:CDDDR |ifdesc|))
                       (BLOCK+ CL:NIL
                               (|LError| (-> |lstate| |cpstream|)
                                         0.
                                         (|#LISP| ((|lispval|))
                                           "#elif: #else already appeared to go with ~A on line ~A"
                                           )
                                         (CL:CADR |ifdesc|)
                                         (CL:CADDDR |ifdesc|))
                               (|return|)))
                 (CL:RPLACA (CL:CDR |ifdesc|)
                            (|#LISP| ((|lispval|))
                              '|#elif|))
                 (CL:RPLACA (CL:CDDDR |ifdesc|) (|LCurLine| |lstate|))
                 (|if| (== (CL:CAR |ifdesc|)
                           (|#LISP| ((|lispval|))
                             CL:T))
                       (CL:RPLACA |ifdesc|
                                  (|#LISP| ((|lispval|))
                                    CL:NIL)))
                 (|if| (! (CL:CAR |ifdesc|))
                       (BLOCK+ CL:NIL (|LSkipToEOL| |lstate|) (|return|)))
                 (= (-> |lstate| |ifstate|) 1.)
                 (= |exptrue|
                    ((-> |lstate| |cpstream|)
                     (|#LISP| ((|lispval|))
                       :IF)))
                 (= (-> |lstate| |ifstate|) 0.)
                 (|LSkipToEOL| |lstate|)
                 (CL:RPLACA (CL:CAR (-> |lstate| |ifstack|))
                            (|?:| |exptrue|
                                  (|#LISP| ((|lispval|))
                                    CL:T)
                                  (|#LISP| ((|lispval|))
                                    :FAILED)))))
(DEFUNC+ (|zctoken.c| 1183.)
         ((|void|) (FCN+ |LElse| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ (((|lispval|) |ifdesc|))
                 (|LSkipToEOL| |lstate|)
                 (|if| (! (-> |lstate| |ifstack|))
                       (BLOCK+ CL:NIL
                               (|LError| (-> |lstate| |cpstream|)
                                         0.
                                         (|#LISP| ((|lispval|))
                                           "#else without matching #if"))
                               (|return|)))
                 (= |ifdesc| (CL:CAR (-> |lstate| |ifstack|)))
                 (|if| (CL:CADR (CL:CDDDR |ifdesc|))
                       (BLOCK+ CL:NIL
                               (|LError| (-> |lstate| |cpstream|)
                                         0.
                                         (|#LISP| ((|lispval|))
                                           "#else: #else already appeared to go with ~A on line ~A"
                                           )
                                         (CL:CADR |ifdesc|)
                                         (CL:CADDDR |ifdesc|))
                               (|return|)))
                 (CL:RPLACA (CL:CDR |ifdesc|)
                            (|#LISP| ((|lispval|))
                              '|#else|))
                 (CL:RPLACA (CL:CDDDR |ifdesc|) (|LCurLine| |lstate|))
                 (CL:RPLACA (CL:CDR (CL:CDDDR |ifdesc|))
                            (|#LISP| ((|lispval|))
                              CL:T))
                 (|if| (== (CL:CAR |ifdesc|)
                           (|#LISP| ((|lispval|))
                             :FAILED))
                       (CL:RPLACA |ifdesc|
                                  (|#LISP| ((|lispval|))
                                    CL:T))
                       (|if| (== (CL:CAR |ifdesc|)
                                 (|#LISP| ((|lispval|))
                                   CL:T))
                             (CL:RPLACA |ifdesc|
                                        (|#LISP| ((|lispval|))
                                          CL:NIL))))))
(DEFUNC+ (|zctoken.c| 1209.)
         ((|void|) (FCN+ |LEndif| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ CL:NIL
                 (|LSkipToEOL| |lstate|)
                 (|if| (! (-> |lstate| |ifstack|))
                       (BLOCK+ CL:NIL
                               (|LError| (-> |lstate| |cpstream|)
                                         0.
                                         (|#LISP| ((|lispval|))
                                           "#endif without matching #if"))
                               (|return|)))
                 (GL:POP (-> |lstate| |ifstack|))))
(DEFUNC+ (|zctoken.c| 1222.)
         ((|void|) (FCN+ |LInclude| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ (((|char|) |c| (* |cptr|) (= (* |token|) (-> |lstate| |tokbuf|)) (* |fnstart|)
                   )
                  ((|lispval|) |filename| |where|))
                 (|if| (|LCondCheck| |lstate|) (|return|))
                 (= |c|
                    (|?:| (= |cptr| (-> |lstate| |bufptr|))
                          (|?:| (= |c| (-> |lstate| |curchar|))
                                |c|
                                (|LNextLine| |lstate| (& |cptr|)))
                          (PROGN+ (= |cptr| (STRING+ "")) (QUOTE+ 141. :CHAR))))
                 (|while| (\|\| (== |c| (QUOTE+ 32. :CHAR)) (== |c| (QUOTE+ 137. :CHAR)))
                          (|?:| (= |c| (* (X++ |cptr|)))
                                |c|
                                (= |c| (|LNextLine| |lstate| (& |cptr|)))))
                 CL:NIL
                 (BLOCK+ CL:NIL
                         (|if| (== |c| (QUOTE+ 141. :CHAR))
                               (BLOCK+ CL:NIL
                                       (PROGN+ (= (-> |lstate| |curchar|) |c|)
                                               (= (-> |lstate| |bufptr|) |cptr|))
                                       (|LEOLErr| |lstate|))
                               (|if| (== |c| 0.)
                                     (BLOCK+ CL:NIL
                                             (PROGN+ (= (-> |lstate| |curchar|) |c|)
                                                     (= (-> |lstate| |bufptr|) |cptr|))
                                             (|LEOFErr| |lstate|)))))
                 CL:NIL
                 (= (-> |lstate| |macexpstart|) (- |cptr| 1.))
                 (|while| (&& (! (& ([] (+ |ctype| 1.) |c|) 8.)) (!= |c| (QUOTE+ 40. :CHAR)))
                          (PROGN+ (= (* (X++ |token|)) |c|)
                                  (|?:| (= |c| (* (X++ |cptr|)))
                                        |c|
                                        (= |c| (|LNextLine| |lstate| (& |cptr|))))))
                 (PROGN+ (= (-> |lstate| |curchar|) |c|) (= (-> |lstate| |bufptr|) |cptr|))
                 (= (* |token|) (QUOTE+ 0. :CHAR))
                 (= |fnstart| (-> |lstate| |tokbuf|))
                 (|if| (== (* |fnstart|) (QUOTE+ 34. :CHAR))
                       (BLOCK+ CL:NIL
                               (++X |fnstart|)
                               (|if| (!= ([] |token| (- 1.)) (QUOTE+ 34. :CHAR))
                                     (|LError| (-> |lstate| |cpstream|)
                                               (- 1.)
                                               (|#LISP| ((|lispval|))
                                                 "Incorrect #include syntax"))
                                     (--X |token|))
                               (= |where|
                                  (|#LISP| ((|lispval|))
                                    :CURRENT)))
                       (|if| (== (* |fnstart|) (QUOTE+ 60. :CHAR))
                             (BLOCK+ CL:NIL
                                     (++X |fnstart|)
                                     (|if| (!= ([] |token| (- 1.)) (QUOTE+ 62. :CHAR))
                                           (|LError| (-> |lstate| |cpstream|)
                                                     (- 1.)
                                                     (|#LISP| ((|lispval|))
                                                       "Incorrect #include syntax"))
                                           (--X |token|))
                                     (|if| (! (|strncmp| |fnstart| (STRING+ "sys/") 4.))
                                           (BLOCK+ CL:NIL
                                                   (+= |fnstart| 4.)
                                                   (= |where|
                                                      (|#LISP| ((|lispval|))
                                                        :INCLUDE-SYS)))
                                           (= |where|
                                              (|#LISP| ((|lispval|))
                                                :INCLUDE))))
                             (|if| (|LSymbol| |lstate| |token| (CAST+ ((|char|) CL:NIL) 0.))
                                   (= |where|
                                      (|#LISP| ((|lispval|))
                                        :CURRENT))
                                   (BLOCK+ CL:NIL
                                           (|LMacExpand| |lstate|)
                                           (|LInclude| |lstate|)
                                           (|return|)))))
                 (|LSkipToEOL| |lstate|)
                 (= |filename| (|LLispTok| |fnstart| |token|))
                 ((-> |lstate| |cpstream|)
                  (|#LISP| ((|lispval|))
                    :INCLUDE)
                  |filename| |where|)))
(DEFUNC+ (|zctoken.c| 1269.)
         ((|void|) (FCN+ |LLine| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ CL:NIL (|LSkipToEOL| |lstate|)))
(DEFUNC+ (|zctoken.c| 1277.)
         ((|int|) (FCN+ |LLisp| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ (((|char|) |c| (* |cptr|) (* |token|)) ((|lispval|) |forms|))
                 (|LSkipToEOL| |lstate|)
                 (= |c|
                    (|?:| (= |cptr| (-> |lstate| |bufptr|))
                          (|?:| (= |c| (-> |lstate| |curchar|))
                                |c|
                                (|LNextLine| |lstate| (& |cptr|)))
                          (PROGN+ (= |cptr| (STRING+ "")) (QUOTE+ 141. :CHAR))))
                 (|?:| (= |c| (* (X++ |cptr|))) |c| (= |c| (|LNextLine| |lstate| (& |cptr|))))
                 (|for| CL:NIL
                        CL:NIL
                        CL:NIL
                        (BLOCK+ CL:NIL
                                (|if| (== |c| (QUOTE+ 35. :CHAR))
                                      (BLOCK+ CL:NIL
                                              (|?:| (= |c| (* (X++ |cptr|)))
                                                    |c|
                                                    (= |c| (|LNextLine| |lstate| (& |cptr|))))
                                              (= |token| (-> |lstate| |tokbuf|))
                                              (|while| (\|\| (== |c| (QUOTE+ 32. :CHAR))
                                                             (== |c| (QUOTE+ 137. :CHAR)))
                                                       (|?:| (= |c| (* (X++ |cptr|)))
                                                             |c|
                                                             (=
                                                              |c|
                                                              (|LNextLine| |lstate| (& |cptr|))
                                                              )))
                                              CL:NIL
                                              (|while| (&& |c|
                                                           (! (& ([] (+ |ctype| 1.) |c|) 8.)))
                                                       (PROGN+ (= (* (X++ |token|)) |c|)
                                                               (|?:|
                                                                (= |c| (* (X++ |cptr|)))
                                                                |c|
                                                                (=
                                                                 |c|
                                                                 (|LNextLine|
                                                                  |lstate|
                                                                  (& |cptr|))))))
                                              (= (* |token|) (QUOTE+ 0. :CHAR))
                                              (= |token| (-> |lstate| |tokbuf|))
                                              (|if| (|strcmp| |token| (STRING+ "endlisp"))
                                                    (BLOCK+ CL:NIL
                                                            (PROGN+
                                                             (= (-> |lstate| |curchar|) |c|)
                                                             (= (-> |lstate| |bufptr|) |cptr|))
                                                            (|LError|
                                                             (-> |lstate| |cpstream|)
                                                             0.
                                                             (|#LISP|
                                                              ((|lispval|))
                                                              "No #directives allowed between #lisp and #endlisp"
                                                              ))
                                                            (|while|
                                                             (&&
                                                              |c|
                                                              (!= |c| (QUOTE+ 141. :CHAR)))
                                                             (|?:|
                                                              (= |c| (* (X++ |cptr|)))
                                                              |c|
                                                              (=
                                                               |c|
                                                               (|LNextLine|
                                                                |lstate|
                                                                (& |cptr|))))))
                                                    (|break|))))
                                (|while| (&& |c| (!= |c| (QUOTE+ 141. :CHAR)))
                                         (BLOCK+ CL:NIL
                                                 (PROGN+ (= (-> |lstate| |curchar|) |c|)
                                                         (= (-> |lstate| |bufptr|) |cptr|))
                                                 (GL:PUSH ((-> |lstate| |cpstream|)
                                                           (|#LISP|
                                                            ((|lispval|))
                                                            :READLISP))
                                                          |forms|)
                                                 (= |c|
                                                    (|?:| (= |cptr| (-> |lstate| |bufptr|))
                                                          (|?:|
                                                           (= |c| (-> |lstate| |curchar|))
                                                           |c|
                                                           (|LNextLine| |lstate| (& |cptr|)))
                                                          (PROGN+
                                                           (= |cptr| (STRING+ ""))
                                                           (QUOTE+ 141. :CHAR))))
                                                 (|while| (\|\|
                                                           (== |c| (QUOTE+ 32. :CHAR))
                                                           (== |c| (QUOTE+ 137. :CHAR)))
                                                          (|?:|
                                                           (= |c| (* (X++ |cptr|)))
                                                           |c|
                                                           (=
                                                            |c|
                                                            (|LNextLine| |lstate| (& |cptr|))))
                                                          )
                                                 CL:NIL
                                                 (|if| (== |c| (QUOTE+ 59. :CHAR))
                                                       (|while| (&&
                                                                 |c|
                                                                 (!= |c| (QUOTE+ 141. :CHAR)))
                                                                (|?:|
                                                                 (= |c| (* (X++ |cptr|)))
                                                                 |c|
                                                                 (=
                                                                  |c|
                                                                  (|LNextLine|
                                                                   |lstate|
                                                                   (& |cptr|))))))))
                                (|if| (== |c| 0.)
                                      (BLOCK+ CL:NIL
                                              (PROGN+ (= (-> |lstate| |curchar|) |c|)
                                                      (= (-> |lstate| |bufptr|) |cptr|))
                                              (|LEOFErr| |lstate|)))
                                (|?:| (= |c| (* (X++ |cptr|)))
                                      |c|
                                      (= |c| (|LNextLine| |lstate| (& |cptr|))))))
                 (PROGN+ (= (-> |lstate| |curchar|) |c|) (= (-> |lstate| |bufptr|) |cptr|))
                 (= (-> |lstate| |yylval|)
                    (CL:CONS (|#LISP| ((|lispval|))
                               '|#LISP|)
                             (CL:CONS (|#LISP| ((|lispval|))
                                        CL:NIL)
                                      (GL:NREVERSE |forms|))))
                 (|return| (|?:| (|LCondP| |lstate|) 315. 0.))))
(DEFUNC+ (|zctoken.c| 1322.)
         ((|lispval|) (FCN+ |LCondP| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ (((|lispval|) (= |ifstack| (-> |lstate| |ifstack|))))
                 (|return| (\|\| (! |ifstack|)
                                 (|#LISP| ((|lispval|))
                                   (CL:EQ (CL:CAAR |ifstack|) CL:T))))))
(DEFUNC+ (|zctoken.c| 1334.)
         ((|lispval|) (FCN+ |LCondCheck| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ (((|char|) |c| (* |cptr|)))
                 (= |c|
                    (|?:| (= |cptr| (-> |lstate| |bufptr|))
                          (|?:| (= |c| (-> |lstate| |curchar|))
                                |c|
                                (|LNextLine| |lstate| (& |cptr|)))
                          (PROGN+ (= |cptr| (STRING+ "")) (QUOTE+ 141. :CHAR))))
                 (|if| (! (|LCondP| |lstate|))
                       (BLOCK+ CL:NIL
                               (|while| (&& |c| (!= |c| (QUOTE+ 141. :CHAR)))
                                        (|?:| (= |c| (* (X++ |cptr|)))
                                              |c|
                                              (= |c| (|LNextLine| |lstate| (& |cptr|)))))
                               (PROGN+ (= (-> |lstate| |curchar|) |c|)
                                       (= (-> |lstate| |bufptr|) |cptr|))
                               (|return| (|#LISP| ((|lispval|))
                                           CL:T))))
                 (PROGN+ (= (-> |lstate| |curchar|) |c|) (= (-> |lstate| |bufptr|) |cptr|))
                 (|return| (|#LISP| ((|lispval|))
                             CL:NIL))))
(DEFUNC+ (|zctoken.c| 1353.)
         ((|lispval|) (FCN+ |LGetSym| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ (((|char|) |c| (* |cptr|) (= (* |token|) (-> |lstate| |tokbuf|))))
                 (= |c|
                    (|?:| (= |cptr| (-> |lstate| |bufptr|))
                          (|?:| (= |c| (-> |lstate| |curchar|))
                                |c|
                                (|LNextLine| |lstate| (& |cptr|)))
                          (PROGN+ (= |cptr| (STRING+ "")) (QUOTE+ 141. :CHAR))))
                 (|while| (\|\| (== |c| (QUOTE+ 32. :CHAR)) (== |c| (QUOTE+ 137. :CHAR)))
                          (|?:| (= |c| (* (X++ |cptr|)))
                                |c|
                                (= |c| (|LNextLine| |lstate| (& |cptr|)))))
                 CL:NIL
                 (BLOCK+ CL:NIL
                         (|if| (== |c| (QUOTE+ 141. :CHAR))
                               (BLOCK+ CL:NIL
                                       (PROGN+ (= (-> |lstate| |curchar|) |c|)
                                               (= (-> |lstate| |bufptr|) |cptr|))
                                       (|LEOLErr| |lstate|))
                               (|if| (== |c| 0.)
                                     (BLOCK+ CL:NIL
                                             (PROGN+ (= (-> |lstate| |curchar|) |c|)
                                                     (= (-> |lstate| |bufptr|) |cptr|))
                                             (|LEOFErr| |lstate|)))))
                 CL:NIL
                 (|while| (& ([] (+ |ctype| 1.) |c|) (\| (\| (\| 1. 2.) 4.) 128.))
                          (PROGN+ (= (* (X++ |token|)) |c|)
                                  (|?:| (= |c| (* (X++ |cptr|)))
                                        |c|
                                        (= |c| (|LNextLine| |lstate| (& |cptr|))))))
                 (PROGN+ (= (-> |lstate| |curchar|) |c|) (= (-> |lstate| |bufptr|) |cptr|))
                 (|return| ((-> |lstate| |cpstream|)
                            (|#LISP| ((|lispval|))
                              :INTERN)
                            (|LLispTok| (-> |lstate| |tokbuf|) |token|)))))
(DEFUNC+ (|zctoken.c| 1368.)
         ((|void|) (FCN+ |LSkipToEOL| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ (((|char|) |c| (* |cptr|)))
                 (= |c|
                    (|?:| (= |cptr| (-> |lstate| |bufptr|))
                          (|?:| (= |c| (-> |lstate| |curchar|))
                                |c|
                                (|LNextLine| |lstate| (& |cptr|)))
                          (PROGN+ (= |cptr| (STRING+ "")) (QUOTE+ 141. :CHAR))))
                 (|while| (&& |c| (!= |c| (QUOTE+ 141. :CHAR)))
                          (|?:| (= |c| (* (X++ |cptr|)))
                                |c|
                                (= |c| (|LNextLine| |lstate| (& |cptr|)))))
                 (PROGN+ (= (-> |lstate| |curchar|) |c|) (= (-> |lstate| |bufptr|) |cptr|))))
(DEFUNC+ (|zctoken.c| 1382.)
         ((|void|) (FCN+ |LMacArgs| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ (((|int|) |parendepth| (FCN+ GL:LENGTH)) ((|lispval|) |arglist|)
                  ((|char|) |c| (* |cptr|) (* |textbeg|) (* |textend|))
                  ((|char|) (= (* |tmacexpstart|) (-> |lstate| |macexpstart|))))
                 (= |c|
                    (|?:| (= |cptr| (-> |lstate| |bufptr|))
                          (|?:| (= |c| (-> |lstate| |curchar|))
                                |c|
                                (|LNextLine| |lstate| (& |cptr|)))
                          (PROGN+ (= |cptr| (STRING+ "")) (QUOTE+ 141. :CHAR))))
                 (|if| (== |c| (QUOTE+ 32. :CHAR))
                       (BLOCK+ CL:NIL
                               (|LWarn| (-> |lstate| |cpstream|)
                                        0.
                                        (|#LISP| ((|lispval|))
                                          "Warning: space before '(' in a macro invocation is nonportable"
                                          ))
                               (|?:| (= |c| (* (X++ |cptr|)))
                                     |c|
                                     (= |c| (|LNextLine| |lstate| (& |cptr|))))))
                 (|if| (!= |c| (QUOTE+ 40. :CHAR))
                       (BLOCK+ CL:NIL
                               (PROGN+ (= (-> |lstate| |curchar|) |c|)
                                       (= (-> |lstate| |bufptr|) |cptr|))
                               (|LError| (-> |lstate| |cpstream|)
                                         0.
                                         (|#LISP| ((|lispval|))
                                           "Missing argument(s) to preprocessor macro"))
                               (|return|)))
                 (|?:| (= |c| (* (X++ |cptr|))) |c| (= |c| (|LNextLine| |lstate| (& |cptr|))))
                 (PROGN+ (= (-> |lstate| |curchar|) |c|) (= (-> |lstate| |bufptr|) |cptr|))
                 (= (-> |lstate| |lastbufptr|) (- (-> |lstate| |bufptr|) 1.))
                 (= (-> |lstate| |savbufp|) (-> |lstate| |savbuf|))
                 (= (-> |lstate| |macrostate|) 2.)
                 (= |parendepth| 1.)
                 (|for| CL:NIL
                        CL:NIL
                        CL:NIL
                        (|switch| (|LToken| |lstate|)
                                  (BLOCK+ CL:NIL
                                          (|case| 308.)
                                          (|if| (<= (--X |parendepth|) 0.)
                                                (BLOCK+ CL:NIL
                                                        (|LSaveText| |lstate|)
                                                        (= |textend| (-> |lstate| |savbufp|))
                                                        (--X |textend|)
                                                        (|while|
                                                         (&&
                                                          (> |textend| (-> |lstate| |savbuf|))
                                                          (&
                                                           ([]
                                                            (+ |ctype| 1.)
                                                            ([] |textend| (- 1.)))
                                                           8.))
                                                         (--X |textend|))
                                                        (= (* |textend|) (QUOTE+ 0. :CHAR))
                                                        (= |textbeg| (-> |lstate| |savbuf|))
                                                        (|while|
                                                         (&
                                                          ([] (+ |ctype| 1.) (* |textbeg|))
                                                          8.)
                                                         (++X |textbeg|))
                                                        (GL:PUSH
                                                         (|str2lisp| |textbeg|)
                                                         |arglist|)
                                                        (=
                                                         (-> |lstate| |savbufp|)
                                                         (=
                                                          (-> |lstate| |lastbufptr|)
                                                          (CAST+ ((|char|) (* CL:NIL)) 0.)))
                                                        (=
                                                         (-> |lstate| |macargs|)
                                                         (GL:NREVERSE |arglist|))
                                                        (=
                                                         (-> |lstate| |macexpstart|)
                                                         |tmacexpstart|)
                                                        (|return|)))
                                          (|break|)
                                          (|case| 284.)
                                          (|if| (== |parendepth| 1.)
                                                (BLOCK+ CL:NIL
                                                        (|LSaveText| |lstate|)
                                                        (= |textend| (-> |lstate| |savbufp|))
                                                        (--X |textend|)
                                                        (|while|
                                                         (&&
                                                          (> |textend| (-> |lstate| |savbuf|))
                                                          (&
                                                           ([]
                                                            (+ |ctype| 1.)
                                                            ([] |textend| (- 1.)))
                                                           8.))
                                                         (--X |textend|))
                                                        (= (* |textend|) (QUOTE+ 0. :CHAR))
                                                        (= |textbeg| (-> |lstate| |savbuf|))
                                                        (|while|
                                                         (&
                                                          ([] (+ |ctype| 1.) (* |textbeg|))
                                                          8.)
                                                         (++X |textbeg|))
                                                        (GL:PUSH
                                                         (|str2lisp| |textbeg|)
                                                         |arglist|)
                                                        (=
                                                         (-> |lstate| |savbufp|)
                                                         (-> |lstate| |savbuf|))))
                                          (|break|)
                                          (|case| 307.)
                                          (++X |parendepth|)
                                          (|break|)
                                          (|case| 318.)
                                          (|LError| (-> |lstate| |cpstream|)
                                                    0.
                                                    (|#LISP| ((|lispval|))
                                                      "End of file in macro invocation: missing ')'?"
                                                      ))
                                          (= (-> |lstate| |savbufp|)
                                             (= (-> |lstate| |lastbufptr|)
                                                (CAST+ ((|char|) (* CL:NIL)) 0.)))
                                          (= (-> |lstate| |macargs|)
                                             (|#LISP| ((|lispval|))
                                               'CL:NIL))
                                          (= (-> |lstate| |macexpstart|) |tmacexpstart|)
                                          (|return|)
                                          |default|
                                          (|break|))))))
(DEFUNC+ (|zctoken.c| 1458.)
         ((|void|) (FCN+ |LMacExpand| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ (((FLAG) |arg_as_str| |tok_cat|) ((|char|) (* |tokstart|) (* |newbuf|))
                  ((|char|) |tcurchar| (* |tbufptr|))
                  ((|lispval|) (= |yylval| (-> |lstate| |yylval|))))
                 (|#LISP| CL:NIL
                   (CL:SETQ |newbuf.array| |yylval|
                            |newbuf.index| 0.)
                   (CL:SETQ |newbuf.array|
                             (GL:MAKE-ARRAY (GL:ARRAY-LENGTH |newbuf.array|)
                                            :TYPE
                                            SYS:ART-8B
                                            :DISPLACED-TO
                                            |newbuf.array|)))
                 (|if| (== (-> |lstate| |macparams|)
                           (|#LISP| ((|lispval|))
                             'ZETA-C:NO-PARAMS))
                       (= (-> |lstate| |macparams|)
                          (= (-> |lstate| |macargs|)
                             (|#LISP| ((|lispval|))
                               CL:NIL)))
                       (BLOCK+ CL:NIL
                               (|LMacArgs| |lstate|)
                               (|if| (!= (GL:LENGTH (-> |lstate| |macargs|))
                                         (GL:LENGTH (-> |lstate| |macparams|)))
                                     (|LError| (-> |lstate| |cpstream|)
                                               0.
                                               (|#LISP| ((|lispval|))
                                                 "Wrong number of arguments, ~D, to macro ~A")
                                               (GL:LENGTH (-> |lstate| |macargs|))
                                               (-> |lstate| |macro|)))))
                 (= |tcurchar| (-> |lstate| |curchar|))
                 (= |tbufptr| (-> |lstate| |bufptr|))
                 (= (-> |lstate| |curchar|) (* |newbuf|))
                 (= (-> |lstate| |bufptr|) (+ |newbuf| 1.))
                 (= (-> |lstate| |lastbufptr|) |newbuf|)
                 (= (-> |lstate| |savbufp|) (-> |lstate| |savbuf|))
                 (= (-> |lstate| |macrostate|) 3.)
                 (= |arg_as_str| (= |tok_cat| (CAST+ ((|char|) CL:NIL) 0.)))
                 (|for| CL:NIL
                        CL:NIL
                        CL:NIL
                        (BLOCK+ CL:NIL
                                (= (-> |lstate| |macargstr|) |arg_as_str|)
                                (= (-> |lstate| |mactokcat|) |tok_cat|)
                                (= |tokstart| (-> |lstate| |bufptr|))
                                (|switch| (|LToken| |lstate|)
                                          (BLOCK+ CL:NIL
                                                  (|case| 316.)
                                                  (|LSaveText| |lstate|)
                                                  (--X (-> |lstate| |savbufp|))
                                                  (= |arg_as_str| (CAST+ ((|char|) CL:NIL) 1.))
                                                  (|continue|)
                                                  (|case| 317.)
                                                  (|LSaveText| |lstate|)
                                                  (-= (-> |lstate| |savbufp|) 2.)
                                                  (|while| (&
                                                            ([]
                                                             (+ |ctype| 1.)
                                                             ([]
                                                              (-> |lstate| |savbufp|)
                                                              (- 1.)))
                                                            8.)
                                                           (--X (-> |lstate| |savbufp|)))
                                                  (= |tok_cat| (CAST+ ((|char|) CL:NIL) 1.))
                                                  (|break|)
                                                  (|case| 318.)
                                                  (= (* (-> |lstate| |savbufp|))
                                                     (QUOTE+ 0. :CHAR))
                                                  (= (-> |lstate| |savbufp|)
                                                     (= (-> |lstate| |lastbufptr|)
                                                        (CAST+ ((|char|) (* CL:NIL)) 0.)))
                                                  (= (-> |lstate| |bufptr|) |tbufptr|)
                                                  (= (-> |lstate| |curchar|) |tcurchar|)
                                                  (|LScanExpansion| |lstate|)
                                                  (|return|)
                                                  |default|
                                                  (|break|)))))))
(DEFUNC+ (|zctoken.c| 1528.)
         ((|void|) (FCN+ |LMacroSymbol| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ (((|lispval|) |arg| (FCN+ |LMacArg|)) ((|char|) (* |savbufp|))
                  ((|int|) |symlen| |arglen| |i|))
                 (= |symlen| (|strlen| (-> |lstate| |tokbuf|)))
                 (= |arg| (|LMacArg| |lstate| |symlen|))
                 (|if| |arg|
                       (BLOCK+ CL:NIL
                               (= |arglen|
                                  (CAST+ ((|int|) CL:NIL)
                                         (|#LISP| ((|lispval|))
                                           (GL:ARRAY-ACTIVE-LENGTH |arg|))))
                               (|LSaveText| |lstate|)
                               (= |savbufp| (-> |lstate| |savbufp|))
                               (-= |savbufp| |symlen|)
                               (|if| (-> |lstate| |macargstr|)
                                     (= (* (X++ |savbufp|)) (QUOTE+ 34. :CHAR)))
                               (|for| (= |i| 0.)
                                      (< |i| |arglen|)
                                      (++X |i|)
                                      (|#LISP| ((|lispval|))
                                        (GL:ASET (CL:CHAR-CODE (CL:AREF |arg| |i|))
                                                 |savbufp.array|
                                                 (CL:+ |i| |savbufp.index|))))
                               (+= |savbufp| |arglen|)
                               (|if| (-> |lstate| |macargstr|)
                                     (= (* (X++ |savbufp|)) (QUOTE+ 34. :CHAR)))
                               (= (* |savbufp|) (QUOTE+ 0. :CHAR))
                               (= (-> |lstate| |savbufp|) |savbufp|)))))
(DEFUNC+ (|zctoken.c| 1555.)
         ((|lispval|) (FCN+ |LMacArg| |lstate| |symlen|))
         ((((|struct| |lexerstate|)) (* |lstate|)) ((|int|) |symlen|))
         (BLOCK+ (((|lispval|) (= |params| (-> |lstate| |macparams|))
                   (= |args| (-> |lstate| |macargs|)))
                  ((|char|) (= (* |tokbuf|) (-> |lstate| |tokbuf|)))
                  ((|lispval|) (= |lisptok| (|LLispTok| |tokbuf| (+ |tokbuf| |symlen|)))))
                 (|if| (== |params|
                           (|#LISP| ((|lispval|))
                             'ZETA-C:NO-PARAMS))
                       (|return| (|#LISP| ((|lispval|))
                                   CL:NIL)))
                 (|while| |params|
                          (BLOCK+ CL:NIL
                                  (|if| (|#LISP| ((|lispval|))
                                          (ZETA-C:ARRAY-COMPARE |lisptok| (CL:CAR |params|)))
                                        (|return| (CL:CAR |args|)))
                                  (= |params| (CL:CDR |params|))
                                  (= |args| (CL:CDR |args|))))
                 (|return| (|#LISP| ((|lispval|))
                             CL:NIL))))
(DEFUNC+ (|zctoken.c| 1577.)
         ((|void|) (FCN+ |LScanExpansion| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ (((|int|) |exp_len| |line_len| |new_len| |call_len|)
                  ((|char|) (= (* |bufptr|) (- (-> |lstate| |bufptr|) 1.)) (* |call_start|)))
                 (= |exp_len| (|strlen| (-> |lstate| |savbuf|)))
                 (= |line_len|
                    (CAST+ ((|int|) CL:NIL)
                           (|#LISP| ((|lispval|))
                             (GL:ARRAY-ACTIVE-LENGTH |bufptr.array|))))
                 (= |call_len| (- |bufptr| (-> |lstate| |macexpstart|)))
                 (= |call_start| (- |bufptr| |call_len|))
                 (= |new_len| (- (+ |line_len| |exp_len|) |call_len|))
                 (|if| (> |new_len| 4096.)
                       (|LError| (-> |lstate| |cpstream|)
                                 0.
                                 (|#LISP| ((|lispval|))
                                   "More than 4K of line buffer being allocated, while expanding macro ~A;~%perhaps you have mutually recursive macros being expanded?"
                                   )
                                 (-> |lstate| |macro|)))
                 (|if| (> |new_len|
                          (CAST+ ((|int|) CL:NIL)
                                 (|#LISP| ((|lispval|))
                                   (GL:ARRAY-LENGTH |bufptr.array|))))
                       (|#LISP| ((|lispval|))
                         (GL:ADJUST-ARRAY-SIZE |bufptr.array| |new_len|)))
                 (|#LISP| ((|lispval|))
                   (GL:SETF (CL:FILL-POINTER |bufptr.array|) |new_len|))
                 (|memcpy| (+ |call_start| |exp_len|) |bufptr| (+ (|strlen| |bufptr|) 1.))
                 (|memcpy| |call_start| (-> |lstate| |savbuf|) |exp_len|)
                 (= (-> |lstate| |macrostate|) 0.)
                 (= (-> |lstate| |curchar|) (* |call_start|))
                 (= (-> |lstate| |bufptr|) (+ |call_start| 1.))))
(DEFUNC+ (|zctoken.c| 1611.)
         ((|void|) (FCN+ |LSaveText| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ (((|int|) |savlen|))
                 (|if| (-> |lstate| |savbufp|)
                       (BLOCK+ CL:NIL
                               (= |savlen|
                                  (- (- (-> |lstate| |bufptr|) (-> |lstate| |lastbufptr|)) 1.))
                               (|memcpy| (-> |lstate| |savbufp|)
                                         (-> |lstate| |lastbufptr|)
                                         |savlen|)
                               (+= (-> |lstate| |savbufp|) |savlen|)
                               (= (-> |lstate| |lastbufptr|) (- (-> |lstate| |bufptr|) 1.))))))
(DEFUNC+ (|zctoken.c| 1632.)
         ((|char|) (FCN+ |LNextLine| |lstate| |cpp|))
         ((((|struct| |lexerstate|)) (* |lstate|)) ((|char|) (* (* |cpp|))))
         (BLOCK+ (((|char|) (* |line|)))
                 (|if| (* |cpp|) (= (-> |lstate| |bufptr|) (* |cpp|)))
                 (|LSaveText| |lstate|)
                 (|if| (== (-> |lstate| |macrostate|) 3.)
                       (= |line| (CAST+ ((|char|) (* CL:NIL)) 0.))
                       (BLOCK+ CL:NIL
                               ((-> |lstate| |cpstream|)
                                (|#LISP| ((|lispval|))
                                  :NEXT-LINE))
                               (= |line| (-> |lstate| |bufptr|))))
                 (|if| (! |line|)
                       (BLOCK+ CL:NIL
                               (= (* |cpp|) (+ (STRING+ " ") 1.))
                               (|if| (-> |lstate| |savbufp|)
                                     (= (-> |lstate| |lastbufptr|) (- (* |cpp|) 1.)))
                               (|return| (QUOTE+ 0. :CHAR))))
                 (= (* |cpp|) (+ |line| 1.))
                 (|if| (-> |lstate| |savbufp|) (= (-> |lstate| |lastbufptr|) |line|))
                 (|return| ([] |line| 0.))))
(DEFUNC+ (|zctoken.c| 1659.)
         ((|void|) (FCN+ |LSetBuf| |lstate| |buf|))
         ((((|struct| |lexerstate|)) (* |lstate|)) ((|char|) (* |buf|)))
         (BLOCK+ CL:NIL (= (-> |lstate| |bufptr|) |buf|)))
(DEFUNC+ (|zctoken.c| 1670.)
         ((|int|) (FCN+ |LBufIndex| |lstate|))
         ((((|struct| |lexerstate|)) (* |lstate|)))
         (BLOCK+ (((|char|) (= (* |bufptr|) (-> |lstate| |bufptr|))))
                 (|return| (- (CAST+ ((|int|) CL:NIL)
                                     (|#LISP| ((|lispval|))
                                       |bufptr.index|))
                              1.))))
(DEFUNC+ (|zctoken.c| 1682.)
         ((|static| |char|) (* (FCN+ |strcpy| |s1| |s2|)))
         (((|char|) (* |s1|) (* |s2|)))
         (BLOCK+ (((|char|) (= (* |s1temp|) |s1|)))
                 (|do| (= (* (X++ |s1|)) (* |s2|)) (* (X++ |s2|)))
                 CL:NIL
                 (|return| |s1temp|)))
(DEFUNC+ (|zctoken.c| 1692.)
         ((|static| |int|) (FCN+ |strcmp| |s1| |s2|))
         (((|char|) (* |s1|) (* |s2|)))
         (BLOCK+ (((|char|) |c1| |c2|))
                 (|while| (\|\| (* |s1|) (* |s2|))
                          (BLOCK+ CL:NIL
                                  (= |c1| (* (X++ |s1|)))
                                  (= |c2| (* (X++ |s2|)))
                                  (|if| (< |c1| |c2|) (|return| (- 1.)))
                                  (|if| (> |c1| |c2|) (|return| 1.))))
                 (|return| 0.)))
(DEFUNC+ (|zctoken.c| 1707.)
         ((|static| |int|) (FCN+ |strncmp| |s1| |s2| |n|))
         (((|char|) (* |s1|) (* |s2|)) ((|int|) |n|))
         (BLOCK+ (((|char|) |c1| |c2|))
                 (|while| (&& (> (X-- |n|) 0.) (\|\| (* |s1|) (* |s2|)))
                          (BLOCK+ CL:NIL
                                  (= |c1| (* (X++ |s1|)))
                                  (= |c2| (* (X++ |s2|)))
                                  (|if| (< |c1| |c2|) (|return| (- 1.)))
                                  (|if| (> |c1| |c2|) (|return| 1.))))
                 (|return| 0.)))
(DEFUNC+ (|zctoken.c| 1723.)
         ((|static| |int|) (FCN+ |strlen| |s|))
         (((|char|) (* |s|)))
         (BLOCK+ (((|char|) (= (* |s0|) |s|)))
                 (|while| (* |s|) (X++ |s|))
                 (|return| (- |s| |s0|))))
(DEFUNC+ (|zctoken.c| 1733.)
         ((|static| |lispval|) (FCN+ |str2lisp| |s|))
         (((|char|) (* |s|)))
         (BLOCK+ CL:NIL
                 (|return| (|#LISP| ((|lispval|))
                             (ZETA-C:STRING-TO-LISP |s.array| |s.index|)))))
(DEFUNC+ (|zctoken.c| 1742.)
         ((|static| |char|) (* (FCN+ |memcpy| |dest| |src| |nbytes|)))
         (((|char|) (* |dest|) (* |src|)) ((|int|) |nbytes|))
         (BLOCK+ (((|char|) (= (* |tdest|) |dest|)))
                 (|if| (> |src| |dest|)
                       (|while| (> (X-- |nbytes|) 0.) (= (* (X++ |dest|)) (* (X++ |src|))))
                       (BLOCK+ CL:NIL
                               (+= |src| |nbytes|)
                               (+= |dest| |nbytes|)
                               (|while| (> (X-- |nbytes|) 0.)
                                        (= (* (--X |dest|)) (* (--X |src|))))))
                 (|return| |tdest|)))
(ZETA-C:ZCENV>#DEFINE 'ZETA_C_COMPARE_INCOMPARABLE_POINTERS
                      '(ZETA-C:NO-PARAMS . "   ")
                      '(|zctoken.c| 1740.))
(DEFUNC+ (|zctoken.c| 1759.)
         ((|static| |int|) (FCN+ |toupper| |c|))
         (((|char|) |c|))
         (BLOCK+ CL:NIL
                 (|return| (|?:| (& ([] (+ |ctype| 1.) |c|) 2.)
                                 (+ (- |c| (QUOTE+ 97. :CHAR)) (QUOTE+ 65. :CHAR))
                                 |c|))))
(DEFUNC+ (|zctoken.c| 1767.)
         ((|static| |int|) (FCN+ |tolower| |c|))
         (((|char|) |c|))
         (BLOCK+ CL:NIL
                 (|return| (|?:| (& ([] (+ |ctype| 1.) |c|) 1.)
                                 (+ (- |c| (QUOTE+ 65. :CHAR)) (QUOTE+ 97. :CHAR))
                                 |c|))))
