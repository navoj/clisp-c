; -*- Mode: Lisp; Package: (Zeta-C Global); Base: 10; Tab-width: 5 -*-
; File: ZCINIT.LISP
;
;	This code has been placed in the public domain.
;	
; You must load this file before either loading the Zeta-C system itself or loading
; or editing any files of Zeta-C code.

;#-Genera
;(when (not (intern-soft "GMAP" 'global))
;  (load "zeta-c: library; gmap"))
(import 'gmap::gmap)

(setf (symbol-function 'lexpr-funcall) #'apply)

#-CLISP
(defmacro ferror (&optional a b c)
  `(error ,b ,c))

(setf (symbol-function 'ncons) #'list)

(setf (symbol-function 'nlistp) #'listp)

(defun copylist (x) (append x nil))

(defun memq (x y)
  (cond ((null y) nil)
	((eq x (car y)) y)
	((memq x (cdr y)))))

(defun xcons (x y) (cons y x))

(defmacro defconst (symbol &optional initvalue docstring)
  `(progn
     (defvar ,symbol nil ,docstring)
     (setq ,symbol ,initvalue)))

(setf (symbol-function 'multiple-value) #'multiple-value-setq)
(setf (symbol-function 'map) #'mapl)
(setf (symbol-function 'fix) #'floor)
(setf (symbol-function 'array-#-dims) #'array-rank)
(setf (symbol-function 'array-/#-dims) #'array-rank)
(setf (symbol-function 'copylist) #'copy-list)
(setf (symbol-function 'copylist*) #'copy-list*)
(setf (symbol-function 'deletef) #'delete-file)
(setf (symbol-function 'renamef) #'rename-file)
(setf (symbol-function 'probef) #'probe-file)
(setf standard-input *standard-input*)
(setf standard-output *standard-output*)
(setf terminal-io *terminal-io*)
(setf query-io *query-io*)
(setf debug-io *debug-io*)
(setf error-output *error-output*)
(setf readtable *readtable*)
(setf base *print-base*)
(setf prinlevel *print-level*)
(setf prinlength *print-length*)
(setf ibase *read-base*)
(setf (symbol-function 'symeval) #'symbol-value)
(setf (symbol-function 'fsymeval) #'symbol-function)
(defmacro fset (sym fn)
  `(setf (symbol-value ,sym) ,fn))
(setf (symbol-function 'get-pname) #'symbol-name)
(setf (symbol-function 'plist) #'symbol-plist)
(defmacro setplist (sym pl)
  `(setf (symbol-plist ,sym) ,pl))


; Note these zetalist to common lisp conversions:
; (apprend list nil) -> (copy-list list)
; (append list1 list2 nil) -> (copy-list (append list1 list2))
; (append list '()) -> (copy-list list)
; (append list ()) -> (copy-list list)
; (subst nil nil form) -> (copy-tree tree)
; (subst '() '() form) -> (copy-tree tree)
; (subst () () form) -> (copy-tree tree)
; (arraycall t array i1 i2 ...) -> (aref array i1 i2 ...)
; (arraycall nil array i1 i2 ...) -> (aref array i1 i2 ...)
; (arraycall fixnum array i1 i2 ...) -> (the fixnum (aref array i1 i2 ...))
; (arraycall flonum array i1 i2 ...) -> (the float (aref array i1 i2 ...))
; (aset v array i1 i2 ...) -> (setf (aref array i1 i2 ...) v)
; (apply #'aset v a i1 i2 ... 1) -> (setf (apply #'aref a i1 i2 ... 1) v)
; (SI:%LEXPR-ASET v array 1) -> (setf (apply #'aref array 1) v)


(defpackage #-MIT C #+MIT 'C				   ; bug in MIT system.
  (:use)
;  (:relative-names ("GL" GLOBAL))
  (:nicknames "GL")
;  (:size 128)
  (:export "+" "-" "*" "//" "%" "<<" ">>" "&" "|" "^" "~"
		 "==" "!=" "<" ">" "<=" ">=" "!" "&&" "||"
		 "=" "++X" "X++" "--X" "X--"
		 "++" "--"								    ; For the lexer.
		 "+=" "-=" "*=" "//=" "%=" "&=" "|=" "^=" "<<=" ">>="
		 "FUNCALL+" "FCN+" "[]" "." "->" "?:" "PROGN+"
		 "if" "else" "BLOCK+" "goto" "LABEL+"
		 "while" "do" "for" "break" "continue" "return"
		 "switch" "case" "default" "setjmp"
		 "DEFUNC+" "DECL+"
		 "char" "int" "long" "short" "signed" "unsigned"
		 "float" "double" "void" "lispval"
		 "struct" "packed_struct" "union" "enum" "..."
		 "extern" "static" "register" "auto" "typedef" "restarg" "optarg"
		 "CAST+" "sizeof"
		 "#LISP" "LIST+" "QUOTE+" "STRING+" "strcpy" "strncpy" "strcat" "strncat"
		 "strchr" "strcmp" "strncmp" "strlen" "strcspn" "memcpy" "memmove"
		 "memset" "memcmp" "memchr" "malloc" "calloc" "realloc" "_ctype_"
		 "toupper" "tolower" "scanf" "sscanf" "fscanf" "times" "fgetc"))

; Sigh, incompatibility runs rampant.
(defconstant *rubout-handler-message* #+Symbolics ':input-editor
							#-Symbolics ':rubout-handler)


; ================================================================
; Customization variables.

(defvar *firstclass-structures* t
  "Controls whether structures are firstclass objects, i.e., whether they can
   be targets of assignments, passed as arguments, or returned as function values.
   If NIL, any attempt to use structures in one of these ways will signal an error.
   (Note that we're talking about structures themselves, not just pointers to
   structures, which are always firstclass objects.)  Firstclass structures are
   supported by several of the Unix C compilers, but by very few of the non-Unix
   compilers, so for maximum portability they should not be used.")

;(defvar *compare-incomparable-pointers* nil
;  "If two pointers point into different arrays, then in a strict sense they are not
;   comparable, since their relative values have only to do with accidents in memory
;   allocation.  So ZETA-C normally checks for attempts to compare two such pointers,
;   and on finding one, issues an error.  If this variable is T, however, ZETA-C will
;   ignore the arrays of the pointers and compare only the indices (giving better
;   performance at the expense of possible anomalous behavior).
;/The variable takes effect at both compile time and run time: if it's NIL at compile
;   time, a run time check is compiled; if it's T at compile time, no check is
;   compiled (so code compiled this way ignores the runtime value).  To turn it on at
;   compile time, say in your C source file
;     #define ZETA_C_COMPARE_INCOMPARABLE_POINTERS
;   (or /"#undef .../" to turn it off).")
(defvar *compare-incomparable-pointers* nil
  "If two pointers point into different arrays, then in a strict sense they are not
comparable, since their relative values have only to do with accidents in memory
allocation. So ZETA-C normally checks for attempts to compare two such pointers,
and on finding one, issues an error. If this variable is T, however, ZETA-C will
ignore the arrays of the pointers and compare only the indices (giving better
performance at the expense of possible anomalous behavior).
The variable takes effect at both compile time and run time: if it's NIL at compile
time, a run time check is compiled; if it's T at compile time, no check is compiled
\(so code compiled this way ignores the runtime value). To turn it on at compile time,
say in your C source file #define ZETA_C_COMPARE_INCOMPARABLE_POINTERS 
\(or \"#undef ...\" to turn it off).")

(defvar *system-include-directory* "ZETA-C:Include;"
  "The directory in which an #included file is looked for when the name is
   delimited with angle brackets, as in /\"#include <stdio.h>/\"; analogous to
   //usr//include on Unix.")

(defvar *system-include-sys-directory* #-TI "ZETA-C:Include;Sys;"
							    #+TI "ZETA-C:Include.Sys;"	  ;
  "The directory in which to look for a file included by a command of the form
   /\"#include <sys/foobar.h>/\".  See *SYSTEM-INCLUDE-DIRECTORY*.")

(defvar *comments-nest* nil
  "Controls what happens if /\"//*/\" is found inside a comment: if NIL (the C
   standard), it is ignored; if T, it is considered to open a nested comment.")

(defvar *suppress-auto-float-to-double* nil
  "Controls whether floating-point arithmetic is forced to be done in double-
   precision.  If T, it is not; single-precision operands will produce a single-
   precision result (though variables declared \"double\" will still be double-
   precision, and operations where either operand is double will be done in
   double-precision).  NIL (the default) forces double-precision to be used
   everywhere, as is traditional in C implementations.  This variable takes effect
   at compile time, not run time; to turn it on, say in your C source file
      #define ZETA_C_SUPPRESS_AUTO_FLOAT_TO_DOUBLE
   (or \"#undef ...\" to turn it off).")

(defvar *case-insensitive* nil
  "If T, case is ignored in variable and function names.  The default is NIL, which
   gives standard C behavior.")


; ================================================================
; Program setup.

; MIT System 98 and Symbolics 5.0 have new package systems, which appear to even be
; compatible!

;(setf (symbol-function 'pkg-find-package) #'find-package)
(defmacro pkg-find-package (a &optional b)
  `(find-package ,a))

(defun create-c-package (name)
  "Creates a package for a C program to live in.  Be sure you don't use any names
   of existing LispM system packages, since the new package will shadow the old
   one."
  (let ((name (string-upcase name)))
    (or (pkg-find-package name :find)
	   (make-package name :use '(C)))))

(defvar *c-package* (pkg-find-package "C")
  "A variable holding the C package object, for convenience.")

(defun c-package-p (name)
  "Is this a C program package, that is, a subpackage of C:?"
  (let ((pkg (pkg-find-package name ':find)))
    (and pkg (eq (car (package-use-list pkg)) *c-package*))))

(defvar *c-user-package* (create-c-package "C-USER"))

(defvar *package-variable-initialization-alist* nil
  "An alist associating C packages with alists associating variables with
   expressions for computing their initial values.")


; ================================================================
; Proto-declarations (must be here because of file loading order).

; From ZCCLIB.C, String manipulation section.
;(export 'c:(|strcpy| |strncpy| |strcat| |strncat|
;		  |strchr| |strcmp| |strncmp| |strlen| |strcspn|) 'c)
(export (find-symbol "strcpy" 'C) 'C)
(export (find-symbol "strncpy" 'C) 'C)
(export (find-symbol "strcat" 'C) 'C)
(export (find-symbol "strncat" 'C) 'C)
(export (find-symbol "strchr" 'C) 'C)
(export (find-symbol "strcmp" 'C) 'C)
(export (find-symbol "strncmp" 'C) 'C)
(export (find-symbol "strlen" 'C) 'C)
(export (find-symbol "strcspn" 'C) 'C)

;(export 'c:(|memcpy| |memmove| |memset| |memcmp| |memchr|) 'c)
(export (find-symbol "memcpy" 'C) 'C)
(export (find-symbol "memmove" 'C) 'C)
(export (find-symbol "memset" 'C) 'C)
(export (find-symbol "memcmp" 'C) 'C)
(export (find-symbol "memchr" 'C) 'C)

; From ZCCLIB.C, Memory allocation section.
;(export 'c:(|malloc| |calloc| |realloc|) 'c)
(export (find-symbol "malloc" 'C) 'C)
(export (find-symbol "calloc" 'C) 'C)
(export (find-symbol "realloc" 'C) 'C)

; From ZCCLIB.C, Character type table section.
;(export 'c:(|_ctype_| |toupper| |tolower|) 'c)
(export (find-symbol "_ctype_" 'C) 'C)
(export (find-symbol "toupper" 'C) 'C)
(export (find-symbol "tolower" 'C) 'C)

 ; From ZCCLIB.C, Standard I/O section.
;(export 'c:(|scanf| |sscanf| |fscanf|) 'c)
(export (find-symbol "scanf" 'C) 'C)
(export (find-symbol "sscanf" 'C) 'C)
(export (find-symbol "fscanf" 'C) 'C)

; From ZCCLIB.C, Imitation Unix section.
;(export 'c:(|times|) 'c)
(export (find-symbol "times" 'C) 'C)

; From ZCLIB.LISP, Standard I/O section.
;(export 'c:(|fgetc|) 'c)
(export (find-symbol "fgetc" 'C) 'C)

; ================================================================
; Some areas to cons in.  These are in order of increasing permanence.

;; Temporary areas cause too many gross bugs; and the machines all have, or soon
;; will have, ephemeral GC or equivalent.
(defvar working-storage-area '(nil))

(defvar permanent-storage-area '(nil))



(defvar zc-temporary-area working-storage-area
  "The area Zeta-C uses for consing of stuff whose lifetime is the compilation
   of one top-level form.")

;(defvar zc-runtime-consing-area
;	   (make-area ':name 'zc-runtime-consing-area
;			    ':gc ':dynamic
;			    ':room t)
;  "The area compiled Zeta-C code uses for consing pointers and arrays.  At some
;   point this stuff will probably get freelisted, but for now we just let it be
;   dynamic.")
(defvar zc-runtime-consing-area '(nil))

(defvar zc-working-area working-storage-area
  "The area Zeta-C uses for consing of stuff that lasts longer than the compilation
   of one top-level form, but is not likely to be around indefinitely.")

(defvar zc-permanent-area permanent-storage-area
  "The area Zeta-C uses for consing of stuff that will probably not need to be
   garbage collected.")

; Fix bug in package creation *before* creating cparse and zclib
#+TI (load "zeta-c:source;explorer-2-patches")
(create-c-package 'cparse)
(create-c-package 'zclib)

; End of zcinit.lisp
