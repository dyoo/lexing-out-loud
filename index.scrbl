#lang scribble/manual

@(require planet/scribble
          planet/version
          planet/resolver
          scribble/eval
          racket/sandbox)




@title{Lexing Out Loud: parsing @tt{LOLCODE} with Racket's parser-tools}
@author+email["Danny Yoo" "dyoo@hashcollision.org"]


@;; I'll need an evaluator for some small examples.
@(define my-evaluator
   (call-with-trusted-sandbox-configuration 
    (lambda ()
      (parameterize ([sandbox-output 'string]
                     [sandbox-error-output 'string])
        (make-evaluator 'racket
                        #:requires
                        (list 'parser-tools/lex
                              'parser-tools/yacc))))))




@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Introduction}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{The LOLCODE Grammar}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

The
@link["http://lolcode.com/contributions/bnf-grammar-goldparser"]{grammar}
for LOLCode describes the language's structure, using the GOLD
Meta-Language to describe it.


@;; It would be nice if we could somehow directly feed the GOLD
@;; meta-language description into  parser-tools without having to do
@;; any hand-translation.  Hmmm.


The document talks about @emph{character sets}, @emph{terminals}, and
Backus-Naur @emph{rules}.  The documentation of the GOLD Meta-Language
@link["http://www.devincook.com/goldparser/doc/grammars/index.htm"]{describes}
these in some detail.  





@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Tokenizing LOLCODE}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

There are a few things we want to capture when we tokenize LOLCODE; we
especially want to maintain the locations where the lexemes come from.
If we do so with enough fidelity, then it should be possible to
reconstruct the source file if we have the abstract syntax tree in
hand.



@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Parsing LOLCODE}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

