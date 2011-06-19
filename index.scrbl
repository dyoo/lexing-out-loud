#lang scribble/manual

@(require planet/scribble
          planet/version
          planet/resolver
          scribble/eval
          racket/sandbox
          (for-label parser-tools/lex))




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

This is an extended example that shows how to use Racket's
@seclink["top" #:doc '(lib
"parser-tools/parser-tools.scrbl")]{parser-tools} library to create a
parser for the @link["http://lolcode.com"]{LOLCODE} language.  We'll
only consider the surface syntax of LOLCODE here.  If we have some
time, maybe we can talk about how to write the semantics of LOLCODE in
another tutorial.  Maybe.



@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{The LOLCODE Grammar}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

The
@link["http://lolcode.com/contributions/bnf-grammar-goldparser"]{grammar}
for LOLCODE describes the language's structure, using the
@link["http://www.devincook.com/goldparser/"]{GOLD Meta-Language} to
describe it. 
Unfortunately, we don't have a tool that directly takes this file and
generates a parser for us in Racket.  That's too bad!  Well, maybe it
won't be too painful to hand-translate the definitions into ones that
are usable by parser-tools.  Let's keep our fingers crossed.

@;; It would be nice if we could somehow directly feed the GOLD
@;; meta-language description into  parser-tools without having to do
@;; any hand-translation.  Hmmm.


When we take a closer look at the grammar, we see that it's separated
into three distinct sections:
@itemlist[
@item{@emph{character sets}}
@item{@emph{terminals}}
@item{@emph{rules}}
]

The first two, @emph{character sets} and @emph{terminals}, are all
about lexing, to take an 
@seclink["i/o" #:doc '(lib "scribblings/guide/guide.scrbl")]{input port}
 and break the port down to
individual tokens.  The third section, on @emph{rules}, talks
about how to take the tokens and derive a structured representation
from them.

The documentation of the GOLD Meta-Language
@link["http://www.devincook.com/goldparser/doc/grammars/index.htm"]{describes}
these in some detail, and the vocabulary used in the grammar depends a
little on some built-in definitions in GOLD.



@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Twiddling with the tokenizer}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

The @racketmodname[parser-tools/lex] module provides tools for
breaking down the contents of input ports into tokens.  Before we
continue with the particulars of LOLCODE parsing, let's play with this
library a bit, to get used to it.


Let's first load in the @racketmodname[parser-tools/lex] library.
Even at this point, we can a very simple lexer, by using
@racket[lexer-src-pos].  Let's make a stupid-simple one that doesn't
know how to parse anything.  We can open up DrRacket and place in our
definitions window the following content:

@filebox["tokenizer.rkt"]{
@codeblock|{
#lang racket
(require parser-tools/lex)
(define my-silly-lexer (lexer-src-pos [(eof) 'done]))
}|}

@(my-evaluator '(define my-silly-lexer (lexer-src-pos [(eof) 'done])))


The essence of a lexer is a collection of @emph{pattern} and
@emph{actions} pairs; whenever it recognizes a particular pattern from
the input port, it'll suck the content and do an action.  In this
case, our @racket[my-silly-lexer] only has an @racket[eof] pattern,
and when it sees @racket[eof], it's action is to produce a
@racket['done].


Let's see what happens when we pass it some inputs.  In the interactions
window, let's evaluate the following.
@interaction[#:eval my-evaluator
             (my-silly-lexer (open-input-string ""))]
Ok, it seems that we're getting back some kind of @racket[position-token] structure, whatever that means.  Let's
take a closer look at that structured value.

@interaction[#:eval my-evaluator
(define my-first-little-token 
  (my-silly-lexer (open-input-string "")))
(position-token-token my-first-little-token)
(position-token-start-pos my-first-little-token)
(position-token-end-pos my-first-little-token)
]
Oh?  The @racket[position]s are themselves structures too?  That
seems a bit... nested.  Oh well.  Let's make sure we know how to get
in those too.

@interaction[#:eval my-evaluator
(define my-first-start-pos 
  (position-token-start-pos my-first-little-token))
(position-offset my-first-start-pos)
(position-line my-first-start-pos)
(position-col my-first-start-pos)
]

But why are we getting @racket[#f] for the line and column?

That's because input ports don't automatically keep track of their
line and column information, not unless we tell them to track this.
For now, let's force this tracking to happen on all ports by touching
@racket[port-count-lines-enabled], which is a parameter that tells
Racket what default to use when creating new ports.


We'll want to add this following near the front of code that constructs
such ports.
@racketblock[(parameterize ([port-count-lines-enabled #t]) ...)]




What happens if we give the lexer something it doesn't expect?
@interaction[#:eval my-evaluator
             (my-silly-lexer (open-input-string "hello?"))]
Oh, ok, so it'll give us a runtime error.  Reasonable enough.


If you already started looking at the reference information in
parser-tools, you may notice that it provides another way of making
lexers, with @racket[lexer], which has a similar interface to the
@racket[lexer-src-pos] that we used earlier.  Why aren't we using
@racket[lexer]?  The reason is because we ultimately want to capture
as much information as we can during the parsing process, including
positional information.  If we use @racket[lexer], we'd have to
reproduce that information (offset, line, column) from scratch.



@section{Tokenizing LOLCODE}

Let's look at the very first character set.
@verbatim|{
{WS}		= {Whitespace} - {CR} - {LF}
}|

This is saying that we'd like to define a @tt{WS} character class,
which consists of all whitespace, except for carriage returns and line
feeds.

The @racketmodname[parser-tools/lex] module has a similar notion of a
lexer abbreviation, and already has such a definition for
@racket[whitespace] that we can reuse here.


Let's play with it for a moment.

@interaction[#:eval my-evaluator
             (define-lex-abbrev WS (intersection whitespace
                                                 (complement "\r")
                                                 (complement "\n")))
             (define my-simple-whitespace-lexer 
               (lexer-src-pos [WS 
                               (begin
                                 (printf "I see whitespace\n")
                                 lexeme)]))
             (define my-simple-input-port (open-input-string "\t \n"))
             (my-simple-whitespace-lexer my-simple-input-port)
             (my-simple-whitespace-lexer my-simple-input-port)
             (my-simple-whitespace-lexer my-simple-input-port)
             (my-simple-whitespace-lexer my-simple-input-port)
             ]




There are a few things we want to capture when we tokenize LOLCODE; we
especially want to maintain the locations where the lexemes come from.
If we do so with enough fidelity, then it should be possible to
reconstruct the source file if we have the abstract syntax tree in
hand.



@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section{Parsing LOLCODE}
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

