#lang scribble/manual

@(require scribble/example
          racket/sandbox
          @for-label[racket/base
                     racket/contract
                     data/rope])

@(define rope-eval (make-base-eval))
@(rope-eval '(require data/rope))

@title{Rope Data Structure}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@defmodule[data/rope]

@table-of-contents[]

@; ---------------------------------------------------------------------------------------

@section{Introduction}

Hello, world!

@; ---------------------------------------------------------------------------------------

@(close-eval rope-eval)
