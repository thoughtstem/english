#lang scribble/manual
@require[@for-label[english
                    racket/base]]

@title{english}
@author{thoughtstem}

@defmodule[english]

A utility library for generating English text -- e.g. generating plurals and other grammatical annoyances.


@defproc[(english [s string?] ...) string?]{
  Just combines all the @racket[s] values together with a space in between.

  Also a good place to do post-processing and cleanups that can easliy be expressed as a @racket[regexp-replace].  For example, spaces before commas and semi-colons are automatically deleted.

  TODO: Consider adding (via parameters) various post-processings of this sort.  The existing ones are hard coded at the moment. 
}

@defproc[(plural [s string?]) string?]{
  Takes a string and attempts to make an English plural out of it, if it is not already pluralized.

  It uses some pretty dumb algorithms at the moment.  This could be improved.
}

@defproc[(a/an [s string?]) string?]{
  Takes a string and adds the appropriate article onto it -- depending on whether it begins with a vowel. 
}

@defproc[(list-of [#:or word-for-none "none" string?]
                  [s string?] ...) string?]{
  Takes a list and joins them with commas (including the oxford comma).  Handles lists of zero or more.  You may supply the @racket[word-for-none] argument to configure what happens when there are zero things.
}

@defproc[(itemize [s string?] ...) string?]{
  Takes a list of things and renders them as an item list, like: 1) first thing; 2) second thing; 3) another thing.

  Semi-colons are used as separators.  Numbering (of course) begins at 1.

  TODO: Consider adding ways to configure that.
}





