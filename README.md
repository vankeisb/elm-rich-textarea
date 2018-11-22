A styleable textarea written in Elm.

[Live Demo](https://vankeisb.github.io/elm-rich-textarea)

**This project is in early inception stage.**
 
For the moment we mostly poke around the 
"hidden textarea" option for creating some kind of text-editor experience. 

### Features

* textarea-like editing experience (edition, key/mouse nav and selection, scrolling etc)
* ability to plug-in custom rendering (parser)
    * Text styling
    * Images etc. (at least in a second time)
* code completion
    * With pluggable content

All the code should be in pure Elm, without use of ports !
    
Part of the challenge is to provide a clean API for integrators of the "component".
It should be easy to use, and customize.    


### Dev env

Running the tests (in project root dir) :

    elm-test

Working on the package (in project root dir) :

    elm reactor
    
This allows to play with the examples.

### CI

Travis runs the tests, and builds the examples.

[![Build Status](https://travis-ci.org/vankeisb/elm-rich-textarea.svg?branch=develop)](https://travis-ci.org/vankeisb/elm-rich-textarea)


