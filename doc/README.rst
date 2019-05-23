Javalette Compiler
==================

Extensions
----------
The compiler only implements the ``pointers`` extension.

Building the compiler
---------------------

The compiler (``jlc``) is built in Rust_ using Cargo_.
It currently requires the nightly_ rust toolchain in order to compile.

The ``M̀akefile`` included in the project installs this for you.
Or you can manually install Cargo like this:

1. Install Rustup_

2. Install nightly ::

    rustup toolchain install nightly

3. Compile ``jlc`` ::

    cargo +nightly build

    ./target/debug/jlc

.. _Rust: https://www.rust-lang.org
.. _Cargo: https://www.rust-lang.org/tools
.. _nightly: https://doc.rust-lang.org/1.13.0/book/nightly-rust.html
.. _Rustup: https://rustup.rs/


Using the compiler
------------------

Print the help information: ::

    ./jlc --help

Compiling a javalette-file: ::

    ./jlc < my_progam.jl


Javalette
---------

See ``/src/grammar.pest`` for the formal syntax definition in the form of a
`Parsing Expression Grammar`_. You can refer to the `Pest book`_ for the syntax specification
(Pest is the library we use to generate the parser).

PEG is inherently unambiguous due to being greedy, i.e. when faced with an ordered choice it
will always attempt them in order, short circuiting if it succeeds. For this reason, a given
string will always result in a single parse tree. For example, referring to our ``grammar.pest``,
consider the following input: ::

    if (true)
        a++;
    else
        b++;
        
The PEG will match on exactly the rule ``( If ~ "(" ~ Expr ~ ")" ~ Stmt ~ Else ~ Stmt)`` and not
``( If ~ "(" ~ Expr ~ ")" ~ Stmt)`` since the former is an earlier rule in the order, as shown below: ::

    Stmt = {
        ...
        ( If ~ "(" ~ Expr ~ ")" ~ Stmt ~ Else ~ Stmt) |
        ( If ~ "(" ~ Expr ~ ")" ~ Stmt) |
        ...
    }

.. _Parsing Expression Grammar: https://en.wikipedia.org/wiki/Parsing_expression_grammar
.. _Pest book: https://pest.rs/book/
