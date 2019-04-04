Javalette Compiler
==================

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

.. _Parsing Expression Grammar: https://en.wikipedia.org/wiki/Parsing_expression_grammar
.. _Pest book: https://pest.rs/book/
