smocs
=====

Scala Monadic Constraint Solver

The goal of this project is to define a
[Constraint Satisfiaction Problem](http://en.wikipedia.org/wiki/Constraint_satisfaction_problem), *CSP*, solver framework which employs
[Monadic](http://en.wikipedia.org/wiki/Monad_%28functional_programming%29) programming techniques.


Overview
--------

The project is organized into the following subsystems:

* API
* Core

### API ###

Here, the types defined are largely concerned with defining the *contracts*
expected to be provided in a deployed environment.  Why separate them from
where they are fulfilled?  This is done to help facilitate OSGi-based use
as well as to provide a clear separation of concerns.

There are "concrete" classes found in the API bundle, though.  These are
*implementation agnostic* and, even though being classes and not traits,
represent concepts expected not to vary across implementations.  Of course,
some may be found to contain implementation concerns/leanings/encodings.  If
that happens, then they should be refactored.

Two of the main design goals are to enable a rich embedded
[DSL] (http://en.wikipedia.org/wiki/Domain-specific_language) when defining
problems and to facilitate an open-ended set of *solvers* to be available.
A small example of the EDSL looks like :

	val problem = Problem (
		new Equation[Int] {
			def apply = 'x < 10 && 'x > 0
			},
		new Equation[Int] {
			def apply = 'y === 'x ** 2
			},
		// This could have been in the Equation above, but is shown
		// separately for illustrative purposes.
		new Equation[Int] {
			def apply = 'y < 10
			},
		);

Solving these constraints would yield values corresponding with :

	('x -> 1, 'y -> 1), ('x -> 2, 'y -> 4), ('x -> 3, 'y -> 9)


### Core ###

The core subsystem is where the smocs implementation resides.  These
collaborations are most certainly implementation specific and should be
expected to change across major releases, with the option to have binary
incompatibility broken in a minor release (though this should happen less
frequently).

Further Reading
---------------

This section lists various resources considered relevant to CSP.  It is by
no means exhaustive and is provided so that the interested reader can
have a starting point.

1. Stuart Russell, Peter Norvig, "Chapter 6",
	in *Artificial Intelligence A Modern Approach*, (Prentice Hall, 2010).
2. Tom Schrijvers, Peter Stuckey, Philip Wadler,
	*Monaid Constraint Programming*, accessed January 24, 2013,
	https://lirias.kuleuven.be/bitstream/123456789/234095/1/paper.pdf
3. eed3si9n, *learning Scalaz*, accessed January 24, 2013,
	http://eed3si9n.com/category/tags/scala/scalaz
4. Dr. Jacob Feldman, *JSR-331*,
	accessed January 24, 2013,
	http://jcp.org/aboutJava/communityprocess/final/jsr331/index.html
5. Tiark Rompf, Martin Odersky,
	*Lightweight Modular Staging: A Pragmatic Approach to Runtime Code Generation and Compiled DSLs*,
	accessed Feb 7, 2013,
	http://infoscience.epfl.ch/record/150347/files/gpce63-rompf.pdf

