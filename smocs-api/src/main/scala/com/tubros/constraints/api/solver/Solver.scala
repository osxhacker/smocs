/**
 * Created on: Jan 25, 2013
 */
package com.tubros.constraints.api
package solver

import scala.language.higherKinds

import scalaz._

import problem.{
	Equation,
	Problem
	}


/**
 * The '''Solver''' type defines a generic interface for running a CPS solution
 * algorithm. 
 *
 * @author svickers
 *
 */
abstract class Solver[A, M[+_] : Monad, +SolverT <: Solver[A, M, SolverT]]
{
	/// Class Types
	type DomainType[T] <: Domain[T]
	
	
	/**
	 * The add method internalizes an '''equation''' for use when producing
	 * the solution.  Any number of
	 * [[com.tubros.constraints.api.problem.Equation]]s can be provided through
	 * this method.
	 */
	def add (equation : Equation[A]) : M[Unit];
	
	
	/**
	 * This add method allows collaborators to provide a '''problem'' to use
	 * when solving the CSP.
	 */
	def add (problem : Problem[A]) : M[Unit];
	
	
	/**
	 * The apply method is the entry point into solving the CSP.  By having
	 * the syntax of:
	 * 
	 * {{{
	 * val answers = solver (s => defineConstraints (s));
	 * }}}
	 * 
	 * Concrete implementations can perform whatever actions ''they'' deem fit
	 * before and after the '''Solver''' is defined.
	 */
	def apply[C[_]] (context : SolverT => M[Stream[C[Answer[A]]]])
		: Stream[C[Answer[A]]];
	
	
	/**
	 * The newVar method creates a new
	 * [[com.tubros.constraints.api.solver.Variable]] having the unique
	 * '''name''' given and a specific '''domain''' of values it can take.
	 * 
	 * Note that creation is done ''through the monad M'' and may not actually
	 * happen at the point of invocation.
	 */
	def newVar (name : VariableName, domain : DomainType[A])
		: M[Variable[A, DomainType]];
	
	
	/**
	 * The newVars method is a convenience method for creating a [[scala.List]]
	 * of [[com.tubros.constraints.api.solver.Variable]]s all having the same
	 * '''domain''.
	 */
	def newVars[C[_]] (domain : DomainType[A])
		(names : C[VariableName])
		(implicit F : Foldable[C])
		: M[List[Variable[A, DomainType]]];


	def run[C[_]] (implicit mo : Monoid[C[Answer[A]]], a : Applicative[C])
		: M[Stream[C[Answer[A]]]];
}
