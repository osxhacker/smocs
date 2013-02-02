/**
 * Created on: Jan 25, 2013
 */
package com.tubros.constraints.api
package solver

import scala.language.higherKinds

import scalaz._


/**
 * The '''Solver''' type defines a generic interface for running a CPS solution
 * algorithm.  '''Constraint'''s are defined as a nested type so that
 * instances can ''only'' be associated with particular '''Solver'''s, thus
 * eliminating a category of errors wherein '''Constraint'''s from one
 * '''Solver''' are given to another.
 *
 * @author svickers
 *
 */
abstract class Solver[A, M[+_] : Monad, +SolverT <: Solver[A, M, SolverT]]
{
	/// Class Types
	type DomainType[T] <: Domain[T]
	
	trait Constraint
	
	trait Term
	{
		/// Instance Properties
		def name : VariableName;
	}
	
	
	def add[T <: this.Constraint] (constraint : T) : M[_];
	
	
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
	def apply[C[_]] (context : SolverT => M[Stream[C[A]]]) : Stream[C[A]];
	
	
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


	def run[C[_]] (implicit mo : Monoid[C[A]], a : Applicative[C])
		: M[Stream[C[A]]];
}
