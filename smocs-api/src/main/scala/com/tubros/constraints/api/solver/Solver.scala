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
abstract class Solver[A, S[+_] : Monad, +SolverT <: Solver[A, S, SolverT]]
{
	/// Class Types
	type DomainType[T] <: Domain[T]
	
	trait Constraint
	
	trait Term
	{
		/// Instance Properties
		def name : VariableName;
	}
	
	
	def add[T <: this.Constraint] (constraint : T) : S[_];
	
	
	def apply[C[_]] (context : S[Stream[C[A]]]) : Stream[C[A]];
	
	
	def newVar (name : VariableName, domain : DomainType[A])
		: S[Variable[A, DomainType]];
	
	
	def newVars[C[_]] (domain : DomainType[A])
		(names : C[VariableName])
		(implicit F : Foldable[C])
		: S[List[Variable[A, DomainType]]];


	def run[C[_]] (implicit mo : Monoid[C[A]]) : S[Stream[C[A]]];
}
