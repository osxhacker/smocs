/**
 * Created on: Jan 25, 2013
 */
package com.tubros.constraints.api

import scala.language.higherKinds

import scalaz.{
	Monad,
	Monoid
	}


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
abstract class Solver[+SolverT[+A] <: Solver[SolverT, A] : Monad, +A]
{
	/// Class Types
	trait Constraint
	
	trait Term
	{
		/// Instance Properties
		def name : VariableName;
	}
	
	
	def add[T <: this.Constraint] (constraint : T) : this.type;
	
	
//	def run[C[_]] (implicit mo : Monoid[C[A]]) : C[C[A]];
}
