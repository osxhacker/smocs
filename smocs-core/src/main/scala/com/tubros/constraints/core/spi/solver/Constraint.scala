/**
 * Created on: Feb 4, 2013
 */
package com.tubros.constraints.core.spi.solver

import scala.language.higherKinds

import scalaz._

import com.tubros.constraints.api.VariableName
import com.tubros.constraints.api.solver.{
	Domain,
	Variable
	}
import com.tubros.constraints.api.solver.error.SolverError


/**
 * The '''Constraint''' type reifies the concept of ''constraining'' an
 * arbitrary type ''A'' which is participating in the solution of a CSP.
 * the [[com.tubros.constraints.api.solver.Domain]] of values instances of
 * ''A'' can take are represented by
 * [[com.tubros.constraints.api.solver.Variable]]s presented to the `apply`
 * method.  All participating [[com.tubros.constraints.api.solver.Variable]]s
 * will be given to each '''Constraint''' so that they, in turn, have enough
 * context to be able to provide their expected behaviour.
 *
 * @author svickers
 *
 */
trait Constraint[A]
	extends PartialFunction[
		Map[VariableName, A],
		SolverError \/ Map[VariableName, A]
		]
{
	/// Class Imports
	import std.anyVal._
	import syntax.equal._
	
	
	/// Class Types
	type ConstraintResult[+X] = SolverError \/ Env[X]
	type Env[+A] = Map[VariableName, A]
	
	
	/// Instance Properties
	/**
	 * The derived property identifies this '''Constraint''' as either
	 * ''producing'' a derived [[com.tubros.constraints.api.solver.Variable]]
	 * or not.
	 */
	def derived : Option[VariableName];
	
	/**
	 * The variables property contains all
	 * [[com.tubros.constraints.api.solver.Variable]]s ''used'' in the
	 * definition of the '''Constraint'''.
	 */
	def variables : Set[VariableName];
	
	
	/**
	 * The apply method transforms a set of variables by constraining their
	 * domains.
	 */
	def apply (in : Env[A]) : ConstraintResult[A];
	
	
	/**
	 * The exactMatch method determines whether or not this '''Constraint'''
	 * requires precisely all of the
	 * [[com.tubros.constraints.api.VariableName]]s present in the '''names'''
	 * [[scala.collection.Set]].
	 */
	def exactMatch (names : Set[VariableName]) : Boolean =
		(variables.isEmpty || variables.size === names.size) && 
			isDefinedAt (names);
	
	
	override def isDefinedAt (values : Env[A]) : Boolean =
		isDefinedAt (values.keySet);
	
	
	def isDefinedAt (names : Set[VariableName]) : Boolean =
		(variables &~ names).isEmpty;
}


object Constraint
{
	/**
	 * The chained method creates a [[scalaz.Kleisli]] which chains together
	 * the invocation of each of the '''constraints''' given.
	 */
	def chained[A, C[_]] (constraints : C[Constraint[A]])
		(implicit F : Foldable[C])
		=
		F.foldLeft (constraints, kleisliUnit[A]) {
			case (accum, c) =>
				
			accum >==> c;
			}
	
	
	/**
	 * The kleisliUnit method creates a [[scalaz.Kleisli]]
	 * [[com.tubros.constraints.core.spi.solver.Constraint]] which
	 * unconditionally succeeds.
	 */
	def kleisliUnit[A] =
		Kleisli[
			({ type L[+A] = SolverError \/ A})#L,
			Constraint[A]#Env[A],
			Constraint[A]#Env[A]
			] (unit[A]);
	
	
	def unit[A] : Constraint[A] = new Constraint[A] {
		override val derived = None;
		override val variables = Set.empty[VariableName];
		
		override def apply (in : Env[A]) : ConstraintResult[A] = \/- (in);
		}
}
