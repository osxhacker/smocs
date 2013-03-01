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
	/// Class Types
	type Env[A] = Map[VariableName, A]
	
	
	/// Instance Properties
	def variables : Set[VariableName];
	
	
	/**
	 * The apply method transforms a set of variables by constraining their
	 * domains.
	 */
	def apply (in : Env[A]) : SolverError \/ Env[A];
	
	
	override def isDefinedAt (values : Env[A]) : Boolean =
		isDefinedAt (values.keySet);
	
	
	def isDefinedAt (names : Set[VariableName]) : Boolean =
		(variables &~ names).isEmpty;
}


object Constraint
{
	def unit[A] : Constraint[A] = new Constraint[A] {
		override val variables = Set.empty[VariableName];
		
		override def apply (in : Env[A]) : SolverError \/ Env[A] = \/- (in);
		}
	
	
	def kleisliUnit[A] =
		Kleisli[
			({ type L[+A] = SolverError \/ A})#L,
			Map[VariableName, A],
			Map[VariableName, A]
			] (unit[A]);
}
