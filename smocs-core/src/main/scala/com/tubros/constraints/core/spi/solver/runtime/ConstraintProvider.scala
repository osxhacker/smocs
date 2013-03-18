/**
 * Created on: Mar 11, 2013
 */
package com.tubros.constraints.core.spi.solver
package runtime

import scalaz._

import com.tubros.constraints.api.VariableName
import com.tubros.constraints.api.solver.error.SolverError


/**
 * The '''ConstraintProvider''' type defines the contract for types to be able
 * to provider [[com.tubros.constraints.core.spi.solver.Constraint]]s which
 * are in the proper invocation [[scalaz.Kleisli]] composition.
 *
 * @author svickers
 *
 */
trait ConstraintProvider[A]
{
	/// Class Types
	
	
	def constraints () : Set[Constraint[A]];
	
	
	def constraintsFor (available : Set[VariableName]) : Set[Constraint[A]];
	
	
	/**
	 * The globalConstraints method is expected to produce a [[scalaz.Kleisli]]
	 * which applies all [[com.tubros.constraints.core.spi.solver.Constraint]]s
	 * considered to be ''global''.
	 */
	def globalConstraints ()
		: Kleisli[
			({ type L[+A] = SolverError \/ A})#L,
			Constraint[A]#Env[A],
			Constraint[A]#Env[A]
			];
}
