/**
 * Created on: Mar 6, 2013
 */
package com.tubros.constraints.core.internal

import scala.language.{
	higherKinds,
	postfixOps
	}

import scalaz._

import com.tubros.constraints.api.solver._
import com.tubros.constraints.api.solver.error._
import com.tubros.constraints.core.spi.solver.Constraint


/**
 * The '''ConsistencyChecks''' type provides
 * [[com.tubros.constraints.api.solver.Solver]] types with common error
 * checking functionality regarding the ''definition'' of the CSP.  An example
 * of this is checking for any unbound
 * [[com.tubros.constraints.api.solver.Variable]]s within a
 * [[com.tubros.constraints.core.spi.solver.Constraint]].
 *
 * @author svickers
 *
 */
trait ConsistencyChecks[A, DomainT[X] <: Domain[X]]
{
	/// Class Imports
	import Scalaz._
	
	
	/// Class Types
	type ValidationType = Validation[
		SolverError,
		(Seq[Variable[A, DomainT]], Seq[Constraint[A]])
		]
	
	
	def hasVariables (
		variables : Seq[Variable[A, DomainT]],
		constraints : Seq[Constraint[A]]
		)
		: ValidationType =
	{
		variables.isEmpty.fold (
			NoVariablesProvidedError.failure,
			(variables, constraints).success
			);
	}
	
	
	def hasUnknownVariables (
		variables : Seq[Variable[A, DomainT]],
		constraints : Seq[Constraint[A]]
		)
		: ValidationType =
	{
		val available = variables.to[Set] map (_.name);
		val missing = constraints.flatMap (_.variables).toSet.filterNot {
			name =>
				
			available contains (name);
			}
		
		missing.isEmpty.fold (
			(variables, constraints).success,
			MissingVariables (missing).fail
			);
	}
}
