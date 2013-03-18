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
import com.tubros.constraints.core.spi.solver.runtime._


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
		(Seq[Variable[A, DomainT]], SymbolTable, ConstraintProvider[A])
		]
	
	
	def hasVariables (
		variables : Seq[Variable[A, DomainT]],
		symbolTable : SymbolTable,
		provider : ConstraintProvider[A]
		)
		: ValidationType =
	{
		symbolTable.isEmpty.fold (
			NoVariablesProvidedError.failure,
			(variables, symbolTable, provider).success
			);
	}
	
	
	def hasUnknownVariables (
		variables : Seq[Variable[A, DomainT]],
		symbolTable : SymbolTable,
		provider : ConstraintProvider[A]
		)
		: ValidationType =
	{
		val missing = provider.constraints.flatMap (_.variables).toSet.filterNot {
			name =>
				
			symbolTable contains (name);
			}
		
		missing.isEmpty.fold (
			(variables, symbolTable, provider).success,
			MissingVariables (missing).fail
			);
	}
}
