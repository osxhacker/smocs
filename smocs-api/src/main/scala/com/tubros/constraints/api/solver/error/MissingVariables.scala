/**
 * Created on: Feb 12, 2013
 */
package com.tubros.constraints.api.solver.error

import scalaz._

import com.tubros.constraints.api.VariableName


/**
 * The '''MissingVariables'''
 * [[com.tubros.constraints.api.solver.error.SolverError]] type is produced
 * when an [[com.tubros.constraints.api.problem.Equation]] references a
 * [[com.tubros.constraints.api.solver.Variable]] which does not exist in
 * the CPS evaluation environment.
 *
 * @author svickers
 *
 */
case class MissingVariables (
	val missing : Set[VariableName]
	)
	extends SolverError
{
	/// Class Imports
	import scalaz.syntax.std.boolean._
	
	
	/// Instance Properties
	override lazy val message : String =
		"Missing %s variable%s: %s".format (
			missing.size,
			(missing.size > 1) ? "s" | "",
			missing
			);
}
