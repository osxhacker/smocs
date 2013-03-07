/**
 * Created on: Mar 7, 2013
 */
package com.tubros.constraints.api.solver.error

/**
 * The '''NoVariablesProvidedError''' type is produced when there are no
 * [[com.tubros.constraints.api.solver.Variable]]s given for a
 * [[com.tubros.constraints.api.solver.Solver]] to use. 
 *
 * @author svickers
 *
 */
object NoVariablesProvidedError
	extends SolverError
{
	/// Instance Properties
	override val message = "No Variables given";
}
