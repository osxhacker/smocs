/**
 * Created on: Feb 12, 2013
 */
package com.tubros.constraints.api.solver.error


/**
 * The '''UnsolvableError''' type is produced when a CPS solver cannot find a
 * valid solution within the domain of values available.
 *
 * @author svickers
 *
 */
object UnsolvableError
	extends SolverError
{
	/// Instance Properties
	override lazy val message = "Unable to find a valid solution";
}
