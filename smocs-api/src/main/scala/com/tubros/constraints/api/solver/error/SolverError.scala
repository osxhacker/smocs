/**
 * Created on: Feb 8, 2013
 */
package com.tubros.constraints.api.solver.error


/**
 * The '''SolverError''' type is the parent to the category of types which
 * reify [com.tubros.constraints.api.solver.Solver]] errors.
 *
 * @author svickers
 *
 */
trait SolverError
	extends Serializable
{
	/// Instance Properties
	def message : String;
	
	
	override def toString : String = "Solver error: %s".format (message);
}
