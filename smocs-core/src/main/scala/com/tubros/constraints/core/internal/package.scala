/**
 * Created on: Feb 22, 2013
 */
package com.tubros.constraints.core

import com.tubros.constraints.core.spi.solver.ToCanConstrainOps


/**
 * The '''internal''' `package` contains
 * [[com.tubros.constraints.core.spi.solver.Solver]] implementations which, by
 * their very nature and location, are implementation specific.
 *
 * @author svickers
 *
 */
package object internal
{
	/// Instance Properties
	val constraints = ToCanConstrainOps;
}

