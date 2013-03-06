/**
 * Created on: Mar 6, 2013
 */
package com.tubros.constraints.api


/**
 * The '''solver''' `package` defines CSP types which reify abstractions
 * involved in providing CSP solver functionality to Smocs.
 *
 * @author svickers
 *
 */
package object solver
{
	/// Class Types
	type GlobalConstraint[A] = PartialFunction[Seq[Answer[A]], Boolean]
}
