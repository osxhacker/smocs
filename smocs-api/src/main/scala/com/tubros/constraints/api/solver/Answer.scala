/**
 * Created on: Feb 1, 2013
 */
package com.tubros.constraints.api
package solver


/**
 * The '''Answer''' type embodies the results of running a
 * [[com.tubros.constraints.api.solver.Solver]] which satisfies the CSP given
 * to it.
 *
 * @author svickers
 *
 */
final case class Answer[A] (
	val name : VariableName,
	val value : A
	)
	
	
object Answer
{
	def apply[A] (pair : (Symbol, A)) : Answer[A] =
		new Answer (pair._1, pair._2);
}
