/**
 * Created on: Jan 25, 2013
 */
package com.tubros.constraints.api.problem

import scalaz.NonEmptyList


/**
 * The '''Problem''' type defines the representation of a CSP problem
 * definition.  It is independent of any specific
 * [[com.tubros.constraints.api.solver]], as
 * [[com.tubros.constraints.api.solver.Solver]]s define ''how'' a '''Problem'''
 * is solved.
 *
 * @author svickers
 *
 */
case class Problem (
	val equations : NonEmptyList[Equation]
	)


object Problem
{
	/**
	 * The apply method allows a functional creation style for situations where
	 * the caller prefers a variadic invocation style.
	 */
	def apply (head : Equation, tail : Equation *) : Problem =
		new Problem (NonEmptyList (head, tail : _ *));
}