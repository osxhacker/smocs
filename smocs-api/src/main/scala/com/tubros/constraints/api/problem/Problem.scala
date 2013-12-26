/**
 * Created on: Jan 25, 2013
 */
package com.tubros.constraints.api.problem

import scalaz._


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
case class Problem[T] (
	val equations : NonEmptyList[Equation[T]]
	)


object Problem
{
	/**
	 * The apply method allows a functional creation style for situations where
	 * the caller prefers a variadic invocation style.
	 */
	def apply[T] (head : Equation[T], tail : Equation[T] *) : Problem[T] =
		new Problem (NonEmptyList (head, tail : _ *));
	
	
	/// Implicit Conversions
	implicit class SemigroupProblem[A] (val problem : Problem[A])
		extends Semigroup[Problem[A]]
	{
		override def append (a : Problem[A], b : => Problem[A]) : Problem[A] =
			Problem (a.equations.append (b.equations));
	}
	
	
	implicit def ShowProblem[A] : Show[Problem[A]] =
		new Show[Problem[A]] {
            override def show (a : Problem[A]) : Cord =
            	Cord (
            		"Problem(\n\t",
            		Cord.mkCord ("\n\t", a.equations.map (_.show).list : _*),
            		"\n\t)"
            		);
            }
}
