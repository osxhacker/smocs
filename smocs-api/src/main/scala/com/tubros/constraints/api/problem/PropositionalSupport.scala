/**
 * Created on: Feb 2, 2013
 */
package com.tubros.constraints.api.problem


/**
 * The '''PropositionalSupport''' type reifies a calculus for
 * [[http://en.wikipedia.org/wiki/Propositional_logic propositional logic]].
 * Sometimes called "predicate calculus", it effectively provides support
 * for what most programming languages call "logical operators."
 *
 * @author svickers
 *
 */
trait PropositionalSupport[T]
{
	/// Self Type Constraints
	this : Equation[T] =>
	
	
	/// Class Imports
	import ast._
	
	
	/// Class Types
	implicit class PropositionalOps[U <% Expression[T]] (val lhs : U)
	{
		def /\ (rhs : Expression[T]) : Expression[T] = and (lhs, rhs);
		def && (rhs : Expression[T]) : Expression[T] = and (lhs, rhs);
		def \/ (rhs : Expression[T]) : Expression[T] = or (lhs, rhs);
		def || (rhs : Expression[T]) : Expression[T] = or (lhs, rhs);
	}
	
	
	def and (lhs : Expression[T], rhs : Expression[T]) : Expression[T] =
		LogicalAnd (lhs, rhs);
	
	def or (lhs : Expression[T], rhs : Expression[T]) : Expression[T] =
		LogicalOr (lhs, rhs);
}


package ast
{
	
import scalaz._

import syntax.show._


case class LogicalAnd[T] (
	override val lhs : Expression[T],
	override val rhs : Expression[T]
	)
	extends Expression[T]
		with BinaryOperator[T]
{
	override def cord = Cord ("(", lhs.show, ") && (", rhs.show, ")");
}


case class LogicalOr[T] (
	override val lhs : Expression[T],
	override val rhs : Expression[T]
	)
	extends Expression[T]
		with BinaryOperator[T]
{
	override def cord = Cord ("(", lhs.show, ") || (", rhs.show, ")");
}



}
