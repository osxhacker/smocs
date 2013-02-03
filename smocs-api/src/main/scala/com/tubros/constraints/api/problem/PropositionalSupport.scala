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
trait PropositionalSupport
{
	/// Self Type Constraints
	this : Equation =>
	
	
	/// Class Imports
	import ast._
	
	
	/// Class Types
	implicit class PropositionalOps[T <% Expression] (val lhs : T)
	{
		def /\ (rhs : Expression) : Expression = and (lhs, rhs);
		def && (rhs : Expression) : Expression = and (lhs, rhs);
		def \/ (rhs : Expression) : Expression = or (lhs, rhs);
		def || (rhs : Expression) : Expression = or (lhs, rhs);
	}
	
	
	def and (lhs : Expression, rhs : Expression) : Expression =
		LogicalAnd (lhs, rhs);
	
	def or (lhs : Expression, rhs : Expression) : Expression =
		LogicalOr (lhs, rhs);
}


package ast
{
	
case class LogicalAnd (
	override val lhs : Expression,
	override val rhs : Expression
	)
	extends Expression
		with BinaryOperator

case class LogicalOr (
	override val lhs : Expression,
	override val rhs : Expression
	)
	extends Expression
		with BinaryOperator

}
