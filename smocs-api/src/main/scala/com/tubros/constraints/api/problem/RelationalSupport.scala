/**
 * Created on: Jan 27, 2013
 */
package com.tubros.constraints.api
package problem

import scalaz.Tree


/**
 * The '''RelationalSupport''' type provides the
 * [[com.tubros.constraints.api.problem.Equation]] EDSL with logical ordering
 * style operators.
 *
 * @author svickers
 *
 */
trait RelationalSupport
{
	/// Self Type Constraints
	this : Equation =>
	
	
	/// Class Types
	case class EqualTo (
		override val lhs : Expression,
		override val rhs : Expression
		)
		extends Expression
			with BinaryOperator
			
	case class NotEqualTo (
		override val lhs : Expression,
		override val rhs : Expression
		)
		extends Expression
			with BinaryOperator
			
	case class LessThan (
		override val lhs : Expression,
		override val rhs : Expression
		)
		extends Expression
			with BinaryOperator
			
	case class GreaterThan (
		override val lhs : Expression,
		override val rhs : Expression
		)
		extends Expression
			with BinaryOperator
			
	case class LessThanOrEqualTo (
		override val lhs : Expression,
		override val rhs : Expression
		)
		extends Expression
			with BinaryOperator
			
	case class GreaterThanOrEqualTo (
		override val lhs : Expression,
		override val rhs : Expression
		)
		extends Expression
			with BinaryOperator
			
		
	implicit class RelationalOps[T <% Expression] (val lhs : T)
	{
		def === (rhs : Expression) : Expression = equalTo (lhs, rhs);
		def @== (rhs : Expression) : Expression = equalTo (lhs, rhs);
		def !== (rhs : Expression) : Expression = notEqualTo (lhs, rhs);
		def <> (rhs : Expression) : Expression = notEqualTo (lhs, rhs);
		def < (rhs : Expression) : Expression = lessThan (lhs, rhs);
		def > (rhs : Expression) : Expression = greaterThan (lhs, rhs);
		def <= (rhs : Expression) : Expression = lessThanOrEqualTo (lhs, rhs);
		def >= (rhs : Expression) : Expression =
			greaterThanOrEqualTo (lhs, rhs);
	}
	
	
	def equalTo (lhs : Expression, rhs : Expression) : Expression =
		EqualTo (lhs, rhs);
	
	def notEqualTo (lhs : Expression, rhs : Expression) : Expression =
		NotEqualTo (lhs, rhs);
	
	def lessThan (lhs : Expression, rhs : Expression) : Expression =
		LessThan (lhs, rhs);
	
	def greaterThan (lhs : Expression, rhs : Expression) : Expression =
		GreaterThan (lhs, rhs);
	
	def lessThanOrEqualTo (lhs : Expression, rhs : Expression) : Expression =
		LessThanOrEqualTo (lhs, rhs);
	
	def greaterThanOrEqualTo (lhs : Expression, rhs : Expression) : Expression =
		GreaterThanOrEqualTo (lhs, rhs);
}
