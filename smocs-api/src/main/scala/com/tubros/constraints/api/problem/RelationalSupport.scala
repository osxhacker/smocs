/**
 * Created on: Jan 27, 2013
 */
package com.tubros.constraints.api
package problem


/**
 * The '''RelationalSupport''' type provides the
 * [[com.tubros.constraints.api.problem.Equation]] EDSL with logical ordering
 * style operators.
 *
 * @author svickers
 *
 */
trait RelationalSupport[T]
{
	/// Self Type Constraints
	this : Equation[T] =>
	
	
	/// Class Imports
	import ast._
	
	
	/// Class Types
	implicit class RelationalOps[U <% Expression[T]] (val lhs : U)
	{
		def === (rhs : Expression[T]) : Expression[T] = equalTo (lhs, rhs);
		def @== (rhs : Expression[T]) : Expression[T] = equalTo (lhs, rhs);
		def !== (rhs : Expression[T]) : Expression[T] = notEqualTo (lhs, rhs);
		def <> (rhs : Expression[T]) : Expression[T] = notEqualTo (lhs, rhs);
		def < (rhs : Expression[T]) : Expression[T] = lessThan (lhs, rhs);
		def > (rhs : Expression[T]) : Expression[T] = greaterThan (lhs, rhs);
		def <= (rhs : Expression[T]) : Expression[T] = lessThanOrEqualTo (lhs, rhs);
		def >= (rhs : Expression[T]) : Expression[T] =
			greaterThanOrEqualTo (lhs, rhs);
	}
	
	
	def equalTo (lhs : Expression[T], rhs : Expression[T]) : Expression[T] =
		EqualTo (lhs, rhs);
	
	def notEqualTo (lhs : Expression[T], rhs : Expression[T]) : Expression[T] =
		NotEqualTo (lhs, rhs);
	
	def lessThan (lhs : Expression[T], rhs : Expression[T]) : Expression[T] =
		LessThan (lhs, rhs);
	
	def greaterThan (lhs : Expression[T], rhs : Expression[T]) : Expression[T] =
		GreaterThan (lhs, rhs);
	
	def lessThanOrEqualTo (lhs : Expression[T], rhs : Expression[T]) : Expression[T] =
		LessThanOrEqualTo (lhs, rhs);
	
	def greaterThanOrEqualTo (lhs : Expression[T], rhs : Expression[T]) : Expression[T] =
		GreaterThanOrEqualTo (lhs, rhs);
}


package ast
{
	
case class EqualTo[T] (
	override val lhs : Expression[T],
	override val rhs : Expression[T]
	)
	extends Expression[T]
		with BinaryOperator[T]
		
case class NotEqualTo[T] (
	override val lhs : Expression[T],
	override val rhs : Expression[T]
	)
	extends Expression[T]
		with BinaryOperator[T]
		
case class LessThan[T] (
	override val lhs : Expression[T],
	override val rhs : Expression[T]
	)
	extends Expression[T]
		with BinaryOperator[T]
		
case class GreaterThan[T] (
	override val lhs : Expression[T],
	override val rhs : Expression[T]
	)
	extends Expression[T]
		with BinaryOperator[T]
		
case class LessThanOrEqualTo[T] (
	override val lhs : Expression[T],
	override val rhs : Expression[T]
	)
	extends Expression[T]
		with BinaryOperator[T]
		
case class GreaterThanOrEqualTo[T] (
	override val lhs : Expression[T],
	override val rhs : Expression[T]
	)
	extends Expression[T]
		with BinaryOperator[T]

}
