/**
 * Created on: Jan 27, 2013
 */
package com.tubros.constraints.api
package problem


/**
 * The '''ArithmeticSupport''' type provides operators for the EDSL related to
 * mathematical operations, such as `mult` (`*`) and `pow` (`^`).
 *
 * @author svickers
 *
 */
trait ArithmeticSupport
{
	/// Self Type Constraints
	this : Equation =>


	/// Class Imports
	import ast._
	
	
	/// Class Types
	implicit class ArithmeticOps[T <% Expression] (val lhs : T)
	{
		def + (rhs : Expression) : Expression = add (lhs, rhs);
		def - (rhs : Expression) : Expression = sub (lhs, rhs);
		def / (rhs : Expression) : Expression = div (lhs, rhs);
		def % (rhs : Expression) : Expression = mod (lhs, rhs);
		def * (rhs : Expression) : Expression = mult (lhs, rhs);
		def ^ (rhs : Expression) : Expression = pow (lhs, rhs);
	}
			
		
	def add (lhs : Expression, rhs : Expression) : Expression =
		Plus (lhs, rhs);

	def sub (lhs : Expression, rhs : Expression) : Expression =
		Minus (lhs, rhs);

	def div (lhs : Expression, rhs : Expression) : Expression =
		DividedBy (lhs, rhs);

	def mod (lhs : Expression, rhs : Expression) : Expression =
		Modulo (lhs, rhs);
	
	def mult (lhs : Expression, rhs : Expression) : Expression =
		Times (lhs, rhs);

	def pow (lhs : Expression, rhs : Expression) : Expression =
		Power (lhs, rhs);
}


package ast
{
	
case class DividedBy (
	override val lhs : Expression,
	override val rhs : Expression
	)
	extends Expression
		with BinaryOperator

case class Plus (
	override val lhs : Expression,
	override val rhs : Expression
	)
	extends Expression
		with BinaryOperator

case class Power (
	override val lhs : Expression,
	override val rhs : Expression
	)
	extends Expression
		with BinaryOperator

case class Minus (
	override val lhs : Expression,
	override val rhs : Expression
	)
	extends Expression
		with BinaryOperator

case class Modulo (
	override val lhs : Expression,
	override val rhs : Expression
	)
	extends Expression
		with BinaryOperator

case class Times (
	override val lhs : Expression,
	override val rhs : Expression
	)
	extends Expression
		with BinaryOperator
		
}
