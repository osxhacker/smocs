/**
 * Created on: Jan 27, 2013
 */
package com.tubros.constraints.api
package problem

import scala.language.implicitConversions

import scalaz._


/**
 * The '''ArithmeticSupport''' type provides operators for the EDSL related to
 * mathematical operations, such as `mult` (`*`) and `pow` (`**`).
 *
 * @author svickers
 *
 */
trait ArithmeticSupport[T]
{
	/// Self Type Constraints
	this : Equation[T] =>


	/// Class Imports
	import ast._
	
	
	/// Class Types
	implicit class ArithmeticOps[U <% Expression[T]] (val lhs : U)
	{
		def + (rhs : Expression[T]) : Expression[T] = add (lhs, rhs);
		def - (rhs : Expression[T]) : Expression[T] = sub (lhs, rhs);
		def / (rhs : Expression[T]) : Expression[T] = div (lhs, rhs);
		def % (rhs : Expression[T]) : Expression[T] = mod (lhs, rhs);
		def * (rhs : Expression[T]) : Expression[T] = mult (lhs, rhs);
		def ** (rhs : Expression[T]) : Expression[T] = pow (lhs, rhs);
		def unary_- : Expression[T] = negate (lhs);
	}
			
		
	def abs (exp : Expression[T]) : Expression[T] =
		AbsoluteValue (exp);
	
	def add (lhs : Expression[T], rhs : Expression[T]) : Expression[T] =
		Plus (lhs, rhs);

	def sub (lhs : Expression[T], rhs : Expression[T]) : Expression[T] =
		Minus (lhs, rhs);

	def div (lhs : Expression[T], rhs : Expression[T]) : Expression[T] =
		Quotient (lhs, rhs);

	def mod (lhs : Expression[T], rhs : Expression[T]) : Expression[T] =
		Modulo (lhs, rhs);
	
	def mult (lhs : Expression[T], rhs : Expression[T]) : Expression[T] =
		Times (lhs, rhs);

	def negate (exp : Expression[T]) : Expression[T] =
		Negate (exp);
	
	def pow (lhs : Expression[T], rhs : Expression[T]) : Expression[T] =
		Power (lhs, rhs);
}


package ast
{
	
import syntax.show._

case class AbsoluteValue[T] (override val operand : Expression[T])
	extends Expression[T]
		with UnaryOperator[T]
{
	override def cord = Cord ("abs(", operand.show, ")");
}

	
case class Minus[T] (
	override val lhs : Expression[T],
	override val rhs : Expression[T]
	)
	extends Expression[T]
		with BinaryOperator[T]
{
	override def cord = Cord ("(", lhs.show, ") - (", rhs.show, ")");
}


case class Modulo[T] (
	override val lhs : Expression[T],
	override val rhs : Expression[T]
	)
	extends Expression[T]
		with BinaryOperator[T]
{
	override def cord = Cord ("(", lhs.show, ") % (", rhs.show, ")");
}


case class Negate[T] (override val operand : Expression[T])
	extends Expression[T]
		with UnaryOperator[T]
{
	override def cord = Cord ("-(", operand.show, ")");
}


case class Plus[T] (
	override val lhs : Expression[T],
	override val rhs : Expression[T]
	)
	extends Expression[T]
		with BinaryOperator[T]
{
	override def cord = Cord ("(", lhs.show, ") + (", rhs.show, ")");
}


case class Power[T] (
	override val lhs : Expression[T],
	override val rhs : Expression[T]
	)
	extends Expression[T]
		with BinaryOperator[T]
{
	override def cord = Cord ("(", lhs.show, ") ** (", rhs.show, ")");
}


case class Quotient[T] (
	override val lhs : Expression[T],
	override val rhs : Expression[T]
	)
	extends Expression[T]
		with BinaryOperator[T]
{
	override def cord = Cord ("(", lhs.show, ") / (", rhs.show, ")");
}


case class Times[T] (
	override val lhs : Expression[T],
	override val rhs : Expression[T]
	)
	extends Expression[T]
		with BinaryOperator[T]
{
	override def cord = Cord ("(", lhs.show, ") * (", rhs.show, ")");
}

}
