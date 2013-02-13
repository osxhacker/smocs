/**
 * Created on: Jan 27, 2013
 */
package com.tubros.constraints.api
package problem

import scala.language.implicitConversions

import scalaz._


/**
 * The '''Expression''' type defines an Abstract Syntax Tree (AST) construct
 * for creating [[com.tubros.constraints.api.problem.Equation]]s.
 *
 * @author svickers
 *
 */
trait Expression[+T]


object Expression
{
	/// Implicit Conversions
	implicit def expressionToShow[T] : Show[Expression[T]] =
		new Show[Expression[T]] {
			override def shows (expr : Expression[T]) : String =
				expr.toString;
	}
}


trait UnaryOperator[+T]
{
	/// Self Type Constraints
	this : Expression[T] =>
		
		
	/// Instance Properties
	val operand : Expression[T];
}


object UnaryOperator
{
	def unapply[T] (e : Expression[T]) : Option[Expression[T]] =
		e match {
			case uo : UnaryOperator[T] => Some (uo.operand);
			case _ => None;
			}
}


trait BinaryOperator[+T]
{
	/// Self Type Constraints
	this : Expression[T] =>
		
		
	/// Instance Properties
	val lhs : Expression[T];
	val rhs : Expression[T];
}


object BinaryOperator
{
	def unapply[T] (e : Expression[T])
		: Option[(Expression[T], Expression[T])] =
		e match {
			case bo : BinaryOperator[T] => Some (bo.lhs, bo.rhs);
			case _ => None;
			}
}


case class Assignment[T] (
	override val lhs : VariableUse,
	override val rhs : Expression[T]
	)
	extends Expression[T]
		with BinaryOperator[T]


case class VariableUse (name : VariableName)
	extends Expression[Nothing]


case class Constant[T] (value : T)
	extends Expression[T]
