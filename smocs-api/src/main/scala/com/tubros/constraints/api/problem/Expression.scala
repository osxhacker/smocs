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
{
	/// Instance Properties
	implicit def cord : Cord;
}


object Expression
{
	/// Implicit Conversions
	implicit def expressionToShow[T] : Show[Expression[T]] =
		new Show[Expression[T]] {
			override def show (expr : Expression[T]) : Cord =
				expr.cord;
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


package ast
{
	
case class Assignment[T] (
	override val lhs : DerivedVariable,
	override val rhs : Expression[T]
	)
	extends Expression[T]
		with BinaryOperator[T]
{
	override def cord = Cord (lhs.cord, ":=", rhs.cord);
}


case class DerivedVariable (name : VariableName)
	extends Expression[Nothing]
{
	override def cord = Cord (name.toString);
}


case class VariableUse (name : VariableName)
	extends Expression[Nothing]
{
	override def cord = Cord (name.toString);
}


case class Constant[T] (value : T)
	extends Expression[T]
{
	override def cord = Cord (value.toString);
}

}