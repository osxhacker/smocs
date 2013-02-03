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
trait Expression


object Expression
{
	/// Implicit Conversions
	implicit def expressionToShow : Show[Expression] =
		new Show[Expression] {
			override def shows (expr : Expression) : String =
				expr.toString;
	}
}


trait UnaryOperator
{
	/// Self Type Constraints
	this : Expression =>
		
		
	/// Instance Properties
	val operand : Expression;
}

trait BinaryOperator
{
	/// Self Type Constraints
	this : Expression =>
		
		
	/// Instance Properties
	val lhs : Expression;
	val rhs : Expression;
}

case class Assignment (
	override val lhs : VariableUse,
	override val rhs : Expression
	)
	extends Expression
		with BinaryOperator

case class VariableUse (name : VariableName)
	extends Expression

case class Constant[T] (value : T)
	extends Expression
