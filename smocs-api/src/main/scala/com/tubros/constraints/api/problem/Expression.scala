/**
 * Created on: Jan 27, 2013
 */
package com.tubros.constraints.api
package problem

import scala.language.implicitConversions

import scalaz._
import scalaz.syntax.tree._


/**
 * The '''Expression''' type defines an Abstract Syntax Tree (AST) construct
 * for creating [[com.tubros.constraints.api.problem.Equation]]s.
 *
 * @author svickers
 *
 */
sealed trait Expression
{
	/// Instance Properties
	lazy val toTree : Tree[Expression] = this.leaf;
	
	
	def /\ (other : Expression) : Expression = and (other);
	def && (other : Expression) : Expression = and (other);
	def \/ (other : Expression) : Expression = or (other);
	def || (other : Expression) : Expression = or (other);
	
	def and (other : Expression) : Expression = LogicalAnd (this, other);
	def or (other : Expression) : Expression = LogicalOr (this, other);
}


object Expression
{
	/// Implicit Conversions
	implicit def expressionToTree (expr : Expression) : Tree[Expression] =
		expr.toTree;
	
	implicit def expressionToShow : Show[Expression] =
		new Show[Expression] {
			override def shows (expr : Expression) : String =
				expr.toString;
	}
}


case class Operator[T] (opcode : T)
	extends Expression
{
}


object Operator
{
	private[problem] def tree[T] (opcode : T, args : Expression *)
		: Tree[Expression] =
		Tree.node[Expression] (
			Operator (opcode),
			args.map (_.toTree).toStream
			);
}


case class Assignment (name : VariableName, statement : Expression)
	extends Expression
{
	override lazy val toTree = Operator.tree (
		":=",
		VariableUse (name),
		statement
		);
}
	

case class VariableUse (name : VariableName)
	extends Expression


case class Constant[T] (value : T)
	extends Expression


case class UnaryOperator[T] (opcode : T, statement : Expression)
	extends Expression
{
	override lazy val toTree = Operator.tree (opcode, statement);
}


case class BinaryOperator[T] (opcode : T, lhs : Expression, rhs : Expression)
	extends Expression
{
	override lazy val toTree = Operator.tree (opcode, lhs, rhs);
}


case class LogicalAnd (lhs : Expression, rhs : Expression)
	extends Expression
{
	override lazy val toTree = Operator.tree ("&&", lhs, rhs);
}


case class LogicalOr (lhs : Expression, rhs : Expression)
	extends Expression
{
	override lazy val toTree = Operator.tree ("||", lhs, rhs);
}
