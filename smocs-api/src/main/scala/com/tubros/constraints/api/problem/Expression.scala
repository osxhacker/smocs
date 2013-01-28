/**
 * Created on: Jan 27, 2013
 */
package com.tubros.constraints.api
package problem

import scala.language.implicitConversions

import scalaz._
import scalaz.syntax.tree._


/**
 * The '''Expression''' type
 *
 * @author svickers
 *
 */
sealed trait Expression
{
	/// Instance Properties
	final protected val expr : Expression = this;
	
	def toTree : Tree[Expression];
	
	
	def /\ (other : Expression) : Expression = and (other);
	def && (other : Expression) : Expression = and (other);
	def \/ (other : Expression) : Expression = or (other);
	def || (other : Expression) : Expression = or (other);
	
	def and (other : Expression) : Expression = And (this, other);
	def or (other : Expression) : Expression = Or (this, other);
}


object Expression
{
	implicit def expressionAsTree (expr : Expression) : Tree[Expression] =
		expr.toTree;
}


case class Assignment (name : VariableName, statement : Expression)
	extends Expression
{
	override lazy val toTree = expr.node (
		VariableUse (name).toTree,
		statement.toTree
		);
}
	

case class VariableUse (name : VariableName)
	extends Expression
{
	override lazy val toTree = expr.leaf;
}


case class Constant[T] (value : T)
	extends Expression
{
	override lazy val toTree = expr.leaf;
}


case class UnaryOperator[T] (opcode : T, statement : Expression)
	extends Expression
{
	override lazy val toTree = expr.node (
		Constant (opcode).toTree,
		statement.toTree
		);
}

case class BinaryOperator[T] (opcode : T, lhs : Expression, rhs : Expression)
	extends Expression
{
	override lazy val toTree = expr.node (
		Constant (opcode).toTree,
		lhs.toTree,
		rhs.toTree
		);
}

case class And (lhs : Expression, rhs : Expression)
	extends Expression
{
	override lazy val toTree = expr.node (lhs.toTree, rhs.toTree);
}

case class Or (lhs : Expression, rhs : Expression)
	extends Expression
{
	override lazy val toTree = expr.node (lhs.toTree, rhs.toTree);
}
