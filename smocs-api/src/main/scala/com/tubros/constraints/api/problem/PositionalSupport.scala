/**
 * Created on: Feb 18, 2013
 */
package com.tubros.constraints.api.problem

import scala.language.implicitConversions


/**
 * The '''PositionalSupport''' type provides the
 * [[com.tubros.constraints.api.problem.Equation]] EDSL with pseudo-arithmetic
 * style operators capable of stipulating where one
 * [[com.tubros.constraints.api.problem.Variable]] is in relation to another
 * in a potential solution.
 * 
 * So, for example, the following snippet:
 * 
 * {{{
 * new Equation[Foo] with PositionalSupport[Foo] {
 * 	def apply = 'a === 'b - 1;
 * 	}
 * }}}
 * 
 * would only be satisfied if `a` ''immediately'' preceded `b` in a conformant
 * CSP solution.
 * 
 * '''PositionalSupport''' can be thought of as an augmentation to
 * [[com.tubros.constraints.api.problem.RelationalSupport]] by way of allowing
 * ''distance'' relations to be expressed.
 *
 * @author svickers
 *
 */
trait PositionalSupport[T]
{
	/// Self Type Constraints
	this : Equation[T] =>
	
	
	/// Class Imports
	import ast._
	
	
	/// Class Types
	implicit class PositionalOps[U <% Expression[T]] (val lhs : U)
	{
		def + (rhs : Int) : Expression[T] = plus (lhs, rhs);
		def - (rhs : Int) : Expression[T] = minus (lhs, rhs);
	}
	
	
	def first : Expression[T] = new FirstPosition[T];
	
	def last : Expression[T] = new FirstPosition[T];
	
	def minus (lhs : Expression[T], offset : Int) : Expression[T] =
		Before (lhs, Offset[T] (offset));
	
	def plus (lhs : Expression[T], offset : Int) : Expression[T] =
		After (lhs, Offset[T] (offset));
}


package ast
{

import scalaz._

import syntax.show._


case class After[T] (
	override val lhs : Expression[T],
	override val rhs : Expression[T]
	)
	extends Expression[T]
		with BinaryOperator[T]
{
	override def cord = Cord ("(", lhs.show, ") after (", rhs.show, ")");
}


case class Before[T] (
	override val lhs : Expression[T],
	override val rhs : Expression[T]
	)
	extends Expression[T]
		with BinaryOperator[T]
{
	override def cord = Cord ("(", lhs.show, ") before (", rhs.show, ")");
}


case class Offset[T] (
	val value : Int
	)
	extends Expression[T]
{
	override def cord = Cord ("[", value.toString, "]");
}


case class FirstPosition[T] ()
	extends Expression[T]
{
	override def cord = Cord ("first");
}


case class LastPosition[T] ()
	extends Expression[T]
{
	override def cord = Cord ("last");
}

}
