/**
 * Created on: Jan 27, 2013
 */
package com.tubros.constraints.api
package problem

import scala.language.{
	implicitConversions,
	postfixOps
	}


/**
 * The '''Equation''' type provides the ability to define an AST-based
 * [[com.tubros.constraints.api.problem.Expression]] related to defining a
 * [[com.tubros.constraints.api.problem.Problem]].	Using this type, an
 * '''Equation''' can be defined in a natural way, such as:
 * 
 * {{{
 * val xBeforeY = new Equation {
 *		def apply = 'x < 'y
 *		};
 * }}}
 *
 * @author svickers
 *
 */
trait Equation[T]
	extends (() => Expression[T])
{
	/// Instance Properties
	/**
	 * The expression property uses the concrete type's `apply` definition to
	 * cache the [[com.tubros.constraints.api.problem.Expression]] definition.
	 */
	lazy val expression : Expression[T] = apply;
	
	/**
	 * The arity property lets callers know how many ''distinct'' `variables`
	 * are involved in the definition of this '''Equation'''.
	 */
	lazy val arity : Int = variables.size;
	
	/**
	 * The variables property contains the [[scala.Set]] of
	 * [[com.tubros.constraints.VariableName]]s used in this '''Equation'''.
	 */
	lazy val variables : Set[VariableName] =
		findVariables (expression).to[Set];
			
			
	private def findVariables (expr : Expression[T]) : List[VariableName] =
		expr match {
			case VariableUse (name) =>
				name :: Nil;
				
			case UnaryOperator (operand) =>
				findVariables (operand);
				
			case BinaryOperator (lhs, rhs) =>
				findVariables (lhs) ::: findVariables (rhs);
				
			case _ =>
				List.empty;
			}
	
	
	/// Implicit Conversions
	implicit def valToConstant[T <: AnyVal] (v : T) : Expression[T] =
		Constant[T] (v);
	
	implicit def symbolToVariableUse (name : Symbol) : Expression[Nothing] =
		VariableUse (name);
}

