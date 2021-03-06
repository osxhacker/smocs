/**
 * Created on: Jan 27, 2013
 */
package com.tubros.constraints.api
package problem

import scala.language.{
	implicitConversions,
	postfixOps
	}

import scalaz.{
	Cord,
	Show
    }


import ast.{
	Assignment,
	Constant,
	DerivedVariable,
	VariableUse
	}


/**
 * The '''Equation''' type provides the ability to define an AST-based
 * [[com.tubros.constraints.api.problem.Expression]] related to defining a
 * [[com.tubros.constraints.api.problem.Problem]].	Using this type, an
 * '''Equation''' can be defined in a natural way, such as:
 * 
 * {{{
 * val xBeforeY = new Equation {
 *		def apply = 'x < 'y;
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
	 * are involved in the definition of this '''Equation'''.  These include
	 * both references and `derived`
	 * [[com.tubros.constraints.api.VariableName]].
	 */
	lazy val arity : Int = variables.size + derived.size;
	
	/**
	 * The derived property indicates whether or not this '''Equation'''
	 * defines a ''derived value''.  Semantically, only one
	 * [[com.tubros.constraints.api.VariableName]] can be defined in an
	 * '''Equation''', hence it's type is an [[scala.Option]].
	 */
	lazy val derived : Option[VariableName] =
		derivedDefinition (expression);
	
	/**
	 * The variables property contains the [[scala.collection.Set]] of
	 * [[com.tubros.constraints.VariableName]]s used in this '''Equation'''.
	 */
	lazy val variables : Set[VariableName] =
		findVariables (expression).to[Set];
			
			
	/**
	 * The lit EDSL method allows '''Equation'''s to escape a literal value
	 * which may conflict with formulae definition.
	 */
	def lit[T] (value : T) = Constant[T] (value);
	
	
	/**
	 * The show method produces a [[scalaz.Cord]] representation of this
	 * '''Equation'''.
	 */
	def show (implicit SE : Show[Expression[T]]) : Cord =
		Cord (
			"Equation(",
            SE.show (expression),
            ")"
			);
	
	
	/**
	 * The shows method produces a `String` representation of this
	 * '''Equation'''.
	 */
	def shows (implicit SE : Show[Expression[T]]) : String =
		show toString;
	
	
	private def derivedDefinition (expr : Expression[T])
		: Option[VariableName] =
		expr match {
			case Assignment (DerivedVariable (derived), _) =>
				Some (derived);
				
			case _ =>
				None;
			}
	
	
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
	implicit def richSymbolSyntax (name : Symbol) =
		new Equation.RichSymbolSyntax (name);

	implicit def richVariableNameSyntax (name : VariableName) =
		new Equation.RichVariableNameSyntax (name);
}


object Equation
{
	/// Class Types
	class RichSymbolSyntax (v : Symbol)
		extends ArrayNamingPolicy
	{
		def apply (index : Int) = VariableUse (compose (v, index));
	}


	class RichVariableNameSyntax (v : VariableName)
		extends ArrayNamingPolicy
	{
		def apply (index : Int) = VariableUse (compose (v, index));
	}
}

