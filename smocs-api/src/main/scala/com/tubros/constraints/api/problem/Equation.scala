/**
 * Created on: Jan 27, 2013
 */
package com.tubros.constraints.api
package problem

import scala.language.postfixOps

import scalaz._


/**
 * The '''Equation''' type provides the ability to define an AST-based
 * [[com.tubros.constraints.api.problem.Expression]] related to defining a
 * [[com.tubros.constraints.api.problem.Problem]].  Using this type, an
 * '''Equation''' can be defined in a natural way, such as:
 * 
 * {{{
 * val xBeforeY = new Equation {
 * 		def apply = 'x < 'y
 * 		};
 * }}}
 *
 * @author svickers
 *
 */
trait Equation
	extends (() => Tree[Expression])
{
	/// Class Imports
	import std.list._
	
	
	/// Instance Properties
	/**
	 * The tree property uses the concrete type's `apply` definition to cache
	 * the [[scalaz.Tree]] definition.
	 */
	lazy val tree : Tree[Expression] = apply;
	
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
		tree.foldMap {
			_ match {
				case VariableUse (name) =>
					List (name);
					
				case _ =>
					List.empty;
				}
			} toSet;
}

