/**
 * Created on: Jan 27, 2013
 */
package com.tubros.constraints.api
package problem

import scala.language.postfixOps

import scalaz._


/**
 * The '''Equation''' type
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
	lazy val tree : Tree[Expression] = apply;
	
	lazy val arity : Int = variables.size;
	
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

