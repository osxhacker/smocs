/**
 * Created on: Mar 9, 2013
 */
package com.tubros.constraints.api
package problem


/**
 * The '''DerivedValueSupport''' type adds the ability to define new symbolic
 * variables within an [[com.tubros.constraints.api.problem.Equation]].  For
 * example, given an [[com.tubros.constraints.api.problem.Equation]] of:
 * 
 * {{{
 * 		trait SomeEquation
 *   		extends Equation[Int]
 *     			with ArithmeticSupport[Int]
 *        		with DerivedValueSupport[Int]
 *          
 * 		val e = new SomeEquation {
 *   		def apply = ('a * 'b) ** 2;
 *   		}
 * }}}
 * 
 * A ''derived value'' could be created by:
 * 
 * {{{
 * 		val derived = new SomeEquation {
 *   		def apply 'derived := 'a + 'b;
 *   		}
 * }}}
 *
 * In this example, a [[com.tubros.constraints.api.solver.Solver]] would be
 * expected to produce [[com.tubros.constraints.api.solver.Answer]]s containing
 * `'a`, `'b`, and `'derived`.
 * 
 * @author svickers
 *
 */
trait DerivedValueSupport[T]
{
	/// Self Type Constraints
	this : Equation[T] =>
	
	
	/// Class Imports
	import ast._
	
	
	/// Class Types
	implicit class DerivedValueOps (val name : Symbol)
	{
		def :=[T <: AnyVal] (constant : T) : Expression[T] =
			Assignment[T] (DerivedVariable (name), Constant (constant));
		
		
		def :=[T] (statement : Expression[T]) : Expression[T] =
			Assignment[T] (DerivedVariable (name), statement);
	}
}
