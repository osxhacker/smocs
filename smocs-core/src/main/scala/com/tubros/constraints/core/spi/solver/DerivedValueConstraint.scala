/**
 * Created on: Mar 19, 2013
 */
package com.tubros.constraints.core.spi.solver

import scalaz._

import com.tubros.constraints.api._

import problem._
import ast._


/**
 * The '''DerivedValueConstraint''' type provides
 * [[com.tubros.constraints.core.spi.solver.Interpreted]] execution for
 * producing an [[com.tubros.constraints.api.problem.Answer]] based on the
 * value produced from the [[com.tubros.constraints.core.spi.solver.Constraint]]
 * definition.
 * 
 * It is a "stackable trait" and is conditionally used when an
 * [[com.tubros.constraints.api.problem.Equation]] provides a definition.
 *
 * @author svickers
 *
 */
trait DerivedValueConstraint[A]
	extends AbstractInterpretedConstraint[A]
{
	/// Self Type Constraints
	this : Constraint[A] =>
		
		
	/// Class Imports
	import syntax.std.option._
	
	
	override protected def postProcessResult (result : A, vars : Env[A])
		: Env[A] =
		derived.map (derivedName => vars updated (derivedName, result)) | vars;
	
	
	abstract override protected def interpreter
		: Env[A] => PartialFunction[Expression[A], InterpretedResult[A]] =
		env => (super.interpreter (env) orElse derivedValueInterpreter (env));
			
		
	private def derivedValueInterpreter
		: Env[A] => PartialFunction[Expression[A], InterpretedResult[A]] =
	{
		env =>
			
		_ match {
			case Assignment (DerivedVariable (_), expr) =>
				interpreter (env) (expr);
			}
	}
}
