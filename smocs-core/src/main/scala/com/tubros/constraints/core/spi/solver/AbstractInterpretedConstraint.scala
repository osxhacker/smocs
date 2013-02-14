/**
 * Created on: Feb 13, 2013
 */
package com.tubros.constraints.core.spi.solver

import scalaz._

import com.tubros.constraints.api._
import com.tubros.constraints.api.problem.{
	Equation,
	Expression
	}
import com.tubros.constraints.api.problem.ast._
import com.tubros.constraints.api.solver._
import com.tubros.constraints.api.solver.error._


/**
 * The '''AbstractInterpretedConstraint''' type serves as a mix-in `trait`
 * used to reduce duplication when implementing an
 * [[com.tubros.constraints.core.spi.solver.Interpreted]]
 * [[com.tubros.constraints.core.spi.solver.Constraint]]
 *
 * @author svickers
 *
 */
trait AbstractInterpretedConstraint[A]
	extends Constraint[A]
		with Interpreted[A]
{
	/// Class Imports
	import scalaz.syntax.id._
	import scalaz.syntax.monad._
	import scalaz.syntax.std.boolean._
	
	
	/// Instance Properties
	val equation : Equation[A];
	override val variables = equation.variables;
	
	
	override def apply (input : Map[VariableName, A])
		: SolverError \/ Map[VariableName, A] =
		variablesUsed (input) flatMap (evaluate);
		

	override protected def interpreter
		: Env[A] => PartialFunction[Expression[A], Result[A]] =
		env => _ match {
			case VariableUse (x) => env (x).point[Result];
			case Constant (c) => c.point[Result];
			}
		
		
	private def variablesUsed (all : Map[VariableName, A])
		: SolverError \/ Map[VariableName, A] =
	{
		val missing = variables &~ all.keySet;
		
		missing.isEmpty.fold (
			\/- (all),
			-\/ (MissingVariables (missing))
			);
	}
	
	
	private def evaluate (vars : Map[VariableName, A])
		: SolverError \/ Map[VariableName, A] =
		interpreter (vars) (equation.expression) match {
			case -\/ (true) => vars.right;
			case \/- (_) => vars.right;
			case _ => -\/ (UnsolvableError);
			}
}