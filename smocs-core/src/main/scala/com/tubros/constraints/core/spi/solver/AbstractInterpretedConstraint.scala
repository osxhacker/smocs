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
	import scalaz.std.AllInstances._
	import scalaz.syntax.id._
	import scalaz.syntax.monad._
	import scalaz.syntax.std.boolean._
	import scalaz.syntax.std.option._
	
	
	/// Instance Properties
	override val derived : Option[VariableName] = None;
	val equation : Equation[A];
	override val variables = equation.variables;
	
	
	override def apply (input : Map[VariableName, A]) : ConstraintResult[A] =
		variablesUsed (input) flatMap (evaluate);
		

	override protected def interpreter
		: Env[A] => PartialFunction[Expression[A], InterpretedResult[A]] =
		env => _ match {
			case VariableUse (x) => InterpretedResult.right (env (x).some);
			case Constant (c) => InterpretedResult.right (c.some);
			}
		
		
	protected def postProcessResult (result : A, vars : Env[A]) : Env[A] =
		vars;
	
	
	private def variablesUsed (all : Map[VariableName, A])
		: ConstraintResult[A] =
	{
		val missing = variables &~ all.keySet;
		
		missing.isEmpty.fold (
			\/- (all),
			-\/ (MissingVariables (missing))
			);
	}
	
	
	private def evaluate (vars : Map[VariableName, A])
		: ConstraintResult[A] =
		interpreter (vars) (equation.expression).run match {
			case Some (-\/ (true)) => vars.right;
			case Some (\/- (result)) => postProcessResult (result, vars).right;
			case _ => -\/ (UnsolvableError);
			}
}
