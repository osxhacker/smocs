/**
 * Created on: Feb 8, 2013
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
 * The Algebraic Equation Constraint types are ''type classes'' which
 * allow arithmetic operations when evaluating its
 * [[com.tubros.constraints.core.spi.solver.Constraint]].
 *
 * @author svickers
 *
 */
class FractionalAlgebraicEquationConstraint[A : Numeric : Fractional]
	extends CanConstrain[Equation, A] {
	/// Class Imports
	import AlgebraicEquationConstraint._
	
	
	override def constrains (equation : Equation[A]) : Constraint[A] =
		new FractionalConstraint (equation);
}


class IntegralAlgebraicEquationConstraint[A : Numeric : Integral]
	extends CanConstrain[Equation, A] {
	/// Class Imports
	import AlgebraicEquationConstraint._
	
	
	override def constrains (equation : Equation[A]) : Constraint[A] =
		new IntegralConstraint (equation);
}


object AlgebraicEquationConstraint
{
	/// Class Imports
	import Scalaz._
	
	
	/// Class Types
	sealed abstract class AbstractAlgebraicConstraint[A] (
		val equation : Equation[A]
		)
		extends Constraint[A]
			with Interpreted[A]
	{
		/// Instance Properties
		override val variables = equation.variables;
		
		
		override def apply (input : Map[VariableName, A])
			: SolverError \/ Map[VariableName, A] =
			variablesUsed (input) flatMap (evaluate);
		
		
		override protected def interpreter
			: Env[A] => PartialFunction[Expression[A], Result[A]] =
			env => numericOps (env);
			
			
		protected def numericOps (env : Env[A])
			: PartialFunction[Expression[A], Result[A]];
		
		
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
	
	
	private[solver] class FractionalConstraint[A] (
		override val equation : Equation[A]
		)
		(implicit
			override val numeric : Numeric[A],
			override val ordering : scala.Ordering[A],
			fn : Fractional[A]
		)
		extends AbstractAlgebraicConstraint[A] (equation)
			with AlgebraicConstraint[A]
			with RelationalConstraint[A]
	{
		override protected def numericOps (env : Env[A])
			: PartialFunction[Expression[A], Result[A]] =
			_ match {
				case Quotient (n, d) =>
					for {
						num <- interpreter (env) (n)
						denom <- interpreter (env) (d)
						} yield fn.div (num, denom);
				}
	}
	
	private[solver] class IntegralConstraint[A] (
		override val equation : Equation[A]
		)
		(implicit
			override val numeric : Numeric[A],
			override val ordering : scala.Ordering[A],
			in : Integral[A]
		)
		extends AbstractAlgebraicConstraint[A] (equation)
			with AlgebraicConstraint[A]
			with RelationalConstraint[A]
	{
		override protected def numericOps (env : Env[A])
			: PartialFunction[Expression[A], Result[A]] =
			_ match {
				case Quotient (n, d) =>
					for {
						num <- interpreter (env) (n)
						denom <- interpreter (env) (d)
						} yield in.quot (num, denom);
				}
	}
}
