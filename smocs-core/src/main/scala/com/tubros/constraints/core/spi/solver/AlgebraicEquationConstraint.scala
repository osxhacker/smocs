/**
 * Created on: Feb 8, 2013
 */
package com.tubros.constraints.core.spi.solver

import scalaz.{
	Ordering => _,
	_
	}

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
	extends CanConstrain[Equation, A]
{
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
		extends AbstractInterpretedConstraint[A]
			with AlgebraicConstraint[A]
			with RelationalConstraint[A]
	{
		override protected def interpreter
			: Env[A] => PartialFunction[Expression[A], Result[A]] =
			env => (super.interpreter (env) orElse numericOps (env));
			
			
		protected def numericOps (env : Env[A])
			: PartialFunction[Expression[A], Result[A]];
	}
	
	
	private[solver] class FractionalConstraint[A] (
		override val equation : Equation[A]
		)
		(implicit
			override val numeric : Numeric[A],
			override val ordering : Ordering[A],
			fn : Fractional[A]
		)
		extends AbstractAlgebraicConstraint[A] (equation)
	{
		override protected def numericOps (env : Env[A])
			: PartialFunction[Expression[A], Result[A]] =
			_ match {
				case Quotient (n, d) =>
					eval (env) (n, d) map ((fn.div _).tupled);
				}
	}
	
	private[solver] class IntegralConstraint[A] (
		override val equation : Equation[A]
		)
		(implicit
			override val numeric : Numeric[A],
			override val ordering : Ordering[A],
			in : Integral[A]
		)
		extends AbstractAlgebraicConstraint[A] (equation)
	{
		override protected def numericOps (env : Env[A])
			: PartialFunction[Expression[A], Result[A]] =
			_ match {
				case Quotient (n, d) =>
					eval (env) (n, d) map ((in.quot _).tupled);
				}
	}
}
