/**
 * Created on: Feb 13, 2013
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
 * The '''RelationalEquationConstraint''' type is a ''type class'' which
 * provides a [[com.tubros.constraints.core.spi.solver.CanConstraint]] instance
 * capable of constraining any type ''A'' which has an [[scala.Ordering]].
 *
 * @author svickers
 *
 */
class RelationalEquationConstraint[A : Ordering]
	extends CanConstrain[Equation, A]
{
	/// Class Imports
	import RelationalEquationConstraint._
	
	
	override def constrains (equation : Equation[A]) : Constraint[A] =
		new DefaultRelationalConstraint[A] (equation);
}


object RelationalEquationConstraint
{
	/// Class Imports
	import Scalaz._
	
	
	/// Class Types
	sealed abstract class AbstractRelationalConstraint[A] (
		val equation : Equation[A]
		)
		extends AbstractInterpretedConstraint[A]
	{
		override protected def interpreter
			: Env[A] => PartialFunction[Expression[A], Result[A]] =
			env => _ match {
				case VariableUse (x) => env (x).point[Result];
				case Constant (c) => c.point[Result];
				}
	}
	
	
	class DefaultRelationalConstraint[A] (
		override val equation : Equation[A]
		)
		(implicit override val ordering : Ordering[A])
		extends AbstractRelationalConstraint[A] (equation)
			with RelationalConstraint[A]
}