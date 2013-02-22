/**
 * Created on: Feb 18, 2013
 */
package com.tubros.constraints.core.spi.solver

import com.tubros.constraints.api.problem.Equation


/**
 * The '''PositionalEquationConstraint''' type is a ''type class'' which
 * provides a [[com.tubros.constraints.core.spi.solver.CanConstraint]] instance
 * capable of constraining any type ''A'' which has an [[scala.Ordering]] and
 * participates in [[com.tubros.constraints.api.problem.Equation]]s that have
 * [[com.tubros.constraints.api.problem.PositionalSupport]].
 *
 * @author svickers
 *
 */
class PositionalEquationConstraint[A] (implicit o : Ordering[A])
	extends CanConstrain[Equation, A]
{
	/// Class Imports
	import PositionalEquationConstraint._
	
	
	override def constrains (equation : Equation[A]) : Constraint[A] =
		new DefaultPositionalConstraint[A] (equation);
}

trait PositionalEquationConstraintInstances
{
	implicit def constrainOrderedEquation[T : Ordering]
		: CanConstrain[Equation, T] =
		new PositionalEquationConstraint[T];
}


object PositionalEquationConstraint
{
	/// Class Types
	private class DefaultPositionalConstraint[A] (
		override val equation : Equation[A]
		)
		(implicit override val ordering : Ordering[A])
		extends AbstractInterpretedConstraint[A]
			with RelationalConstraint[A]
			with PositionalConstraint[A]
}
