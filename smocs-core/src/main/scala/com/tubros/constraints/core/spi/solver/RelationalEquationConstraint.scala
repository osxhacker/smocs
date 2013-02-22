/**
 * Created on: Feb 13, 2013
 */
package com.tubros.constraints.core.spi.solver

import com.tubros.constraints.api.problem.Equation


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


trait RelationalEquationConstraintInstances
{
	implicit def constrainOrderedEquations[T : Ordering]
		: CanConstrain[Equation, T] =
		new RelationalEquationConstraint[T];
}


object RelationalEquationConstraint
{
	/// Class Types
	class DefaultRelationalConstraint[A] (
		override val equation : Equation[A]
		)
		(implicit override val ordering : Ordering[A])
		extends AbstractInterpretedConstraint[A]
			with RelationalConstraint[A]
}