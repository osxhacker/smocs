/**
 * Created on: Feb 4, 2013
 */
package com.tubros.constraints.core.internal

import com.tubros.constraints.api.problem.Equation
import com.tubros.constraints.core.spi.solver._


/**
 * The '''simple''' `package` provides types/instances relevant when using the
 * [[com.tubros.constraints.api.solver.Solver]]s defined herein.  These
 * [[com.tubros.constraints.api.solver.Solver]]s are typically ''not'' meant
 * for use in environments needing optimal time and/or space performance.
 * 
 * What they do provide are relatively low complexity collaborations.  The idea
 * behind providing them is to foster a learning environment.
 *
 * @author svickers
 *
 */
package object simple
{
	/// Class Types
	object algebraic
	{
		implicit def constrainFractionalEquations[T : Numeric : Fractional]
			: CanConstrain[Equation, T] =
			new FractionalAlgebraicEquationConstraint[T];
		
		
		implicit def constrainIntegralEquations[T : Numeric : Integral]
			: CanConstrain[Equation, T] =
			new IntegralAlgebraicEquationConstraint[T];
	}
	
	/// Instance Properties
	val constraints = ToCanConstrainOps;
	
	
	/// Implicit Conversions
}
