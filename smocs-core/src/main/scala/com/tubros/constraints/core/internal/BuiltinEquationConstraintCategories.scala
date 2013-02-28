/**
 * Created on: Feb 24, 2013
 */
package com.tubros.constraints.core.internal

import com.tubros.constraints.core.spi.solver._


/**
 * The '''BuiltinEquationConstraintCategories''' type provides `package` objects
 * with a one-stop-shopping place to support the builtin
 * [[com.tubros.constraints.api.problem.Equation]]
 * [[com.tubros.constraints.core.spi.solver.CanConstrain]] objects without
 * having to duplicate their definition throughout `internal`.
 *
 * @author svickers
 *
 */
trait BuiltinEquationConstraintCategories
{
	object algebraic
		extends AlgebraicEquationConstraintInstances
	
		
	object relational
		extends RelationalEquationConstraintInstances
	
		
	object positional
		extends PositionalEquationConstraintInstances
}
