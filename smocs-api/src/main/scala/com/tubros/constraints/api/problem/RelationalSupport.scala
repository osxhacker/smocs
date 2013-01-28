/**
 * Created on: Jan 27, 2013
 */
package com.tubros.constraints.api
package problem

import scalaz.Tree


/**
 * The '''RelationalSupport''' type provides the
 * [[com.tubros.constraints.api.problem.Equation]] EDSL with logical ordering
 * style operators.
 *
 * @author svickers
 *
 */
trait RelationalSupport
	extends DefinitionFunctions
{
	/// Self Type Constraints
	this : Equation =>
	
	
	implicit class RelationalOps (val name : Symbol)
	{
		def === (statement : Expression) : Expression =
			expression ("==", name, statement);
		
		def !== (statement : Expression) : Expression =
			expression ("!=", name, statement);
		
		def < (statement : Expression) : Expression =
			expression ("<", name, statement);
		
		def <= (statement : Expression) : Expression =
			expression ("<=", name, statement);
		
		def > (statement : Expression) : Expression =
			expression (">", name, statement);
		
		def >= (statement : Expression) : Expression =
			expression (">=", name, statement);
	}
}
