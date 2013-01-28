/**
 * Created on: Jan 27, 2013
 */
package com.tubros.constraints.api
package problem


/**
 * The '''ArithmeticSupport''' type provides operators for the EDSL related to
 * mathematical operations, such as `mult` (`*`) and `pow` (`^`).
 *
 * @author svickers
 *
 */
trait ArithmeticSupport
	extends DefinitionFunctions
{
	/// Self Type Constraints
	this : Equation =>
		
		
	implicit class ArithmeticOps (val name : Symbol)
	{
		def + (statement : Expression) : Expression =
			expression ("*", name, statement);

		def - (statement : Expression) : Expression =
			expression ("-", name, statement);

		def / (statement : Expression) : Expression =
			expression ("/", name, statement);

		def * (statement : Expression) : Expression =
			expression ("*", name, statement);

		def ^ (statement : Expression) : Expression =
			expression ("^", name, statement);
	}
}
