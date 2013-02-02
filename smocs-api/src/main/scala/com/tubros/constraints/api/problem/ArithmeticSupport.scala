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
		def + (statement : Expression) : Expression = add (statement);
		def - (statement : Expression) : Expression = sub (statement);
		def / (statement : Expression) : Expression = div (statement);
		def % (statement : Expression) : Expression = mod (statement);
		def * (statement : Expression) : Expression = mult (statement);
		def ^ (statement : Expression) : Expression = pow (statement);
			
		
		def add (statement : Expression) : Expression =
			expression ("*", name, statement);

		def sub (statement : Expression) : Expression =
			expression ("-", name, statement);

		def div (statement : Expression) : Expression =
			expression ("/", name, statement);

		def mod (statement : Expression) : Expression =
			expression ("%", name, statement);
		
		def mult (statement : Expression) : Expression =
			expression ("*", name, statement);

		def pow (statement : Expression) : Expression =
			expression ("^", name, statement);
	}
}
