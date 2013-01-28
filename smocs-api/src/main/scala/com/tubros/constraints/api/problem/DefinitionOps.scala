/**
 * Created on: Jan 27, 2013
 */
package com.tubros.constraints.api
package problem


/**
 * The '''DefinitionFunctions''' type provides common support methods used
 * when creating EDSL operators/methods.
 *
 * @author svickers
 *
 */
trait DefinitionFunctions
{
	/// Self Type Constraints
	this : Equation =>


	protected def expression[T] ( op : T, name : VariableName)
		: UnaryOperator[T] =
		UnaryOperator (op, VariableUse (name));


	protected def expression[T] (
		op : T,
		name : VariableName,
		rhs : Expression
		)
		: BinaryOperator[T] =
		BinaryOperator (op, VariableUse (name), rhs);
}