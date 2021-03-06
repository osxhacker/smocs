/**
 * Created on: Jan 27, 2013
 */
package com.tubros.constraints.api

import scala.language.implicitConversions

import scalaz._

import com.tubros.constraints.api.problem.ast.{
	Assignment,
	Constant,
	VariableUse
	}


/**
 * The '''problem''' `package` provides the smocs API with types supporting the
 * creation of [[com.tubros.constraints.api.problem.Problem]]s.  A lot of
 * what's provided is an Embedded Domain Specific Language (EDSL) available
 * for use when defining a [[com.tubros.constraints.api.problem.Problem]].
 *
 * @author svickers
 *
 */
package object problem
{
	/// Implicit Conversions
	implicit def fastSymbolToVariableUse (name : FastSymbol)
		: Expression[Nothing] =
		VariableUse (name);
	
	implicit def symbolToVariableUse (name : Symbol) : Expression[Nothing] =
		VariableUse (name);

	implicit def valToConstant[T <: AnyVal] (v : T) : Expression[T] =
		Constant[T] (v);
}
