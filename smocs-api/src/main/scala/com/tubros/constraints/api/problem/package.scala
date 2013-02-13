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
	implicit class EquationDefinition (val name : Symbol)
		extends AnyVal
	{
		def :=[T <: AnyVal] (constant : T) : Expression[T] =
			Assignment[T] (VariableUse (name), Constant (constant));
		
		
		def :=[T] (statement : Expression[T]) : Expression[T] =
			Assignment[T] (VariableUse (name), statement);
	}
}
