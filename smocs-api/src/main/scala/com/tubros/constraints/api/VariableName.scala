/**
 * Created on: Jan 25, 2013
 */
package com.tubros.constraints.api

import scalaz._


/**
 * The '''VariableNameTag''' type is a simple [[scalaz.Tagged]] construct which
 * disambiguates [[scala.Symbol]]s from their use in
 * [[com.tubros.constraints.api.Problem]] definitions.
 *
 * @author svickers
 *
 */
sealed trait VariableNameTag


object VariableName
{
	def apply (name : Symbol) : VariableName =
		Tag[Symbol, VariableNameTag] (name);
	
	
	def apply (name : String) : VariableName = apply (Symbol (name));
}
