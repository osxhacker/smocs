/**
 * Created on: Jan 25, 2013
 */
package com.tubros.constraints

import scala.language.implicitConversions

import scalaz._


/**
 * The '''api''' `package` and its constituents define the types involved
 * in specifying the Application Programming Interface (API) available for
 * defining and, ultimately, ''solving''
 * [[http://en.wikipedia.org/wiki/Constraint_satisfaction_problem CSP]]'s.
 *
 * @author svickers
 *
 */
package object api
{
	/// Class Imports
	
	
	/// Class Types
	/**
	 * The '''VariableName''' `type` is defined here, at the `package` level,
	 * so that it can participate in type signatures as a developer would
	 * expect:
	 * 
	 * {{{
	 * val foo : VariableName = 'someName
	 * }}}.
	 */
	type VariableName = Symbol @@ VariableNameTag
	
	
	/// Implicit Conversions
	implicit def symbolToVariableName (name : Symbol) : VariableName =
		VariableName (name);
}
