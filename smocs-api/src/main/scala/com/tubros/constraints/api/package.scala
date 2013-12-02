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
 * There are three fundamental concepts involved in a CSP:
 * 
 * $ - variables: the objects a CSP is seeking to find values from their domain
 * $ - domains: valid values each variable can have (can differ per variable)
 * $ - constraints: restrictions on variables and domains
 *
 * Since each variable can have a different domain of values, the
 * '''Variable''' type captures the allowable `domain` ''for that instance''.
 * 
 * To facilitate modularity, the API is roughly divided into two
 * sub-`package`s.  First, the [[com.tubros.constraints.api.problem]] `package`
 * defines the ability to ''formulate'' a problem definition.  Second, the
 * [[com.tubros.constraints.api.solver]] `package` defines the types involved
 * in ''solving'' an arbitrary [[com.tubros.constraints.api.problem.Problem]].
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
	
	implicit val variableNameEqual : Equal[VariableName] = Equal.equalA;
}
