/**
 * Created on: Jan 25, 2013
 */
package com.tubros.constraints.api.problem

import scalaz.NonEmptyList


/**
 * The '''Problem''' type defines the representation and functionality
 * intrinsically available in a '''Problem''' definition.  There are three
 * fundamental concepts involved in a CSP:
 * 
 * $ - variables: the objects a CSP is seeking to find values from their domain
 * $ - domains: valid values each variable can have (can differ per variable)
 * $ - constraints: restrictions on variables and domains
 *
 * Since each variable can have a different domain of values, the
 * '''Variable''' type captures the allowable `domain` ''for that instance''.
 *
 * @author svickers
 *
 */
case class Problem[DomainT] (
	val equations : NonEmptyList[Equation]
	)
{

}


object Problem
{
	/// Implicit Definitions
}