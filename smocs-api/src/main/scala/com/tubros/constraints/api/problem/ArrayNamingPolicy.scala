/**
 * Created on: Mar 5, 2013
 */
package com.tubros.constraints.api
package problem

import scalaz._


/**
 * The '''ArrayNamingPolicy''' type serves as a single point of truth for how
 * a [[com.tubros.constraints.api.VariableName]] is created when the name
 * references an EDSL [[scala.Array]]-style
 * [[com.tubros.constraints.api.VariableName]].
 *
 * @author svickers
 *
 */
trait ArrayNamingPolicy
{
	/// Class Imports
	import std.option._
	import syntax.std.boolean._
	import ArrayNamingPolicy._
	
	
	/**
	 * The compose method creates a [[com.tubros.constraints.api.VariableName]]
	 * from the given '''root''' and the desired '''index'''.
	 */
	def compose (root : VariableName, index : Int) : VariableName =
		VariableName (root.name.toString + separator + index.toString);
	
	
	/**
	 * The decompose method attempts to break apart the '''name''' based on
	 * the '''ArrayNamingPolicy''' defined by this `trait`.
	 */
	def decompose (name : VariableName) : Option[(VariableName, Int)] =
		isArrayName (name).option {
			val parts = name.name.split (separator);
			
			(VariableName (parts (0)), parts (1).toInt);
			}
		
	
	/**
	 * The isArrayName determines whether or not the '''candidate''' is a
	 * [[com.tubros.constraints.api.VariableName]] previously `compose`d by
	 * this policy type.
	 */
	def isArrayName (candidate : VariableName) : Boolean =
		candidate.name.contains (separator);
	
	
	/**
	 * The whenArrayName method allows for higher-ordered functions to be
	 * applied when the '''candidate''' name has been `compose`d by this
	 * policy.
	 */
	def whenArrayName[R] (candidate : VariableName)
		(block : (VariableName, Int) => R)
		: Option[R] =
		decompose (candidate) map (block.tupled);
	
	
	/**
	 * The unlessArrayName method performs the opposite role of
	 * `whenArrayName` by only executing the '''block''' when the
	 * '''candidate''' name is ''not'' an array name.
	 */
	def unlessArrayName[R] (candidate : VariableName)
		(block : (VariableName) => R)
		: Option[R] =
		isArrayName (candidate) ? none[R] | Option (block (candidate));
}


object ArrayNamingPolicy
{
	/// Instance Properties
	private val separator = "__$ub$cript__";
}
