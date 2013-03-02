/**
 * Created on: Feb 25, 2013
 */
package com.tubros.constraints.core.internal

import scala.language.higherKinds

import scalaz._

import com.tubros.constraints.api.solver._


/**
 * The '''SolutionSpace''' type codifies the concept of a CSP's possible
 * solutions, with the universe of values not necessarily known at the outset
 * of searching.
 *
 * @author svickers
 *
 */
trait SolutionSpace[A, M[_], DomainT[X] <: Domain[X]]
	extends Searchable[A, SolutionSpace[A, M, DomainT], DomainT]
{
	/// Class Types
	type NodeType[T] <: SolutionSpace.Node[T]
	type ThisType = SolutionSpace[A, M, DomainT]
	
	
	/// Instance Properties
	def frontier : Frontier[NodeType[A]];
	
	
	/**
	 * The expand method attempts to increase the known '''SolutionSpace''' by
	 * using the '''variables''' and '''valuesFor''' them to add to the space.
	 * It is both supported ''and'' expected that either of those two can be
	 * filtered out such that their introduction to the '''SolutionSpace'''
	 * is blocked.
	 */
	def expand[M[+_]] (
		location : LocationType,
		variables : M[VariableType],
		valuesFor : ValueGenerator
		)
		(implicit fm : Foldable[M])
		: ThisType;
}

object SolutionSpace
{
	/// Class Types
	trait Node[A]
	{
		/// Instance Properties
		def assignments : Seq[Answer[A]]
	}
}
