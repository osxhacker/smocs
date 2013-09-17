/**
 * Created on: Feb 21, 2013G
 */
package com.tubros.constraints.core.internal

import scala.language.higherKinds

import scalaz._

import com.tubros.constraints.api.solver._
import com.tubros.constraints.core.spi.solver.runtime.AssignmentEnumerator


/**
 * The '''Searchable''' type defines a concrete type's ability to perform
 * the fundamental CSP solution operations needed to find answers.
 *
 * @author svickers
 *
 */
trait Searchable[A, +Repr, DomainT[X] <: Domain[X]]
{
	/// Class Types
	type LocationType
	final type VariableType = Variable[A, DomainT]
	type AssignmentGenerator = AssignmentEnumerator[A, Stream]
	
	
	/**
	 * The prune method eliminates all ''LocationType''s logically at and
	 * underneath the given '''location'''.  Figuratively, if the
	 * representation were a binary tree, this operation would "cut off" all
	 * nodes having '''location''' in their ancestry.  Note that this is
	 * just an example and should not be interpreted as requiring a tree-like
	 * implementation structure.
	 */
	def prune (location : LocationType) : Repr;
	
	
	/**
	 * The search method uses the given '''variables''' to expand 
	 */
	def search[M[_]] (
		variables : M[VariableType],
		choose : M[VariableType] => M[VariableType],
		valuesFor : AssignmentGenerator
		)
		(implicit FM : Foldable[M])
		: Option[Repr];
}
