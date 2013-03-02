/**
 * Created on: Feb 21, 2013G
 */
package com.tubros.constraints.core.internal

import scala.language.higherKinds

import scalaz._

import com.tubros.constraints.api.solver._


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
	type ValueGenerator = (Seq[Answer[A]], VariableType) => VariableType
	
	
	def prune (location : LocationType) : Repr;
	
	
	def search[M[+_]] (
		location : LocationType,
		variables : M[VariableType],
		choose : M[VariableType] => M[VariableType],
		valuesFor : ValueGenerator
		)
		(implicit fm : Foldable[M])
		: Repr;
}
