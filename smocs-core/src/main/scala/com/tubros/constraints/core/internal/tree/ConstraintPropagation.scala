/**
 * Created on: Feb 28, 2013
 */
package com.tubros.constraints.core.internal.tree

import scala.language.higherKinds

import scalaz._

import com.tubros.constraints.api.VariableName
import com.tubros.constraints.api.solver._
import com.tubros.constraints.core.spi.solver._


/**
 * The '''ConstraintPropagation''' type is a functor which reduces the
 * [[com.tubros.constraints.api.solver.Domain]] of a candidate
 * [[com.tubros.constraints.api.solver.Variable]] based on the
 * [[com.tubros.constraints.core.spi.solver.Constraint]]s ''applicable'' to
 * the available (known) variables.
 *
 * @author svickers
 *
 */
class ConstraintPropagation[A, DomainT[X] <: Domain[X]] (
	val constraints : Set[Constraint[A]]
	)
	extends ((Seq[Answer[A]], Variable[A, DomainT]) => Vector[A])
{
	override def apply (
		assignments : Seq[Answer[A]],
		variable : Variable[A, DomainT]
		)
		: Vector[A] =
	{
		val available = assignments.map (_.name).to[Set] + variable.name;
		val applicableConstraints = constraints.filter (
			_.isDefinedAt (available)
			);
		val priorVariables = evaluate (
			assignments.map (_.toTuple).toMap,
			variable.name
			) _;
		
		variable.domain.to[Vector] filter {
			value =>
				
			applicableConstraints.forall (priorVariables (value));
			}
	}
	
	
	private def evaluate (variables : Map[VariableName, A], name : VariableName)
		(value : A)
		(constraint : Constraint[A])
		: Boolean =
		constraint (variables updated (name, value)).isRight;
}
