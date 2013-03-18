/**
 * Created on: Feb 28, 2013
 */
package com.tubros.constraints.core.spi.solver.heuristic

import scala.language.higherKinds

import scalaz._

import com.tubros.constraints.api.VariableName
import com.tubros.constraints.api.solver._
import com.tubros.constraints.core.spi.solver._

import runtime._


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
case class ConstraintPropagation[A, DomainT[X] <: Domain[X]] (
	private val provider : ConstraintProvider[A],
	private val symbolTable : SymbolTable
	)
	extends ((Iterable[Answer[A]], Variable[A, DomainT]) => Variable[A, DomainT])
{
	override def apply (
		assignments : Iterable[Answer[A]],
		variable : Variable[A, DomainT]
		)
		: Variable[A, DomainT] =
	{
		val available = assignments.map (_.name).to[Set] + variable.name;
		val applicableConstraints = provider.constraintsFor (available);
		val priorVariables = evaluate (
			assignments.map (_.toTuple).toMap,
			variable.name
			) _;
		
		variable.filter {
			value =>
				
			applicableConstraints.forall (priorVariables (value));
			}
	}
	
	
	def apply (variable : Variable[A, DomainT]) : Variable[A, DomainT] =
		apply (Seq.empty[Answer[A]], variable);
	
	
	private def evaluate (variables : Map[VariableName, A], name : VariableName)
		(value : A)
		(constraint : Constraint[A])
		: Boolean =
		constraint (variables updated (name, value)).isRight;
}
