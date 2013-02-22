/**
 * Created on: Feb 22, 2013
 */
package com.tubros.constraints.core.internal

import com.tubros.constraints.api.problem._
import com.tubros.constraints.api.solver._
import com.tubros.constraints.core.spi.solver._


/**
 * The '''VariableStore''' type is an `internal` type used to manage
 * [[com.tubros.contraints.api.solver.Variable]]s and the
 * [[com.tubros.constraints.core.spi.solver.Constraint]]s placed on them for
 * various [[com.tubros.contraints.api.solver.Solver]]s.
 *
 * @author svickers
 *
 */
final case class VariableStore[A] (
	variables : Vector[Variable[A, DiscreteDomain]],
	constraints : Set[Constraint[A]],
	answerFilters : Set[Constraint[A]]
	)
{
	def addAnswerFilter (entry : Constraint[A]) =
		copy (answerFilters = answerFilters + entry);
	
	def addConstraint (entry : Constraint[A]) =
		copy (constraints = constraints + entry);
		
	def addVariable (entry : DiscreteVariable[A]) =
		copy (variables = variables :+ entry);
	
	def addVariables (entries : Seq[DiscreteVariable[A]]) =
		copy (variables = variables ++ entries);
}


object VariableStore
{
	def empty[A] = new VariableStore[A] (
		variables = Vector.empty,
		constraints = Set.empty,
		answerFilters = Set.empty
		);
}
