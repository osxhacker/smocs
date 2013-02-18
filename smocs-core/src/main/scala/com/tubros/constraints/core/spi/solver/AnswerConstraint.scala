/**
 * Created on: Feb 17, 2013
 */
package com.tubros.constraints.core.spi.solver

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

import scalaz._

import com.tubros.constraints.api._

import solver.Answer
import solver.error._


/**
 * The '''AnswerConstraint''' type is a filter `class` used to apply ''global''
 * [[com.tubros.constraints.core.spi.solver.Constraint]]s to
 * [[com.tubros.constraints.api.solver.Answer]]s
 *
 * @author svickers
 *
 */
class AnswerConstraint[A, C[_]] (constraint : C[A] => Boolean)
	(implicit cbf : CanBuildFrom[Nothing, A, C[A]])
	extends Constraint[A]
{
	/// Instance Properties
	val variables = Set.empty[VariableName];
	
	
	override def apply (in : Env[A]) : SolverError \/ Env[A] =
	{
		if (constraint (in.values.to[C]))
			\/- (in);
		else
			-\/ (UnsolvableError);
	}
}