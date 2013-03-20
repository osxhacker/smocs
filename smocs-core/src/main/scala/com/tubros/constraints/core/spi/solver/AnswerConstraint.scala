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
 * The '''AnswerConstraint''' type uses a client-provided
 * [[scala.PartialFunciton]] to determine whether or not the ''Env'' should
 * be allowed to participate in the solution.
 *
 * @author svickers
 *
 */
class AnswerConstraint[A] (
	private val pf : PartialFunction[Seq[Answer[A]], Boolean]
	)
	extends Constraint[A]
{
	/// Instance Properties
	override val derived = None;
	override val variables = Set.empty[VariableName];
	
	
	override def apply (in : Env[A]) : SolverError \/ Env[A] =
	{
		val answers = in.view.map (pair => Answer (pair._1, pair._2)).toSeq;
		
		if (pf.isDefinedAt (answers) && pf (answers))
			\/- (in);
		else
			-\/ (UnsolvableError);
	}
}
	
	
/**
 * The '''AnswerValueConstraint''' type is a filter `class` used to apply
 * ''global'' [[com.tubros.constraints.core.spi.solver.Constraint]]s to the
 * values of each [[com.tubros.constraints.api.solver.Answer]].
 *
 * @author svickers
 *
 */
class AnswerValueConstraint[A, C[_]] (private val constraint : C[A] => Boolean)
	(implicit cbf : CanBuildFrom[Nothing, A, C[A]])
	extends Constraint[A]
{
	/// Instance Properties
	override val derived = None;
	val variables = Set.empty[VariableName];
	
	
	override def apply (in : Env[A]) : SolverError \/ Env[A] =
	{
		if (constraint (in.values.to[C]))
			\/- (in);
		else
			-\/ (UnsolvableError);
	}
}
