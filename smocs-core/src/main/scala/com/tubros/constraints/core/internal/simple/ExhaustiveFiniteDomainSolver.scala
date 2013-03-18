/**
 * Created on: Jan 31, 2013
 */
package com.tubros.constraints.core.internal
package simple

import scala.collection.mutable.LinkedHashMap
import scala.language.higherKinds

import scalaz._

import com.tubros.constraints.api._
import com.tubros.constraints.core.spi.solver._

import problem.{
	Equation,
	Problem
	}
import solver._
import solver.error._


/**
 * The '''ExhaustiveFiniteDomainSolver''' type is a
 * [[com.tubros.constraints.api.solver.Solver]] which searches the ''entire''
 * solution space for all solutions satisfying the CSP constraints.  Since
 * this type of solver can take exponential resources, it should only be used
 * for ''very'' simple problem domains.
 *
 * @author svickers
 *
 */
class ExhaustiveFiniteDomainSolver[A]
	(implicit
		override val canConstrain : CanConstrain[Equation, A],
		override val MS : MonadState[StateBasedSolver.SolverStateT, VariableStore[A]]
	)
	extends Solver[
		A,
		StateBasedSolver[A, ExhaustiveFiniteDomainSolver[A]]#SolverState,
		ExhaustiveFiniteDomainSolver[A]
		]
		with StateBasedSolver[A, ExhaustiveFiniteDomainSolver[A]]
{
	/// Class Imports
	import scalaz.std.list._
	import scalaz.std.stream._
	import scalaz.std.vector._
	import scalaz.syntax.applicative._
	import scalaz.syntax.compose._
	import scalaz.syntax.id._
	import scalaz.syntax.traverse._
	
	
	/**
	 * The '''ExhaustiveFiniteDomainSolver''' version of the run method
	 * simply produces the combination of all remaining
	 * [[com.tubros.constraints.core.spi.solver.DiscreteVariable]]s if and only
	 * if ''all'' have at least one value remaining in their
	 * [[com.tubros.constraints.api.solver.DiscreteDomain]].
	 */
	override def run[C[_]] (
		implicit mo : Monoid[C[Answer[A]]],
		a : Applicative[C]
		)
		: SolverState[Stream[C[Answer[A]]]] =
		for {
			vars <- variables ()
			filters <- filterAnswers ()
			constrained <- applyConstraints (vars, filters)
			answers <- label (constrained)
			} yield answers;
	
	
	private def applyConstraints (
		vars : Seq[Variable[A, DomainType]],
		filters : Seq[Constraint[A]]
		)
		: SolverState[Stream[Map[VariableName, A]]] =
		MS.gets {
			vs =>
				
			val streams = vars.view.to[Stream].map (_.enumerate.to[Stream]);
			val perAnswer = Constraint.chained (filters.to[Vector]);
			val combinedConstraints = vs.globalConstraints ().andThen (perAnswer);
			
			streams.sequence.map (LinkedHashMap.apply).filter {
				candidate =>
					
				combinedConstraints.run (candidate.toMap).isRight;
				}
			}
			
	
	private def filterAnswers ()
		: SolverState[Seq[Constraint[A]]] =
		MS.gets {
			vs =>
				
			vs.constraints.to[Seq];
			}
			
	
	private def label[C[_]] (variables : Stream[Map[VariableName, A]])
		(implicit mo : Monoid[C[Answer[A]]], a : Applicative[C])
		: SolverState[Stream[C[Answer[A]]]] =
		StateT {
			vs =>

			val answers = variables.map {
				_.to[List].foldMap {
					cur =>
						
					Answer (cur).point[C];
					}
				}
			
			\/- (vs, answers);
			}
	
	
	private def variables ()
		: SolverState[Seq[Variable[A, DomainType]]] =
		MS.gets {
			_.variables;
			}
}

