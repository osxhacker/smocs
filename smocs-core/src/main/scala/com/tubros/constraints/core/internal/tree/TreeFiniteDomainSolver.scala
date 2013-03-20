/**
 * Created on: Feb 22, 2013
 */
package com.tubros.constraints.core.internal
package tree

import scala.collection.immutable.TreeMap
import scala.language.higherKinds

import scalaz.{
	Ordering => _,
	_
	}

import com.tubros.constraints.api._
import com.tubros.constraints.core.spi.solver._

import heuristic.ConstraintPropagationEnumeratee
import problem._
import runtime._
import solver._
import solver.error._


/**
 * The '''TreeFiniteDomainSolver''' type uses the
 * [[com.tubros.constraints.core.internal.tree.SolutionTree]] type to define a
 * tree-based [[com.tubros.constraints.api.solver.Solver]].
 *
 * @author svickers
 *
 */
class TreeFiniteDomainSolver[A] (
	private val variableRanking : VariableRankingPolicy[A]
	)
	(implicit
		override val canConstrain : CanConstrain[Equation, A],
		override val MS : MonadState[StateBasedSolver.SolverStateT, VariableStore[A]],
		e : Equal[A]
	)
	extends Solver[
		A,
		StateBasedSolver[A, TreeFiniteDomainSolver[A]]#SolverState,
		TreeFiniteDomainSolver[A]
		]
		with StateBasedSolver[A, TreeFiniteDomainSolver[A]]
		with ConsistencyChecks[A, DiscreteDomain]
{
	/// Class Imports
	import Scalaz._

	
	/**
	 * The '''TreeFiniteDomainSolver''' version of the run method drives a
	 * [[com.tubros.constraints.core.internal.tree.SolutionTree]]-based
	 * search space to solve the problem in question.
	 */
	override def run[C[_]] (
		implicit MO : Monoid[C[Answer[A]]],
		AC : Applicative[C]
		)
		: SolverState[Stream[C[Answer[A]]]] =
		for {
			available <- variables ()
			chosen <- chooseRootFrom (available)
			satisfactory <- search (chosen)
			answers <- label[C] (satisfactory)
			} yield answers;

	
	private def variables ()
		: SolverState[Seq[Variable[A, DomainType]]] =
		StateT {
			vs =>
				
			val validations = for {
				_ <- hasVariables (vs.variables, vs.symbols, vs)
				_ <- hasUnknownVariables (vs.variables, vs.symbols, vs)
				} yield vs.variables.toSeq;
				
			validations match {
				case Success (vars) =>
					\/- (vs, vars);

				case Failure (err) =>
					-\/ (err);
				}
			}
	
	
	private def chooseRootFrom (available : Seq[Variable[A, DomainType]])
		: SolverState[List[Variable[A, DomainType]]] =
		MS.gets {
			vs =>
				
			variableRanking (vs) (available.toList);
			}
			

	private def label[C[_]] (answers : Stream[Seq[Answer[A]]])
		(implicit MO : Monoid[C[Answer[A]]], a : Applicative[C])
		: SolverState[Stream[C[Answer[A]]]] =
		MS.gets {
			vs =>
				
			val convertedStream = answers.map {
				cur =>
					
				cur.foldLeft (MO.zero) {
					case (accum, answer) =>
						
					accum |+| answer.point[C];
					}
				}
			
			convertedStream;
			}
	
	
	private def search (variables : List[Variable[A, DomainType]])
		: SolverState[Stream[Seq[Answer[A]]]] =
		MS.gets {
			vs =>
				
			implicit val ordering = new VariableStore.AnswerOrdering[A] (vs);
			val tree = SolutionTree[A] ();
			val assignmentProducer = AssignmentEnumerator[A, Stream] () map {
				steps =>
					
				steps %= ConstraintPropagationEnumeratee[
					A,
					DomainType,
					AssignmentEnumerator[A, Stream]#StateType
					] (vs);
				}
			
			// TOOO: this is a *very* temporary approach, as it hits all nodes!
			val bruteForce = tree.expand (
				tree.root,
				variables,
				assignmentProducer
				);
			val globalFilters = vs.globalConstraints ();
			
			bruteForce.toStream (expected = vs.symbols.size).filter {
				candidate =>
					
				val args = TreeMap (candidate.map (_.toTuple) : _*) (
					VariableStore.VariableNameOrdering
					);
				
				globalFilters.run (args).isRight;
				}
			}
}

