/**
 * Created on: Feb 22, 2013
 */
package com.tubros.constraints.core.internal
package tree

import scala.language.higherKinds
import scalaz._
import com.tubros.constraints.api._
import com.tubros.constraints.api.problem._
import com.tubros.constraints.api.solver._
import com.tubros.constraints.core.spi.solver._
import heuristic.{
	AssignmentImpact,
	ConstraintPropagation
	}
import com.tubros.constraints.core.spi.solver.heuristic.AssignmentImpact


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
		e : Equal[A]
	)
	extends Solver[
		A,
		StateBasedSolver[A, TreeFiniteDomainSolver[A]]#SolverState,
		TreeFiniteDomainSolver[A]
		]
		with StateBasedSolver[A, TreeFiniteDomainSolver[A]]
{
	/// Class Imports
	import Scalaz._

	
	/**
	 * The '''TreeFiniteDomainSolver''' version of the run method drives a
	 * [[com.tubros.constraints.core.internal.tree.SolutionTree]]-based
	 * search space to solve the problem in question.
	 */
	override def run[C[_]] (
		implicit mo : Monoid[C[Answer[A]]],
		a : Applicative[C]
		)
		: SolverState[Stream[C[Answer[A]]]] =
		for {
			available <- variables ()
			chosen <- chooseRootFrom (available)

			(root, children) = chosen
			
			satisfactory <- search (root, children)
			answers <- label[C] (satisfactory)
			} yield answers;
	
	
	private def variables () : SolverState[Seq[Variable[A, DomainType]]] =
		gets {
			_.variables
			}
	
	
	private def chooseRootFrom (available : Seq[Variable[A, DomainType]])
		: SolverState[(Variable[A, DomainType], Seq[Variable[A, DomainType]])] =
		gets {
			vs =>
				
			val prioritize = variableRanking[Set];
			val ranked = prioritize (vs.constraints) (available.toList);
			
			(ranked.head, ranked.tail);
			}
			

	private def label[C[_]] (variables : Stream[Seq[Answer[A]]])
		(implicit mo : Monoid[C[Answer[A]]], a : Applicative[C])
		: SolverState[Stream[C[Answer[A]]]] =
		gets {
			vs =>
				
			val answers = variables.map {
				cur =>
					
				cur.foldLeft (mo.zero) {
					case (accum, answer) =>
						
					accum |+| answer.point[C];
					}
				}
			
			answers;
			}
	
	
	private def search (
		root : Variable[A, DomainType],
		children : Seq[Variable[A, DomainType]]
		)
		: SolverState[Stream[Seq[Answer[A]]]] =
		gets {
			vs =>
				
			implicit val order = new VariableStore.AnswerOrdering[A] (vs);
			val tree = SolutionTree[A] ();
			
			// TOOO: this is a *very* temporary approach, as it hits all nodes!
			val bruteForce = tree.expand (
				tree.root,
				root :: children.to[List],
				new ConstraintPropagation[A, DomainType] (vs.constraints)
				);
			
			val all : Stream[Seq[Answer[A]]] = {
				def step (frontier : Frontier[SolutionTree[A]#NodeType[A]])
					: Stream[Option[Set[Answer[A]]]] = {
					val (cur, nextFrontier) = frontier.dequeue;
					
					cur.map (_.assignments) #:: step (nextFrontier);
					}
				
				val (first, frontier) = bruteForce.frontier.dequeue;
				
				// TODO: this is an ugly form that needs to get cleaned up.
				(first.map (_.assignments) #:: step (frontier)) takeWhile (
					_.isDefined
					) map (_.get.toSeq);
				}
			
			all;
			}
}
