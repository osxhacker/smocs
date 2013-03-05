/**
 * Created on: Feb 22, 2013
 */
package com.tubros.constraints.core.internal
package tree

import scala.collection.mutable.LinkedHashMap
import scala.language.higherKinds

import scalaz._

import com.tubros.constraints.api._
import com.tubros.constraints.core.spi.solver._

import heuristic.{
	AssignmentImpact,
	ConstraintPropagation
	}
import problem._
import solver._


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
		: SolverState[(Variable[A, DomainType], List[Variable[A, DomainType]])] =
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
		children : List[Variable[A, DomainType]]
		)
		: SolverState[Stream[Seq[Answer[A]]]] =
		gets {
			vs =>
				
			implicit val order = new VariableStore.AnswerOrdering[A] (vs);
			val tree = SolutionTree[A] ();
			
			// TOOO: this is a *very* temporary approach, as it hits all nodes!
			val bruteForce = tree.expand (
				tree.root,
				root :: children,
				new ConstraintPropagation[A, DomainType] (vs.constraints)
				);
			val c = vs.answerFilters.foldLeft (Constraint.kleisliUnit[A]) {
				case (accum, c) =>
					
				accum >==> c;
				}
			
			bruteForce.toStream (expected = children.length + 1).filter {
				candidate =>
					
				val args = LinkedHashMap (candidate.map (_.toTuple) : _*);
				
				c.run (args.toMap).isRight;
				}
			}
}
