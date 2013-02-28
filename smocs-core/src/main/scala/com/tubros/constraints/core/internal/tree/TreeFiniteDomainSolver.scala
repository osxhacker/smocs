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


/**
 * The '''TreeFiniteDomainSolver''' type uses the
 * [[com.tubros.constraints.core.internal.graph.Tree]] type to define a
 * tree-based [[com.tubros.constraints.api.solver.Solver]].
 *
 * @author svickers
 *
 */
class TreeFiniteDomainSolver[A]
	(implicit
		override val canConstrain : CanConstrain[Equation, A],
		manifest : Manifest[A]
	)
	extends Solver[
		A,
		StateBasedSolver[A, TreeFiniteDomainSolver[A]]#SolverState,
		TreeFiniteDomainSolver[A]
		]
		with StateBasedSolver[A, TreeFiniteDomainSolver[A]]
{
	/// Class Imports
	import std.list._
	import syntax.all._
	import State._

	
	/**
	 * The '''TreeFiniteDomainSolver''' version of the run method drives a
	 * [[com.tubros.constraints.core.internal.graph.SolutionTree]]-based
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
			answers <- label (satisfactory)
			} yield answers;
	
	
	private def variables () : SolverState[Seq[Variable[A, DomainType]]] =
		gets {
			_.variables
			}
	
	
	private def chooseRootFrom (available : Seq[Variable[A, DomainType]])
		: SolverState[(Variable[A, DomainType], Seq[Variable[A, DomainType]])] =
		state {
			// TODO: use heuristics to pick a good root
			(available.head, available.drop (1))
			}
			

	private def label[C[_]] (variables : Stream[Seq[Answer[A]]])
		(implicit mo : Monoid[C[Answer[A]]], a : Applicative[C])
		: SolverState[Stream[C[Answer[A]]]] =
		state {
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
				
			val tree = SolutionTree[A] ();
			
			// TOOO: this is a *very* temporary approach, as it hits all nodes!
			val justForTesting = tree.expand (
				tree.root,
				root :: children.to[List],
				(_, v : Variable[A, DomainType]) => v.domain
				);
			
			val all : Stream[Seq[Answer[A]]] = {
				def step (frontier : Frontier[SolutionTree[A]#NodeType])
					: Stream[Option[Seq[Answer[A]]]] = {
					val (cur, nextFrontier) = frontier.dequeue;
					
					cur.map (_.assignments) #:: step (nextFrontier);
					}
				
				val (first, frontier) = justForTesting.frontier.dequeue;
				
				(first.map (_.assignments) #:: step (frontier)) takeWhile (_.isDefined) map (_.get);
				}
			
			all;
				/*
			val pickVar = (vars : Seq[Variable[A, DomainType]]) => vars.head;
			val pickValue = (vals : DomainType[A]) => vals.head;
			val tree = search (Node.empty[A], root +: children, pickVar, pickValue) (
				SolutionTree (Node.empty[A])
				);
			
			println ("### tree: %s".format (tree))
			tree.answers (children.size + 1).to[Stream].map {
				answer =>
					
				answer.flatMap (_.assignment).map (_.toTuple).toMap
				}
				* 
				*/
			}
}
