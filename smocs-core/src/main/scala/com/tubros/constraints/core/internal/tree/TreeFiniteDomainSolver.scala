/**
 * Created on: Feb 22, 2013
 */
package com.tubros.constraints.core.internal
package tree

import scala.annotation._
import scala.collection.immutable.TreeMap
import scala.language.{
	higherKinds,
	postfixOps
	}

import scalaz.{
	Ordering => _,
	_
	}

import com.tubros.constraints.api._
import com.tubros.constraints.core.spi.solver._

import heuristic._
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
		E : Equal[A],
		S : Show[A]
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
			satisfactoryAnswers <- search (chosen)
			answers <- label[C] (satisfactoryAnswers)
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
				
			answers.map {
				_.foldLeft (MO.zero) {
					case (accum, answer) =>
						
					accum |+| answer.point[C];
					}
				}
			}
	
	
	private def search (variables : List[Variable[A, DomainType]])
		: SolverState[Stream[Seq[Answer[A]]]] =
		MS.gets {
			vs =>
				
			lazy val globalFilters = vs.globalConstraints ();
			val assignmentProducer = AssignmentEnumerator[A, Stream] () map {
				steps =>
					
				steps %= ConstraintPropagationEnumeratee[
					A,
					DomainType,
					AssignmentEnumerator[A, Stream]#StateType
					] (vs);
				}
			val spaceToExplore = variables.headOption map {
				root =>
					
				implicit val ordering = vs.ordering;
				
				SolutionTree[A] (root, assignmentProducer);
				} toStream;
			
			spaceToExplore >>=
				{
				explore (_, variables, assignmentProducer, vs.symbols) >>= (
					_.latest
					);
				} >>=
				{
				(candidate : Seq[Answer[A]]) =>
					
				val args = TreeMap.empty[VariableName, A] (
					VariableStore.VariableNameOrdering
					) ++ candidate.map (_.toTuple);
				
				globalFilters.run (args).fold (
					_ => Stream.empty,
					_.to[Seq].map (Answer.fromTuple[VariableName, A]).point[Stream]
					);
				}
			}
	
	
	private def explore (
		tree : SolutionTree[A],
		variables : List[Variable[A, DomainType]],
		assigner : AssignmentEnumerator[A, Stream],
		symbolTable : SymbolTable
		)
		: Stream[SolutionTree[A]] =
	{
		val minimalSize = MinimumDomainSize[A] ();
		val chooser
			: List[Variable[A, DomainType]] => List[Variable[A, DomainType]] =
			{
			choices =>
				
			minimalSize (
				choices filterNot (v => symbolTable.isDerived (v.name))
				);
			}
		
		@tailrec
		def nextAnswer (t : SolutionTree[A]) : Option[SolutionTree[A]] =
			t.search (variables, chooser, assigner) match {
				case Some (tree) if (tree.depth === symbolTable.size) =>
					Some (tree);
					
				case Some (other) =>
					nextAnswer (other);
					
				case None =>
					None;
				}
		
		def loop (cur : SolutionTree[A]) : Stream[SolutionTree[A]] =
		{
			nextAnswer (cur) match {
				case Some (head) =>
					head #:: loop (head);
					
				case None =>
					Stream.empty;
				}
		}
		
		loop (tree);
	}
}


object TreeFiniteDomainSolver
	extends StateBasedFunctions
	
