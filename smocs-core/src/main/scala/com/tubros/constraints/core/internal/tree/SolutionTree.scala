/**
 * Created on: Feb 22, 2013
 */
package com.tubros.constraints.core.internal
package tree

import scala.collection.immutable.Stack
import scala.language.{
	higherKinds,
	postfixOps
	}

import scalaz._

import com.tubros.constraints.api.solver._


/**
 * The '''SolutionTree''' type defines a [[scalaz.Tree]]-based
 * [[http://en.wikipedia.org/wiki/Tree_%28graph_theory%29 Tree].
 * [[com.tubros.constraints.api.solver.Solver]] specific functionality
 * should be added by extension classes.
 * 
 * '''SolutionTree''' does satisfy the requirements set forth by
 * [[com.tubros.constraints.core.internal.SolutionSpace]] and is designed to
 * integrate with ''search combinators''.
 *
 * @author svickers
 *
 */
final case class SolutionTree[A] (
	private val tree : Tree[SolutionTree[A]#NodeType],
	override val frontier : Frontier[SolutionTree[A]#NodeType]
	)
	extends SolutionSpace[A, Tree, DiscreteDomain]
{
	/// Class Imports
	import Scalaz._
	import SolutionTree._
	
	
	/// Class Types
	override type LocationType = TreeLoc[NodeType]
	override type NodeType = SolutionTreeNode[A]
	
	
	/// Instance Properties
	lazy val root : LocationType = tree.loc;
	private val NodeType = SolutionTreeNode;
	
	
	override def expand[M[+_], N[_]] (
		location : LocationType,
		variables : M[VariableType],
		valuesFor : ValueGenerator[N]
		)
		(implicit fm : Foldable[M], fn : Foldable[N])
		: SolutionTree[A] =
		{
			val (subTree, newFrontier) = expander (
				location,
				frontier,
				variables.toList,
				valuesFor
				);
			
			copy (
				tree = subTree.fold (location.toTree) (_.toTree),
				frontier = newFrontier
				);
		}
	
	
	override def prune (location : LocationType) : SolutionTree[A] =
		copy (tree = location.delete.map (_.toTree) getOrElse (tree));
	
	
	override def search[M[+_], N[_]] (
		location : LocationType,
		variables : M[VariableType],
		choose : M[VariableType] => M[VariableType],
		valuesFor : ValueGenerator[N]
		)
		(implicit fm : Foldable[M], fn : Foldable[N])
		: SolutionTree[A] =
	{
		expand (location, choose (variables), valuesFor);
	}

	
	private def expander[N[_]] (
		parent : LocationType,
		frontier : Frontier[NodeType],
		vars : List[VariableType],
		valuesFor : ValueGenerator[N]
		)
		(implicit f : Foldable[N])
		: (Option[LocationType], Frontier[NodeType]) =
		vars match {
			case Nil =>
				(None, frontier);
				
			case last :: Nil =>
				val allowedValues = valuesFor (
					parent.getLabel.assignments,
					last
					);
				
				val (node, newFrontier) = allowedValues.foldLeft ((parent, frontier)) {
					case (p, value) =>
						
					val (parent, frontier) = p;
					
					createChildNode (parent, last, value) :-> (frontier.enqueue);
					}
				
				/// We only propagate changes if any were introduced
				(node.hasChildren.option (node), newFrontier);
				
			case intermediary :: tail =>
				val allowedValues = valuesFor (
					parent.getLabel.assignments,
					intermediary
					);
				
				allowedValues.foldLeft ((parent.some, frontier)) {
					case (p, value) =>
						
					val (updatedParent, newFrontier) = expander (
						createChildNode (
							p._1.get,
							intermediary,
							value
							)._1.lastChild.get,
						p._2,
						tail,
						valuesFor
						);
					
					/// As with leaf generation, only propagate changes if there
					(
						updatedParent.flatMap (_.parent).orElse (p._1),
						newFrontier
					);
					}
			}
	
	
	private def createChildNode (
		parent : LocationType,
		variable : VariableType,
		value : A
		)
		: (LocationType, NodeType) =
	{
		val node = NodeType (
			parent.getLabel.assignments :+ Answer (variable.name, value)
			);
		
		(
			parent.insertDownLast (node.leaf).parent.get,
			node
		);
	}
}
	
	
object SolutionTree
{
	/// Class Imports
	import syntax.monoid._
	import syntax.show._
	
	
	/// Class Types
	type NodeType[A] = SolutionTreeNode[A]
	type TreeType[A] = Tree[NodeType[A]]
	
	
	case class SolutionTreeNode[A] (
		override val assignments : Seq[Answer[A]]
		)
		extends SolutionSpace.Node[A]
	
	object SolutionTreeNode
	{
		implicit def showNode[A] : Show[SolutionTreeNode[A]] =
			new Show[SolutionTreeNode[A]] {
				override def shows (n : SolutionTreeNode[A]) = n.toString;
				}
	}
	
	
	class ShowSolutionTree[A : Show]
		extends Show[SolutionTree[A]]
	{
		override def show (solution : SolutionTree[A]) : Cord =
			Cord ("SolutionTree(\n") ++
			Cord.stringToCord (solution.tree.drawTree) ++
			solution.frontier.show ++
			Cord ("\n)");
	}
	
	
	/**
	 * This apply method is provided to enable functional-style creation and
	 * is defined in terms of the `empty` method.
	 */
	def apply[A] () = empty[A];
	
	
	def empty[A] : SolutionTree[A] =
		new SolutionTree (
			Tree (SolutionTreeNode[A] (Seq.empty[Answer[A]])),
			Frontier.fifo[NodeType[A]]
			);
	
	
	object implicits
	{
		/// Class Imports
		import syntax.tree._
		
		
		implicit def showSolution[A : Show] : Show[SolutionTree[A]] =
			new ShowSolutionTree[A];
	}
}
