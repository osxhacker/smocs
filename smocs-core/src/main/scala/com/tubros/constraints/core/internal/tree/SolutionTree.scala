/**
 * Created on: Feb 22, 2013
 */
package com.tubros.constraints.core.internal
package tree

import scala.annotation._
import scala.language.{
	higherKinds,
	postfixOps
	}

import scalaz.{
	Ordering => _,
	_
	}

import com.tubros.constraints.api._
import com.tubros.constraints.api.solver._


/**
 * The '''SolutionTree''' type defines a [[scalaz.Tree]]-based
 * [[http://en.wikipedia.org/wiki/Tree_%28graph_theory%29 Tree]].
 * [[com.tubros.constraints.api.solver.Solver]] specific functionality
 * should be added by extension classes.
 * 
 * '''SolutionTree''' does satisfy the requirements set forth by
 * [[com.tubros.constraints.core.internal.SolutionSpace]] and is designed to
 * integrate with ''search combinators''.
 * 
 * Also, each node has its assignments kept in the [[scala.math.Ordering]]
 * `implicitly` provided to this instance.  Doing this satisfies the contract
 * for [[com.tubros.constraints.api.solver.Variable]] position stability,
 * needed by global constraints.
 *
 * @author svickers
 *
 */
final case class SolutionTree[A] (
	private val tree : Tree[SolutionTree[A]#NodeType[A]],
	val focus : SolutionTree[A]#LocationType,
	override val frontier : Frontier[SolutionTree[A]#NodeType[A]]
	)
	(implicit val ao : Ordering[Answer[A]], e : Equal[A])
	extends SolutionSpace[A, Tree, DiscreteDomain]
{
	/// Class Imports
	import Scalaz._
	import SolutionTree._
	
	
	/// Class Types
	override type LocationType = TreeLoc[NodeType[A]]
	override type NodeType[T] = SolutionTreeNode[T]
	type TreeType[T] = Tree[NodeType[T]]
	
	
	/// Instance Properties
	lazy val isEmpty : Boolean = tree.rootLabel.isEmpty && frontier.isEmpty;
	lazy val root : LocationType = tree.loc;
	private implicit val frontierMonoid = frontier.monoid;
	private val NodeType = SolutionTreeNode;
	
	
	def depth : Int = focus.firstChild.map (_.getLabel.assignments.size) |
		focus.getLabel.assignments.size;


	def expand[M[_]] (
		variables : M[VariableType],
		valuesFor : AssignmentGenerator
		)
		(implicit fm : Foldable[M])
		: Option[SolutionTree[A]] =
		expand[M] (focus, variables, valuesFor);
	
	
	override def expand[M[_]] (
		location : LocationType,
		variables : M[VariableType],
		valuesFor : AssignmentGenerator
		)
		(implicit fm : Foldable[M])
		: Option[SolutionTree[A]] =
		expander (location, frontier, variables.toList, valuesFor).map {
			case (subTree, newFrontier) =>
				copy (
					tree = subTree.toTree,
					focus = subTree,
					frontier = newFrontier
					);
			}
	
	
	/**
	 * The flatMap method gives the provided '''functor''' a
	 * [[com.tubros.constraints.core.internal.Frontier]] based on this
	 * instance and integrates the resultant '''SolutionTree''' into a newly
	 * created '''SolutionTree'''.  Also, the new '''SolutionTree''' has its
	 * `focus` set to the merged tree's location.
	 */
	def flatMap (
		functor : Frontier[SolutionTree[A]#NodeType[A]] => SolutionTree[A]
		)
		: SolutionTree[A] =
	{
		val subTree = functor (frontier);
		/// A functor which returns an empty tree will not be merged
		/// into this SolutionTree.
		val insertionSpot =
			Some (subTree).filterNot (_.isEmpty).flatMap {
				newTree =>
					
				val lookingFor = newTree.tree.rootLabel;
				
				findNodeUnder (focus, lookingFor) orElse {
					findNodeUnder (root, lookingFor);
					}
			}
		
		insertionSpot.fold (this) {
			node =>
				
			val merged = node.setTree (subTree.root.toTree).toTree;
			
			copy (
				tree = merged,
				focus = merged.loc,
				frontier = frontier |+| subTree.frontier
				);
			}
	}
	
	
	def latest () : Stream[Seq[Answer[A]]] = focus.tree.subForest map {
		children =>
			
		children.rootLabel.assignments.to[Seq];
		}
	
	
	override def prune (location : LocationType) : SolutionTree[A] =
	{
		val pruned = location.delete.map (_.toTree);
		
		pruned.fold (this) {
			t =>
				
			copy (tree = t);
			}
	}
	
	
	override def search[M[_]] (
		variables : M[VariableType],
		choose : M[VariableType] => M[VariableType],
		valuesFor : AssignmentGenerator
		)
		(implicit F : Foldable[M])
		: Option[SolutionTree[A]] =
	{
		@tailrec
		def next (unexploredFrontier : Frontier[NodeType[A]], leafSize : Int)
			: (Option[NodeType[A]], Frontier[NodeType[A]]) =
			unexploredFrontier.dequeue match {
				/// Skip over frontier entries which represent the limits of
				/// the solution space and have no way to be expanded.
				case (Some (leaf), updated)
					if (leaf.assignments.size === leafSize)
					=>

					next (updated, leafSize);
					
				case other =>
					other;
				}
		
		val (location, updatedFrontier) = next (frontier, variables.toList.size);
		val maybeExpandFrom = location >>= {
			node =>
				
			/// Try finding it under the current focus first, dropping back
			/// to the root for nodes in the frontier "outside" of the focus.
			findNodeUnder (focus, node) orElse findNodeUnder (root, node);
			}
		
		maybeExpandFrom >>= {
			from =>
				
			val available = from.getLabel.unassigned (choose (variables));
			val updated = copy (focus = from, frontier = updatedFrontier);
			val expanded = updated.expand (available, valuesFor);
			
			expanded orElse (Some (updated)) filterNot (_.frontier.isEmpty);
			}
	}
	
	
	/**
	 * The toStream method creates a [[scala.collection.immutable.Stream]]
	 * containing answers which have the '''expected''' number of assignments.
	 */
	def toStream (expected : Int) : Stream[Seq[Answer[A]]] =
		tree.flatten.withFilter (_.assignments.size === expected).map {
			node =>
				
			node.assignments.toSeq;
			}

	
	private def addToLocation (
		node : LocationType,
		additional : Stream[NodeType[A]]
		)
		: LocationType =
		node.modifyTree {
			t =>
				
			Tree.node (t.rootLabel, t.subForest #::: additional.map (_.leaf));
			}
		
	
	/**
	 * The expander method adds to the nodes under `parent` *for one level*.
	 */
	private def expander (
		parent : LocationType,
		frontier : Frontier[NodeType[A]],
		vars : List[VariableType],
		valuesFor : AssignmentGenerator
		)
		: Option[(LocationType, Frontier[NodeType[A]])] =
		vars match {
			case Nil =>
				None;
				
			case current :: more =>
				val leaves = immediateChildren (parent, current, valuesFor);
				
				/// Only create a node if there were child nodes produced
				(!leaves.isEmpty).option {
					addToLocation (parent, leaves) -> leaves.foldLeft (frontier) {
						_.enqueue (_);
						}
					}
			}
	
	
	private def findNodeUnder (startingAt : LocationType, node : NodeType[A])
		: Option[LocationType] =
	{
		startingAt.findChild (_.rootLabel.isSubsetOf (node)) >>= {
			child =>
				
			if (child.getLabel.size >= node.size) 
				Some (child);
			else
				findNodeUnder (child, node);
			}
	}
	
	
	private def immediateChildren (
		parent : LocationType,
		variable : VariableType,
		valuesFor : AssignmentGenerator
		)
		: Stream[NodeType[A]] =
	{
		val parentNode = parent.getLabel;
		
		valuesFor.generate (parentNode.assignments.toSeq, variable) map {
			additions =>

			parentNode map (e => additions ++ e);
			}
	}
}
	
	
object SolutionTree
{
	/// Class Imports
	import collection.immutable.SortedSet
	import Scalaz._
	
	
	/// Class Types
	type NodeType[A] = SolutionTreeNode[A]
	type TreeType[A] = Tree[NodeType[A]]
	
	
	class ShowSolutionTree[A : Show]
		extends Show[SolutionTree[A]]
	{
		/// Class Imports
		import Cord._
		
		
		override def show (solution : SolutionTree[A]) : Cord =
			Cord (
				"SolutionTree(\n",
				mkCord (
					"\n",
					solution.tree.drawTree,
					Cord (
						"focus=",
						solution.focus.getLabel.show
						),
					Cord (
						"depth=",
						solution.depth.show
						),
					solution.frontier.show
					),
				")\n"
				)
	}
	
	
	/**
	 * This apply method is provided to enable functional-style creation and
	 * is defined in terms of the `empty` method.
	 */
	def apply[A] ()
		(implicit AO : Ordering[Answer[A]], E : Equal[A])
		: SolutionTree[A] =
		empty[A];
	
	
	/**
	 * This version of the apply method allows for convenient '''SolutionTree'''
	 * creation when given the result of a
	 * [[com.tubros.constraints.core.internal.Frontier]] `dequeue` operation.
	 */
	def apply[A] (root : Option[NodeType[A]], frontier : Frontier[NodeType[A]])
		(implicit AO : Ordering[Answer[A]], E : Equal[A])
		: SolutionTree[A] =
		root.fold (empty[A]) {
			node =>
				
			val tree = Tree (node);
			
			new SolutionTree[A] (tree, tree.loc, frontier);
			}
	
			
	def apply[A] (
		variable : Variable[A, DiscreteDomain],
		valuesFor : SolutionTree[A]#AssignmentGenerator
		)
		(implicit AO : Ordering[Answer[A]], E : Equal[A])
		: SolutionTree[A] =
	{
		val space = empty[A];
		
		space.expand[Option] (space.root, Option (variable), valuesFor) | space;
	}
	
	
	def empty[A] (implicit AO : Ordering[Answer[A]], E : Equal[A])
		: SolutionTree[A] =
	{
		val tree = Tree (SolutionTreeNode[A] (SortedSet.empty[Answer[A]]));
		
		new SolutionTree (tree, tree.loc, Frontier.lifo[NodeType[A]]);
	}
	
	
	/**
	 * The fromFrontier method creates a '''SolutionTree''' based on the
	 * next '''NodeType''' provided by the '''frontier'''.
	 */
	def fromFrontier[A] (frontier : Frontier[NodeType[A]])
		(implicit AO : Ordering[Answer[A]], E : Equal[A])
		: SolutionTree[A] =
		(
			SolutionTree (
				_ : Option[SolutionTreeNode[A]],
				_ : Frontier[SolutionTreeNode[A]]
				)
		).tupled (frontier.dequeue);
	
	
	/// Implicit Conversions
	implicit def showSolution[A : Show] : Show[SolutionTree[A]] =
		new ShowSolutionTree[A];
}
