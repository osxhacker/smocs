/**
 * Created on: Feb 22, 2013
 */
package com.tubros.constraints.core.internal
package tree

import scala.language.{
	higherKinds,
	postfixOps
	}

import scalaz.{
	Ordering => _,
	_
	}

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
	(implicit val ao : Ordering[Answer[A]])
	extends SolutionSpace[A, Tree, DiscreteDomain]
{
	/// Class Imports
	import Scalaz._
	import SolutionTree._
	
	
	/// Class Types
	override type LocationType = TreeLoc[NodeType[A]]
	override type NodeType[T] = SolutionTreeNode[T]
	type TreeType[T] = Tree[SolutionTreeNode[T]]
	
	
	/// Instance Properties
	lazy val root : LocationType = tree.loc;
	private implicit val frontierMonoid = frontier.monoid;
	private val NodeType = SolutionTreeNode;
	
	
	override def expand[M[+_]] (
		location : LocationType,
		variables : M[VariableType],
		valuesFor : ValueGenerator
		)
		(implicit fm : Foldable[M])
		: SolutionTree[A] =
		{
			val (subTree, newFrontier) = expander (
				location,
				frontier,
				variables.toList,
				valuesFor
				);
			
			copy (
				tree = subTree.fold (tree) (_.toTree),
				focus = subTree.getOrElse (location),
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
		val parentNode = NodeType (subTree.root.getLabel.assignments.init);
		
		findNodeUnder (tree.loc, parentNode).fold (this) {
			node =>
				
			val merged = node.insertDownLast (subTree.root.toTree).toTree;
			
			copy (
				tree = merged,
				focus = node,
				frontier = frontier |+| subTree.frontier
				);
			}
	}
	
	
	override def prune (location : LocationType) : SolutionTree[A] =
		copy (tree = location.delete.map (_.toTree) getOrElse (tree));
	
	
	override def search[M[+_]] (
		variables : M[VariableType],
		choose : M[VariableType] => M[VariableType],
		valuesFor : ValueGenerator
		)
		(implicit fm : Foldable[M])
		: Option[SolutionTree[A]] =
	{
		val (location, nextFrontier) = frontier.dequeue;
		val maybeExpandFrom = location.flatMap {
			node =>
				
			/// Try finding it under the current focus first, dropping back
			/// to the root for nodes in the frontier "outside" of the focus.
			findNodeUnder (focus, node) orElse findNodeUnder (tree.loc, node);
			}
		
		maybeExpandFrom.map {
			from =>
				
			copy (frontier = nextFrontier).expand (
				from,
				choose (variables),
				valuesFor
				);
			}
	}
	
	
	/**
	 * The toStream method creates a [[scala.collection.immutable.Stream]]
	 * containing answers which have the '''expected''' number of assignments.
	 */
	def toStream (expected : Int) : Stream[Seq[Answer[A]]] =
		tree.flatten.filter (_.assignments.size === expected).map {
			node =>
				
			node.assignments.toSeq;
			}

	
	private def expander (
		parent : LocationType,
		frontier : Frontier[NodeType[A]],
		vars : List[VariableType],
		valuesFor : ValueGenerator
		)
		: (Option[LocationType], Frontier[NodeType[A]]) =
		vars match {
			case Nil =>
				(None, frontier);
				
			case last :: Nil =>
				val allowedValues = valuesFor (
					parent.getLabel.assignments,
					last
					).domain;
				
				val (node, newFrontier) = allowedValues.foldLeft ((parent, frontier)) {
					case ((parent, frontier), value) =>
						
					createChildNode (parent, last, value) :-> (frontier.enqueue);
					}
				
				/// We only propagate changes if any were introduced
				(node.hasChildren.option (node), newFrontier);
				
			case intermediary :: tail =>
				val allowedValues = valuesFor (
					parent.getLabel.assignments,
					intermediary
					).domain;
				
				allowedValues.foldLeft ((parent.some, frontier)) {
					case ((Some (prevParent), prevFrontier), value) =>
						
					val (updatedParent, newFrontier) = expander (
						createChildNode (
							prevParent,
							intermediary,
							value
							)._1.lastChild.get,
						prevFrontier,
						tail,
						valuesFor
						);
					
					/// As with leaf generation, only propagate changes if there
					(
						updatedParent.flatMap (_.parent).orElse {
							Some (prevParent)
							},
						newFrontier
					);
					}
			}
	
	
	private def createChildNode (
		parent : LocationType,
		variable : VariableType,
		value : A
		)
		: (LocationType, NodeType[A]) =
	{
		val node = NodeType (
			 parent.getLabel.assignments + Answer (variable.name, value)
			);
		
		(
			parent.insertDownLast (node.leaf).parent.get,
			node
		);
	}
	
	
	private def findNodeUnder (startingAt : LocationType, node : NodeType[A])
		: Option[LocationType] =
	{
		val delta = node.assignments &~ startingAt.getLabel.assignments;
		
		def finder (loc : LocationType, assignments : List[Answer[A]])
			: Option[LocationType] =
			assignments.isEmpty.fold (
				Option (loc) filter (_.getLabel === node),
				loc.findChild {
					node =>
						
					node.rootLabel.assignments.contains (assignments.head);
					}.flatMap (finder (_, assignments.tail))
				);
		
		return (finder (startingAt, delta.toList));
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
	
	
	case class SolutionTreeNode[A] (
		override val assignments : SortedSet[Answer[A]]
		)
		extends SolutionSpace.Node[A]
	
	object SolutionTreeNode
	{
		implicit def equalNode[A] : Equal[SolutionTreeNode[A]] = Equal.equalA;
		
		
		implicit def showNode[A] : Show[SolutionTreeNode[A]] =
			new Show[SolutionTreeNode[A]] {
				override def shows (n : SolutionTreeNode[A]) = n.toString;
				}
	}
	
	
	class ShowSolutionTree[A : Show]
		extends Show[SolutionTree[A]]
	{
		/// Class Imports
		import Cord._
		
		
		override def show (solution : SolutionTree[A]) : Cord =
			Cord ("SolutionTree(\n") ++
			stringToCord (solution.tree.drawTree) ++
			Cord ("Focus(") ++
			stringToCord (solution.focus.getLabel.toString) ++
			Cord (")\n") ++
			solution.frontier.show ++
			Cord ("\n)");
	}
	
	
	/**
	 * This apply method is provided to enable functional-style creation and
	 * is defined in terms of the `empty` method.
	 */
	def apply[A] ()
		(implicit ao : Ordering[Answer[A]])
		: SolutionTree[A] =
		empty[A];
	
	
	/**
	 * This version of the apply method allows for convenient '''SolutionTree'''
	 * creation when given the result of a
	 * [[com.tubros.constraints.core.internal.Frontier]] `dequeue` operation.
	 */
	def apply[A] (root : Option[NodeType[A]], frontier : Frontier[NodeType[A]])
		(implicit ao : Ordering[Answer[A]])
		: SolutionTree[A] =
		root.fold (empty[A]) {
			node =>
				
			val tree = Tree (node);
			
			new SolutionTree[A] (tree, tree.loc, frontier);
			}
	
			
	def apply[A] (
		variable : Variable[A, DiscreteDomain],
		valuesFor : SolutionTree[A]#ValueGenerator
		)
		(implicit ao : Ordering[Answer[A]])
		: SolutionTree[A] =
	{
		val space = empty[A];
		
		space.expand[Option] (space.root, Option (variable), valuesFor);
	}
	
	
	def empty[A] (implicit ao : Ordering[Answer[A]]) : SolutionTree[A] =
	{
		val tree = Tree (SolutionTreeNode[A] (SortedSet.empty[Answer[A]]));
		
		new SolutionTree (tree, tree.loc, Frontier.lifo[NodeType[A]]);
	}
	
	
	/**
	 * The fromFrontier method creates a '''SolutionTree''' based on the
	 * next '''NodeType''' provided by the '''frontier'''.
	 */
	def fromFrontier[A] (frontier : Frontier[NodeType[A]])
		(implicit ao : Ordering[Answer[A]])
		: SolutionTree[A] =
		(
			SolutionTree (
				_ : Option[SolutionTreeNode[A]],
				_ : Frontier[SolutionTreeNode[A]]
				)
		).tupled (frontier.dequeue);
	
	
	object implicits
	{
		/// Class Imports
		import syntax.tree._
		
		
		implicit def solutionShow[A : Show] : Show[SolutionTree[A]] =
			new ShowSolutionTree[A];
	}
}
