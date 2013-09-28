/**
 * Created on: Mar 14, 2013
 */
package com.tubros.constraints.core.internal
package tree

import scala.annotation._
import scala.collection.GenTraversableOnce
import scala.collection.immutable.SortedSet
import scala.language.higherKinds

import scalaz.{
	Ordering => _,
	_
	}

import com.tubros.constraints.api._
import com.tubros.constraints.api.solver.{
	Answer,
	Domain,
	Variable
	}


/**
 * The '''SolutionTreeNode''' type is the `class` responsible for tracking
 * [[com.tubros.constraints.core.internal.tree.SolutionTree]] information at
 * each ''node'' within the tree.  Empty nodes are supported, and the
 * `implicit` [[scala.math.Ordering]] is used to determine where each
 * [[com.tubros.constraints.api.solver.Answer]] is placed within the
 * '''assignments'''.
 *
 * @author svickers
 *
 */
final case class SolutionTreeNode[A] (
	override val assignments : SortedSet[Answer[A]]
	)
	(implicit E : Equal[A], O : Ordering[Answer[A]])
	extends SolutionSpace.Node[A]
{
	/// Class Imports
	import Scalaz._
	
	
	/// Instance Properties
	private lazy val bound = assignments map (_.name);
	val isEmpty : Boolean = assignments.isEmpty;
	val size : Int = assignments.size;
	
	
	def :+ (answer : Answer[A]) : SolutionTreeNode[A] =
		copy (assignments + answer);
	
	
	def map (f : GenTraversableOnce[Answer[A]] => GenTraversableOnce[Answer[A]])
		: SolutionTreeNode[A] =
		SolutionTreeNode[A] (SortedSet.empty[Answer[A]] ++ f (assignments));
	
	
	/**
	 * The unassigned method produces a `List[Variable[A, DT]]` of the given
	 * '''variables''' which are _not_ already `bound` in this node.
	 */
	def unassigned[DT[X] <: Domain[X], M[_]] (variables : M[Variable[A, DT]])
		(implicit F : Foldable[M])
		: List[Variable[A, DT]] =
		variables.foldLeft (List.empty[Variable[A, DT]]) {
			(accum, v) =>
				
			bound.exists (_ === v.name) fold (accum, v :: accum);
			}.reverse;
		
	
	/**
	 * The isSubsetOf method is optimized for speed, as it is ''heavily'' used
	 * in the
	 * [[com.tubros.constraints.core.internal.internal.SolutionTree]]
	 * implementation.
	 */
	private[tree] def isSubsetOf (other : SolutionTreeNode[A]) : Boolean =
	{
		@tailrec
		def loop (us : Iterator[Answer[A]], them : Iterator[Answer[A]])
			: Boolean =
			if (us.hasNext && them.hasNext)
			{
				val ourCurrent = us.next;
				val theirNext = them.next;
				
				if (ourCurrent === theirNext)
					loop (us, them);
				else
				{
					val theirCurrent = them.dropWhile (_ =/= ourCurrent);
					
					if (theirCurrent.hasNext && theirCurrent.next === ourCurrent)
						loop (us, them);
					else
						false;
				}
			}
			else
				us.isEmpty;
				
		return (loop (assignments.iterator, other.assignments.iterator));
	}
}


object SolutionTreeNode
{
	/// Class Imports
	import syntax.show._
	import syntax.std.boolean._
	
	
	/// Implicit Conversions
	implicit def nodeEqual[A] : Equal[SolutionTreeNode[A]] = Equal.equal {
		(a, b) =>
			
		a.assignments == b.assignments;
		}
	
	
	implicit def nodeMonoid[A] (implicit E : Equal[A], O : Ordering[Answer[A]])
		: Monoid[SolutionTreeNode[A]] =
		new Monoid[SolutionTreeNode[A]] {
			override val zero = SolutionTreeNode (SortedSet.empty[Answer[A]]);
			
			override def append (
				a : SolutionTreeNode[A],
				b : => SolutionTreeNode[A]
				)
				: SolutionTreeNode[A] =
				a.assignments.isEmpty.fold (
					b,
					SolutionTreeNode[A] (a.assignments ++ b.assignments)
					);
			}
	
	
	implicit def nodeShow[A : Show] : Show[SolutionTreeNode[A]] =
		new Show[SolutionTreeNode[A]] {
			/// Class Imports
			import Cord._
			
			
			override def show (n : SolutionTreeNode[A]) =
				Cord (
					"Node(",
					mkCord (",", n.assignments.map (_.show).toSeq : _*),
					")"
					);
			}
}
	