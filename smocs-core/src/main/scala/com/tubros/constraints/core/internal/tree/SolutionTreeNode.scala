/**
 * Created on: Mar 14, 2013
 */
package com.tubros.constraints.core.internal
package tree

import scala.collection.GenTraversableOnce
import scala.collection.immutable.SortedSet
import scala.language.higherKinds

import scalaz.{
	Ordering => _,
	_
	}

import com.tubros.constraints.api.solver.Answer


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
	(implicit o : Ordering[Answer[A]])
	extends SolutionSpace.Node[A]
{
	/// Class Imports
	import Scalaz._
	
	
	/// Instance Properties
	lazy val isEmpty : Boolean = assignments.isEmpty;
	
	
	def :+ (answer : Answer[A]) : SolutionTreeNode[A] =
		copy (assignments + answer);
	
	
	def flatMap (f : GenTraversableOnce[Answer[A]] => SolutionTreeNode[A])
		: SolutionTreeNode[A] =
		f (assignments);
		
	
	def map (f : GenTraversableOnce[Answer[A]] => GenTraversableOnce[Answer[A]])
		: SolutionTreeNode[A] =
		SolutionTreeNode[A] (SortedSet.empty[Answer[A]] ++ f (assignments));
}


object SolutionTreeNode
{
	/// Class Imports
	import syntax.std.boolean._
	
	
	/// Implicit Conversions
	implicit def nodeApplicative[A] (implicit o : Ordering[Answer[A]])
		: Applicative[({ type L[X] = SolutionTreeNode[A] })#L] =
		nodeMonoid[A].applicative;
	
	
	implicit def nodeEqual[A] : Equal[SolutionTreeNode[A]] = Equal.equal {
		(a, b) =>
			
		a.assignments == b.assignments;
		}
	
	
	implicit def nodeMonoid[A] (implicit o : Ordering[Answer[A]])
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
	
	
	implicit def nodeShow[A] : Show[SolutionTreeNode[A]] =
		new Show[SolutionTreeNode[A]] {
			override def shows (n : SolutionTreeNode[A]) = n.toString;
			}
}
	