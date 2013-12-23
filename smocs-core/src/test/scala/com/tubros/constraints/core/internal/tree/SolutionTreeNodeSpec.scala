/**
 * Created on: Sep 28, 2013
 */
package com.tubros.constraints.core.internal
package tree

import scala.collection.immutable.SortedSet

import scalaz.{
	Ordering => _,
	_
	}

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api.{
	ProjectSpec,
	VariableName
	}
import com.tubros.constraints.api.solver._
import com.tubros.constraints.core.spi.solver._


/**
 * The '''SolutionTreeNodeSpec''' type exercises the
 * [[com.tubros.constraints.core.internal.tree.SolutionTreeNode]] type to ensure
 * its correct operation when managed by the
 * [[com.tubros.constraints.core.internal.tree.SolutionTree]] `class`.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class SolutionTreeNodeSpec
	extends ProjectSpec
{
	/// Class Imports
	import std.AllInstances._
	import syntax.monoid._
	
	
	/// Testing Collaborators
	implicit def nameBasedOrderingAnswer[A] (implicit O : Ordering[A])
		: Ordering[Answer[A]] =
		new Ordering[Answer[A]] {
			override def compare (a : Answer[A], b : Answer[A]) : Int =
				a.name.name.compareTo (b.name.name);
			}
		
		
	"The SolutionTreeNode" should "be a model of SolutionSpace.Node" in
	{
		classOf[SolutionSpace.Node[_]].isAssignableFrom (
			classOf[SolutionTreeNode[_]]
			) shouldBe (true);
	}
	
	it should "support an empty assignments Set" in
	{
		val node = SolutionTreeNode (
			SortedSet.empty[Answer[String]],
			Seq.empty
			);
		
		node should be ('empty);
		node.size shouldBe (0);
	}
	
	it should "be able to identify unassigned Answers" in
	{
		val node = SolutionTreeNode[Int] (
			createAssignments (
				'a -> 1,
				'b -> 2,
				'c -> 3
				),
			createAssignments ('c -> 3).toSeq
			);
		
		node should not be ('empty);
		node.unassigned (createVariables ('a)) should be ('empty);
		node.unassigned (createVariables ('z)) should have size (1);
		node.unassigned (createVariables ('a, 'b, 'c, 'd, 'e)) should have size (2);
	}
	
	it should "identify an empty node as a subset of another empty node" in
	{
		val empty = mzero[SolutionTreeNode[Int]];
		
		empty.isSubsetOf (mzero[SolutionTreeNode[Int]]) shouldBe (true);
	}
	
	it should "identify a proper subset" in
	{
		val subset = SolutionTreeNode[Double] (
			createAssignments ('g -> 1.0),
			createAssignments ('g -> 1.0).toSeq
			);
		val superset = SolutionTreeNode[Double] (
			createAssignments ('g -> 1.0, 'p -> 3.0),
			createAssignments ('p -> 3.0).toSeq
			);
		
		subset.isSubsetOf (superset) shouldBe (true);
	}
	
	it should "identify an empty node as a subset of a non-empty node" in
	{
		val empty = mzero[SolutionTreeNode[Double]];
		val populated = SolutionTreeNode[Double] (
			createAssignments ('a -> 1.0, 'b -> 2.0),
			createAssignments ('b -> 2.0).toSeq
			);
		
		empty.isSubsetOf (populated) shouldBe (true);
	}
	
	it should "not identify a non-empty node as a subset of an empty node" in
	{
		val empty = mzero[SolutionTreeNode[Double]];
		val populated = SolutionTreeNode[Double] (
			createAssignments ('a -> 1.0, 'b -> 2.0),
			createAssignments ('b -> 2.0).toSeq
			);
		
		populated.isSubsetOf (empty) shouldNot be (true);
	}
	
	it should "ignore prefix elements when doing subset determination" in
	{
		val subset = SolutionTreeNode[Double] (
			createAssignments ('g -> 1.0, 'p -> 2.0),
			createAssignments ('g -> 1.0, 'p -> 2.0).toSeq
			);
		val superset = SolutionTreeNode[Double] (
			createAssignments ('a -> 0.0, 'g -> 1.0, 'p -> 2.0),
			createAssignments ('a -> 0.0).toSeq
			);
		
		subset.isSubsetOf (superset) shouldBe (true);
	}
	
	it should "ignore suffix elements when doing subset determination" in
	{
		val subset = SolutionTreeNode[Double] (
			createAssignments ('g -> 1.0, 'p -> 2.0),
			createAssignments ('g -> 1.0, 'p -> 2.0).toSeq
			);
		val superset = SolutionTreeNode[Double] (
			createAssignments ('g -> 1.0, 'p -> 2.0, 'z -> 99.0),
			createAssignments ('z -> 99.0).toSeq
			);
		
		subset.isSubsetOf (superset) shouldBe (true);
	}
	
	it should "ignore infix elements when doing subset determination" in
	{
		val subset = SolutionTreeNode[Double] (
			createAssignments ('g -> 1.0, 'p -> 2.0),
			createAssignments ('p -> 2.0).toSeq
			);
		val superset = SolutionTreeNode[Double] (
			createAssignments ('g -> 1.0, 'j -> 1.5, 'p -> 2.0),
			createAssignments ('j -> 1.5).toSeq
			);
		
		subset.isSubsetOf (superset) shouldBe (true);
	}
	
	
	private def createAssignments[A] (pairs : (Symbol, A) *)
		(implicit O : Ordering[Answer[A]])
		: SortedSet[Answer[A]] =
		SortedSet.empty[Answer[A]] ++ pairs.map {
			p =>

			Answer (VariableName (p._1), p._2)
			}
	
	
	private def createVariables (names : Symbol *)
		: Vector[Variable[Int, DiscreteDomain]] =
		names.map {
			name =>
				
			val vn = VariableName (name);
			val domain = FiniteDiscreteDomain[Int] (0 until 10);

			DiscreteVariable[Int] (vn, domain);
			}.to[Vector];
}
