/**
 * Created on: Feb 22, 2013
 */
package com.tubros.constraints.core.internal
package tree

import scalaz.{
	Ordering => _,
	_
	}

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api._
import com.tubros.constraints.api.solver._
import com.tubros.constraints.core.spi.solver._


/**
 * The '''SolutionTreeSpec''' type defines the unit tests designed to certify
 * the [[com.tubros.constraints.core.internal.graph.SolutionTree]] type.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class SolutionTreeSpec
	extends ProjectSpec
{
	/// Class Imports
	import std.AllInstances._
	import syntax.show._
	import syntax.std.option._
	import SolutionTree._
	import SolutionTree.implicits._
	
	
	/// Testing Collaborators
	implicit def answerOrdering[A] = new Ordering[Answer[A]] {
		override def compare (x : Answer[A], y : Answer[A]) =
			x.name.toString.compareTo (y.name.toString);
		}
	
	
	"A SolutionTree" should "be agnostic to its node type" in
	{
		val intTree = SolutionTree[Int] ();
		val stringTree = SolutionTree[String] ();

		intTree must not be === (null);
		intTree.frontier should be ('empty);
		
		stringTree must not be === (null);
		stringTree.frontier should be ('empty);
	}
	
	it should "be able to expand the solution space one level deep" in
	{
		val empty = SolutionTree[Int] ();
		val variables = List[Variable[Int, DiscreteDomain]] (
			DiscreteVariable ('a, FiniteDiscreteDomain (1, 2, 3))
			);
		val expanded = empty.expand (
			empty.root,
			variables,
			valuesFor = (_, variable : Variable[Int, DiscreteDomain]) => variable
			);
		
		expanded.frontier should not be ('empty);
	}
	
	it should "be able to expand the solution space multiple levels deep" in
	{
		val empty = SolutionTree[Int] ();
		val variables = List[Variable[Int, DiscreteDomain]] (
			DiscreteVariable ('a, FiniteDiscreteDomain (1, 2, 3)),
			DiscreteVariable ('b, FiniteDiscreteDomain (10, 20)),
			DiscreteVariable ('c, FiniteDiscreteDomain (100, 200))
			);
		val expanded = empty.expand (
			empty.root,
			variables,
			valuesFor = (_, variable : Variable[Int, DiscreteDomain]) => variable
			);
		
		expanded.frontier should not be ('empty);
		
		// With this expansion, each a -> b -> c(100, 200) node on the frontier
		expanded.frontier.size should be === (12);
	}
	
	it should "be able to handle a Variable with an empty Domain" in
	{
		val empty = SolutionTree[Int] ();
		val preExpansionVariables = List[Variable[Int, DiscreteDomain]] (
			DiscreteVariable ('x)
			);
		val expanded = empty.expand (
			empty.root,
			preExpansionVariables,
			valuesFor = (_, variable : Variable[Int, DiscreteDomain]) => variable
			);
		
		expanded.frontier should be ('empty);
	}
	
	it should "be able to handle a Variable with no selected values" in
	{
		val empty = SolutionTree[Int] ();
		val preExpansionVariables = List[Variable[Int, DiscreteDomain]] (
			DiscreteVariable ('x, FiniteDiscreteDomain (0 to 100))
			);
		val expanded = empty.expand (
			empty.root,
			preExpansionVariables,
			valuesFor = (_, v : Variable[Int, DiscreteDomain]) => v.filter (_ => false)
			);
		
		expanded.frontier should be ('empty);
	}
	
	it should "support flatMap operations" in
	{
		val identitySelector :
			(Iterable[Answer[Int]], Variable[Int, DiscreteDomain])
				=> Variable[Int, DiscreteDomain] =
			(ia, variable) => variable;
			
		val expanded = SolutionTree[Int] (
			DiscreteVariable ('a, FiniteDiscreteDomain (1, 2, 3)),
			valuesFor = identitySelector
			);
		
		expanded.frontier should not be ('empty);
		
		val mapped = expanded flatMap (SolutionTree.fromFrontier[Int]);
		
		mapped.root.getLabel should be === (expanded.root.getLabel);
		mapped.root.hasChildren should be === (true);
	}
	
	it should "be able to iteratively search" in
	{
		val variables = List[Variable[Int, DiscreteDomain]] (
			DiscreteVariable ('a, FiniteDiscreteDomain (1, 2, 3)),
			DiscreteVariable ('b, FiniteDiscreteDomain (10, 20)),
			DiscreteVariable ('c, FiniteDiscreteDomain (100, 200))
			);
		val selector : SolutionTree[Int]#ValueGenerator =
			(bound, variable) => variable;
		val expanded = SolutionTree[Int] (
			variables.head,
			valuesFor = selector
			);
		
		expanded.frontier should not be ('empty);
		expanded.frontier.dequeue._1 should be ('defined);
		expanded.frontier.dequeue._1.get.assignments.size should be === (1);
		
		val secondLevel = expanded flatMap {
			node =>
				
			SolutionTree.fromFrontier (node).search (
				variables,
				(vars : List[Variable[Int, DiscreteDomain]]) => List (vars (1)),
				selector
				).getOrElse (SolutionTree.empty[Int])
			}
		
		secondLevel should not be === (null);
		
		secondLevel.focus must not be === (null);
		secondLevel.focus.hasChildren should be === (true);
		secondLevel.root.firstChild should be ('defined);
		secondLevel.root.firstChild.map (_.rights.size) should be === (Some (2));
		secondLevel.frontier should not be ('empty);
		secondLevel.frontier.dequeue._1 should be ('defined);
		secondLevel.frontier.dequeue._1.get.assignments.size should be === (2);
		
		val thirdLevel = secondLevel flatMap {
			SolutionTree.fromFrontier (_).search (
				variables,
				(vars : List[Variable[Int, DiscreteDomain]]) => List (vars (2)),
				selector
				).getOrElse (SolutionTree.empty[Int])
			}
		
		thirdLevel.focus must not be === (null);
		thirdLevel.focus.hasChildren should be === (true);
		thirdLevel.frontier should not be ('empty);
		thirdLevel.frontier.dequeue._1 should be ('defined);
		thirdLevel.frontier.dequeue._1.get.assignments.size should be === (3);
	}
}
