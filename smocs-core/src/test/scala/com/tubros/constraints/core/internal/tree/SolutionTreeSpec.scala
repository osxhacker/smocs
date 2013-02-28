/**
 * Created on: Feb 22, 2013
 */
package com.tubros.constraints.core.internal
package tree

import scalaz._

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
			valuesFor = (variable : Variable[Int, DiscreteDomain]) => variable.domain
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
			valuesFor = (variable : Variable[Int, DiscreteDomain]) => variable.domain
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
			valuesFor = (_ : Variable[Int, DiscreteDomain]).domain
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
			valuesFor = (_ : Variable[Int, DiscreteDomain]) => List.empty[Int]
			);
		
		expanded.frontier should be ('empty);
	}
}