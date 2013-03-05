/**
 * Created on: Mar 3, 2013
 */
package com.tubros.constraints.core.internal.tree

import Predef.{
	any2stringadd => _,
	_
	}

import scalaz.{
	Ordering => _,
	_
	}

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api._
import com.tubros.constraints.core.spi.solver._

import problem._
import solver._


/**
 * The '''VariableRankingPolicySpec''' type exercises the
 * [[com.tubros.constraints.core.internal.tree.VariableRankingPolicy]] type
 * to ensure its heuristics perform as expected.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class VariableRankingPolicySpec
	extends ProjectSpec
{
	/// Class Imports
	import relational._
	import std.list._
	import std.string._
	
	
	/// Class Types
	trait RelationalEquation
		extends Equation[String]
			with PropositionalSupport[String]
			with RelationalSupport[String]
	
	
	/// Testing Collaborators
	implicit val canConstrain = implicitly[CanConstrain[Equation, String]];
	val variables = List[Variable[String, DiscreteDomain]] (
		DiscreteVariable ('a, FiniteDiscreteDomain ("1", "2", "3")),
		DiscreteVariable ('b, FiniteDiscreteDomain ("a", "b")),
		DiscreteVariable ('c, FiniteDiscreteDomain ("w", "x", "y", "z"))
		);
	val problem = Problem (
		new RelationalEquation {
			def apply = 'a < lit ("a");
			},
		new RelationalEquation {
			def apply = 'b <= 'c;
			}
		);
	val constraints = problem.equations.list.map (canConstrain.constrains);
		
		
	"The VariableRankingPolicy" should "choose the smallest domain in ties" in
	{
		val policy = (
			ImpactRankingPolicy[String] () andThen
			PreferSmallerDomain[String] ()
			);
		
		val ranked = policy[List].apply (constraints) (variables);
		
		ranked must not be === (null);
		ranked must have size (variables.size);
		ranked.head.name should be === ('b);
	}
	
	it should "pick the most impacted variable" in
	{
		val additionalConstraints = canConstrain.constrains (
			new RelationalEquation {
				def apply = 'c < lit ("x");
				}) :: constraints;
		val policy = (
			ImpactRankingPolicy[String] () andThen
			PreferSmallerDomain[String] ()
			);
		
		val ranked = policy[List].apply (additionalConstraints) (variables);
		
		ranked must not be === (null);
		ranked must have size (variables.size);
		ranked.head.name should be === ('c);
	}
}
