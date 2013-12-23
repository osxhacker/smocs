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
import runtime._
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
		with ConstraintTestingSupport
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
	
	case class SampleConstraintsProvider (
		override val constraints : Set[Constraint[String]],
		val variables : Traversable[Variable[String, DiscreteDomain]]
		)
		extends TestConstraintProvider[String]
			with SymbolTableProvider
	{
		override val symbols = variables.map (_.name).foldLeft (SymbolTable.empty) {
			case (st, n) =>
				
			st addSymbol (n);
			}
	}
	
	
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
	val constraints = problem.equations.list.map (canConstrain.constrains).toSet;
		
		
	"The VariableRankingPolicy" should "choose the smallest domain in ties" in
	{
		val policy = (
			ImpactRankingPolicy[String] () andThen
			PreferSmallerDomain[String] ()
			);
		
		val ranked = policy (SampleConstraintsProvider (constraints, variables)) (variables);
		
		ranked shouldNot be (null);
		ranked should have size (variables.size);
		ranked.head.name shouldBe (VariableName ('b));
	}
	
	it should "pick the most impacted variable" in
	{
		val additionalConstraint = canConstrain.constrains (
			new RelationalEquation {
				def apply = 'c < lit ("x");
				});
		val policy = (
			ImpactRankingPolicy[String] () andThen
			PreferSmallerDomain[String] ()
			);
		val provider = SampleConstraintsProvider (
			constraints + additionalConstraint,
			variables
			);
		
		val ranked = policy (provider) (variables);
		
		ranked shouldNot be (null);
		ranked should have size (variables.size);
		ranked.head.name shouldBe (VariableName ('c));
	}
}
