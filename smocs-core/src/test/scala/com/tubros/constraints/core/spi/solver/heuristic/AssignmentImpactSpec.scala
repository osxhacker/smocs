/**
 * Created on: Mar 2, 2013
 */
package com.tubros.constraints.core.spi.solver
package heuristic

import Predef.{
	any2stringadd => _,
	_
	}

import scalaz._

import org.junit.runner.RunWith
import org.scalamock.scalatest.MockFactory
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api._
import com.tubros.constraints.core.internal.BuiltinEquationConstraintCategories

import problem._
import runtime._
import solver._


/**
 * The '''AssignmentImpactSpec''' type contains the unit tests used to ensure
 * that the
 * [[com.tubros.constraints.core.spi.solver.heuristic.AssignmentImpact]]
 * heuristic operates as expected for Smocs.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class AssignmentImpactSpec
	extends ProjectSpec
		with BuiltinEquationConstraintCategories
		with ConstraintTestingSupport
{
	/// Class Imports
	import algebraic._
	import std.anyVal._
	import std.list._
	import std.vector._
	
	
	/// Class Types
	trait PolyEquation
		extends Equation[Int]
			with ArithmeticSupport[Int]
			with PropositionalSupport[Int]
			with RelationalSupport[Int]
	
	
	/// Testing Collaborators
	implicit val canConstrain = implicitly[CanConstrain[Equation, Int]];
	val emptyVariables = List.empty[Variable[Int, DiscreteDomain]];
	val alpha = DiscreteVariable ('a, FiniteDiscreteDomain (0 until 10));
	val beta = DiscreteVariable ('b, FiniteDiscreteDomain (0 until 20));
	val gamma = DiscreteVariable ('c, FiniteDiscreteDomain (0 until 30));
	val variables = List[Variable[Int, DiscreteDomain]] (alpha, beta, gamma);
	val problem = Problem (
		new PolyEquation {
			def apply = 'c ** 2 @== 'a ** 2 + 'b ** 2;
			},
		new PolyEquation {
			def apply = 'a <> 'b - 1;
			},
		new PolyEquation {
			def apply = 'b < 17;
			},
		new PolyEquation {
			def apply = 'a > 0 && 'a < 5;
			}
		);
	
	object TestConstraints
		extends TestConstraintProvider[Int]
			with SymbolTableProvider
	{
		val constraints = problem.equations.map (canConstrain.constrains).list.to[Set];
		val symbols = variables.foldLeft (SymbolTable.empty) {
			case (st, v) =>
				
			st addSymbol (v.name);
			}
	}
	
	
	"The AssignmentImpact heuristic" should "gracefully deal with empty sets" in
	{
		val assessor = AssignmentImpact (emptyVariables, TestConstraints);
		
		assessor (Seq.empty[Answer[Int]], alpha, 0) should be === (1.0);
	}
	
	it should "prefer an assignment which does not reduce the domain" in
	{
		val assessor = AssignmentImpact (variables, TestConstraints);
		val noImpact = assessor (Seq (Answer ('a -> 2)), beta, 2);
		val impactsDomain = assessor (Seq (Answer ('a -> 2)), beta, 3);
		
		noImpact should be < (1.0);
		noImpact should be < (impactsDomain);
	}
	
	it should "calculate the impact of a variable" in
	{
		val assessor = AssignmentImpact (variables, TestConstraints);
		val impactOfAlpha = assessor.ofVariable (alpha);
		val impactOfBeta = assessor.ofVariable (beta);
		
		impactOfAlpha should be > (impactOfBeta);
	}
	
	it should "assess relative variable impact" in
	{
		val xIsMostImpactedProblem = Problem (
			new PolyEquation {
				def apply = 'y @== 'x ** 3 
				},
			new PolyEquation {
				def apply = 'x @== 2
				}
			);
		val x = DiscreteVariable ('x, FiniteDiscreteDomain (1 to 1000));
		val y = DiscreteVariable ('y, FiniteDiscreteDomain (1 to 1000));
		val provider = new TestConstraintProvider[Int] with SymbolTableProvider {
			
			val constraints = xIsMostImpactedProblem.equations.map (
				canConstrain.constrains
				).list.to[Set];
			
			val symbols = SymbolTable.empty addSymbol ('x) addSymbol ('y);
			}
		val assessor = AssignmentImpact (
			Vector[Variable[Int, DiscreteDomain]] (x, y),
			provider
			);
		
		assessor.ofVariable (x) should be > (assessor.ofVariable (y));
	}
}
