/**
 * Created on: Mar 8, 2013
 */
package com.tubros.constraints.core.internal
package tree

import Predef.{
	any2stringadd => _,					// disable (x : Any) + "foo" conversions
	_
	}
import scala.language.higherKinds
import scalaz.{
	Ordering => _,
	_
	}

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api._

import problem._
import solver._


/**
 * The '''SolveKnapsackProblemSpec''' type uses the
 * [[com.tubros.constraints.core.internal.tree.TreeFiniteDomainSolver]] to sovle
 * the [[http://en.wikipedia.org/wiki/Knapsack_problem knapsack problem]].
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class SolveKnapsackProblemSpec
	extends SolverUseCaseSpec[
		Int,
		TreeFiniteDomainSolver[Int]#SolverState,
		TreeFiniteDomainSolver[Int]
		]
		with TreeSolverUsage[Int]
{
	/// Class Imports
	import algebraic._
	import std.list._
	import std.vector._
	import SolverUseCaseSpec._
	
	
	/// Instance Properties
	implicit val equal = Equal.equalA[Int];
	val knapsack = new DefaultTreeSolverUsage;

	
	feature ("Solving the knapsack constraint problem")
	{
		scenario ("When there are 18 units available")
		{
			import knapsack._
			
			Given ("size constriants of 4 whiskey, 3 perfume, and 2 cigs");
			
			val problem = Problem (
				new PolynomialEquation[Int] {
					def apply = 'whiskey * 4 + 'perfume * 3 + 'cigs * 2 <= 18
					},
				new PolynomialEquation[Int] {
					def apply = 'profit := 'whiskey * 15 + 'perfume * 10 + 'cigs * 5;
					},
				new PolynomialEquation[Int] {
					def apply = 'weight := 'whiskey * 10 + 'perfume * 1 + 'cigs * 3;
					},
				new PolynomialEquation[Int] {
					def apply = 'weight < 20 && 'profit > 0;
					}
				);
			
			When ("solving the problem");
			
			val answers = withSolver {
				solver =>
					
				for {
					_ <- solver.newVars[List] (domain (solver, 0 to 18)) {
						List ('whiskey, 'perfume, 'cigs);
						}
					_ <- solver.add (problem)
					stream <- solver.run[Vector]
					} yield stream;
				}
			
            Then ("there should be answers");
			answers should be ('right);
			answers foreach {
				result =>
					
				result should not be ('empty);
				}
			
            And ("the 'profit' Answer should be present");
			val best = answers map (
				_.sortBy (_.find (_.name == 'profit).get.value).reverse.take (5)
				);
			
            And ("each production satisfies the weight requirement");
			best map (
				_.foreach {
					result =>
						
					result should not be ('empty);
					result.map (_.toTuple).toMap.apply ('weight) should be < (20);
					}
				);
		}
	}
}
