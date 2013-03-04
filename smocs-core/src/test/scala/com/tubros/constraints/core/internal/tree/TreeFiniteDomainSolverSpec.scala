/**
 * Created on: Feb 22, 2013
 */
package com.tubros.constraints.core.internal
package tree

import Predef.{
	any2stringadd => _,
	_
	}

import scalaz._

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api._
import com.tubros.constraints.api.problem._
import com.tubros.constraints.api.solver._
import com.tubros.constraints.core.spi.solver._


/**
 * The '''TreeFiniteDomainSolverSpec''' type defines the unit tests which
 * certify the
 * [[com.tubros.constraints.core.internal.graph.TreeFiniteDomainSolver]] for
 * use as a CSP [[com.tubros.constraints.api.solver.Solver]].
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class TreeFiniteDomainSolverSpec
	extends ProjectSpec
{
	/// Class Imports
	import scalaz.std.AllInstances._
	import algebraic._
	
	
	/// Class Types
	trait PolynomialEquation[T]
		extends Equation[T]
			with ArithmeticSupport[T]
			with PropositionalSupport[T]
			with RelationalSupport[T]
	
	
	/// Testing Collaborators
	val rankingPolicy =
		ImpactRankingPolicy[Int] () andThen PreferSmallerDomain[Int] ();
	
	
	"A TreeFiniteDomainSolver" should "be able to be constructed" in
	{
		val aSolver = new TreeFiniteDomainSolver[Int] (rankingPolicy);
	}
	
	it should "produce a cartesian product with no constraints" in
	{
		val solver = new TreeFiniteDomainSolver[Int] (rankingPolicy);
		val domain = FiniteDiscreteDomain (1 to 10);
		val answer = solver {
			s =>
				
			for {
				_ <- s.newVar ('a, domain)
				_ <- s.newVar ('b, domain)
				stream <- s.run[Vector]
				} yield stream;
			}
		val expected = for {
			a <- domain
			b <- domain
			} yield (a, b);
			
		answer should have size (domain.size * domain.size);
		expected foreach {
			case (a, b) =>
				
			answer.contains (List (Answer ('a, a), Answer ('b, b))) should be === (true);
			}
	}
	
	it should "employ constraint propagation" in
	{
		val problem = Problem (
			new PolynomialEquation[Int] {
				def apply = 'y @== 'x ** 3 
				},
			new PolynomialEquation[Int] {
				def apply = 'x @== 2
				}
			);
		val solver = new TreeFiniteDomainSolver[Int] (rankingPolicy);
		val domain = FiniteDiscreteDomain (1 to 1000);
		val answer = solver {
			s =>
				
			for {
				_ <- s.newVar ('x, domain)
				_ <- s.newVar ('y, domain)
				_ <- s.add (problem)
				stream <- s.run[Vector]
				} yield stream;
			}
		
		answer should not be ('empty);
		answer should have size (1);
		answer.head should be === (Vector (Answer ('x, 2), Answer ('y, 8)));
	}
}
