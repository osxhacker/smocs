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
	
	
	"A TreeFiniteDomainSolver" should "be able to be constructed" in
	{
		val aSolver = new TreeFiniteDomainSolver[Int];
	}
	
	it should "produce a cartesian product with no constraints" in
	{
		val solver = new TreeFiniteDomainSolver[Int];
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
}
