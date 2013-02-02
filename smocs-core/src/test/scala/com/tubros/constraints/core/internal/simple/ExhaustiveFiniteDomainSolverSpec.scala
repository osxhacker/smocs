/**
 * Created on: Jan 31, 2013
 */
package com.tubros.constraints.core.internal.simple

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api._

import solver._


/**
 * The '''ExhaustiveFiniteDomainSolverSpec''' type defines the unit tests
 * for ensuring that the
 * [[com.tubros.constraints.core.internal.simple.ExhaustiveFiniteDomain]]
 * solver performs as expected.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class ExhaustiveFiniteDomainSolverSpec
	extends ProjectSpec
{
	/// Class Imports
	import scalaz.std.AllInstances._
	
	
	"A ExhaustiveFiniteDomainSolver" should "be able to be constructed" in
	{
		val solver = new ExhaustiveFiniteDomainSolver[Int];
	}
	
	it should "produce a cartesian product with no constraints" in
	{
		val solver = new ExhaustiveFiniteDomainSolver[Int];
		val domain = FiniteDiscreteDomain (1 to 5);
		val answer : Stream[List[Int]] = solver {
			s =>
				
			for {
				x <- s.newVar ('x, domain)
				y <- s.newVar ('y, domain)
				stream <- s.run[List]
				} yield stream;
			}
			
		answer.contains (List (1, 1)) should be === (true);
		answer.contains (List (2, 5)) should be === (true);
		answer.contains (List (5, 5)) should be === (true);
	}
}
