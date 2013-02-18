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
	import algebraic._
	
	
	"A ExhaustiveFiniteDomainSolver" should "be able to be constructed" in
	{
		val solver = new ExhaustiveFiniteDomainSolver[Int];
	}
	
	it should "produce a cartesian product with no constraints" in
	{
		val solver = new ExhaustiveFiniteDomainSolver[Int];
		val domain = FiniteDiscreteDomain (1 to 5);
		val answer : Stream[List[Answer[Int]]] = solver {
			s =>
				
			for {
				x <- s.newVar ('x, domain)
				y <- s.newVar ('y, domain)
				stream <- s.run[List]
				} yield stream;
			}
			
		answer.contains (List (Answer ('x -> 1), Answer ('y -> 1))) should be === (true);
		answer.contains (List (Answer ('x -> 2), Answer ('y -> 5))) should be === (true);
		answer.contains (List (Answer ('x -> 5), Answer ('y -> 5))) should be === (true);
		answer should have size (25);
	}
	
	it should "produce answers with stable positions" in
	{
		val solver = new ExhaustiveFiniteDomainSolver[Int];
		val domain = FiniteDiscreteDomain (1 to 5);
		val answer : Stream[Stream[Answer[Int]]] = solver {
			s =>
				
			for {
				vars <- s.newVars[List] (domain) (List ('a, 'b, 'c, 'd))
				stream <- s.run[Stream]
				} yield stream;
			}
		
		answer must not be ('empty);
		
		answer.foreach {
			a =>
				
			a (0).name should be === ('a);
			a (1).name should be === ('b);
			a (2).name should be === ('c);
			a (3).name should be === ('d);
			}
	}
}
