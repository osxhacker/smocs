/**
 * Created on: Feb 4, 2013
 */
package com.tubros.constraints.api
package solver

import Predef.{
	any2stringadd => _,					// disable (x : Any) + "foo" conversions
	_
	}

import scalaz._

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import problem._


/**
 * The '''SolvePolynomialEquationUseCase''' type defines a polynomial equation
 * Use Case and performs several `scenario`s designed to verify expected
 * behaviour.  Note that this [[com.tubros.constraints.api.UseCaseSpec]]
 * differs from many in that it is parameterized on the types needed by
 * [[com.tubros.constraints.api.solver.Solver]].
 *
 * @author svickers
 *
 */
trait SolvePolynomialEquationUseCase[M[+_], SolverT <: Solver[Int, M, SolverT]]
	extends UseCaseSpec
{
	/// Class Imports
	import Scalaz._
	
	
	/// Class Types
	trait PolynomialEquation
		extends Equation
			with ArithmeticSupport
			with RelationalSupport
			
			
	trait SolverUse
	{
		def withSolver[C[_]] (block : SolverT => M[Stream[C[Answer[Int]]]])
			(implicit a : Applicative[C], mo : Monoid[C[Answer[Int]]])
			: Stream[C[Answer[Int]]];
		
		def domain (solver : SolverT, range : Range) : solver.DomainType[Int];
		
	}
	
	
	/// Test Collaborators
	implicit val solvable : SolverUse;
	implicit val unsolvable : SolverUse;
	implicit val monad : Monad[M];
	
	
	feature ("Solving polynomial equations")
	{
		info ("The CPS Solver can be used to attempt to solve arbitrarty");
		info ("polynomial equations.");
		
		scenario ("A polynomial which can be solved within the given domain")
		{
			import solvable._
			
			Given ("a solvable polynomial Equation");
			
			val polynomial = new PolynomialEquation {
				def apply = 'x ^ 2 + 'x * 4 - 4 @== 'y ^ 3;
				}
			
			When ("solving the equation");
			
			val answers = withSolver {
				solver =>
					
				for {
					x <- solver.newVar ('x, domain (solver, 1 to 10))
					y <- solver.newVar ('y, domain (solver, 0 to 5))
					_ <- solver.add (polynomial.some)
					stream <- solver.run[Set]
					} yield stream;
				}
			
			Then ("there should be one answer");
			
			answers should not be ('empty);
			answers should have size (1);
			answers.head should be === (
				Set (Answer ('x -> 2), Answer ('y -> 2))
				);
		}
		
		scenario ("Trying to solve an unsolvable polynomial")
		{
			import unsolvable._
			
			Given ("a valid, but unsolvable within the domain, Equation");
		
			val polynomial = new PolynomialEquation {
				def apply = ('x ^ 5) - ('x * 3) + 1 @== 0;
				}
		
			When ("attempting to solve the Equation");
			
			val answers = withSolver {
				solver =>
					
				for {
					x <- solver.newVar ('x, domain (solver, -10 to 10));
					_ <- solver.add (polynomial.some)
					stream <- solver.run[List]
					} yield stream;
				}
				
			Then ("there should be no answers");
			
			answers should be ('empty);
		}
	}
}
