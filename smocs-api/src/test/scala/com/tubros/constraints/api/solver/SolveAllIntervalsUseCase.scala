/**
 * Created on: Mar 5, 2013
 */
package com.tubros.constraints.api
package solver

import Predef.{
	any2stringadd => _,					// disable (x : Any) + "foo" conversions
	_
	}
import scala.language.higherKinds

import scalaz._

import problem._


/**
 * The '''SolveAllIntervalsUseCase''' type defines a Use-Case in which the
 * "all intervals" CSP is solved.
 *
 * @author svickers
 *
 */
trait SolveAllIntervalsUseCase[M[+_], SolverT <: Solver[Int, M, SolverT]]
	extends SolverUseCaseSpec[Int, M, SolverT]
{
	/// Class Imports
	import Scalaz._
	import SolveAllIntervalsUseCase._
	
	
	/// Test Collaborators
	implicit val fiveElements : SolverUsage;
	implicit val unsolvable : SolverUsage;
	implicit val monad : Monad[M];
	
	
	feature ("Solving the 'all intervals' constraint problem")
	{
		info ("A well-known CSP is called the 'all intervals' problem and is");
		info ("a suitable real-world CSP representative");
		
		scenario ("An intervals problem capable of being solved")
		{
			import fiveElements._
			
			Given ("an 'all intervals' definition with 5 elements");
			
			/// Due to a conflict between the Scalatest implicit for Symbols,
			/// the equation is defined in the companion object below.
			val problem = fiveElementProblem;
			val alldiff : GlobalConstraint[Int] = {
				case ArrayVariables (xs, diffs) =>
					(
						xs.isEmpty || xs.values.to[Set].size == xs.size
					) &&
					(
						diffs.isEmpty || diffs.values.to[Set].size == diffs.size
					)
				}
			
			When ("solving the problem");
			
			val answers = withSolver {
				solver =>
					
				for {
					_ <- solver.newArrayVar ('x, 5, domain (solver, 0 to 4))
					_ <- solver.newArrayVar ('diffs, 4, domain (solver, 1 to 4))
					_ <- solver.impose (alldiff)
					_ <- solver.add (problem)
					stream <- solver.run[Vector]
					} yield stream;
				}
			
			Then ("there should be at least one answer");
			
			answers should be ('right);
			answers foreach {
				a =>
					
				a should not be ('empty);
				}
		}
	}
}


object SolveAllIntervalsUseCase
{
	/// Class Imports
	import std.indexedSeq._
	import syntax.std.indexedSeq._
	import SolverUseCaseSpec._
	
	
	/// Instance Properties
	val fiveElementProblem = createAllIntervalsProblem (5);
	
	
	private def createAllIntervalsProblem (size : Int) : Problem[Int] =
	{
		val additionalConstraints = NonEmptyList (
			new PolynomialEquation[Int] {
				def apply = 'diffs (0) < 'diffs (1);
				},
			new PolynomialEquation[Int] {
				def apply = 'x (0) < 'x (size - 1)
				}
			);
		val diffEquations =
			for (n <- 0 until size)
				yield new PolynomialEquation[Int] {
					def apply = 'diffs (n) === abs ('x (n + 1) - 'x (n));
					}
		
		return (Problem (diffEquations.to[List] <::: additionalConstraints));
	}
}
