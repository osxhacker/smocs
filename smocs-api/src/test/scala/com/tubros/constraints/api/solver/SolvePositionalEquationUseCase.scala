/**
 * Created on: Feb 19, 2013
 */
package com.tubros.constraints.api
package solver

import Predef.{
	any2stringadd => _,					// disable (x : Any) + "foo" conversions
	_
	}
import scala.language.higherKinds

import scalaz.{
	Ordering => _,
	_
	}
import scalaz.syntax.{
	ToOrderOps => _
	}

import org.scalatest._

import problem._


/**
 * The '''SolvePositionalEquationUseCase''' type defines a positional equation
 * Use Case and performs several `scenario`s designed to verify expected
 * behaviour.  Note that this [[com.tubros.constraints.api.UseCaseSpec]]
 * differs from many in that it is parameterized on the types needed by
 * [[com.tubros.constraints.api.solver.Solver]].
 *
 * @author svickers
 *
 */
trait SolvePositionalEquationUseCase[
	M[+_],
	SolverT <: Solver[Symbol, M, SolverT]
	]
	extends SolverUseCaseSpec[Symbol, M, SolverT]
{
	/// Class Imports
	//import Scalaz._
	import scalaz.std.AllInstances._
	import scalaz.syntax.std.list._
	import scalaz.syntax.monad._
	import scalaz.syntax.monoid._
	
	/// Class Types
	trait PositionalEquation
		extends Equation[Symbol]
			with PositionalSupport[Symbol]
			with PropositionalSupport[Symbol]
			with RelationalSupport[Symbol]
	
	implicit object SymbolOrdering
		extends Ordering[Symbol]
	{
		/**
		 * For this test, we're going to define compare as being 'odds' first
		 * and then 'evens', with 'evens' being in descending order.
		 */
		override def compare (x : Symbol, y : Symbol) : Int =
		{
			val (xVal :: yVal :: Nil) = List (x, y).map (_.name).map {
				n =>
					
				n.reverse.head.toInt;
				};
				
			(xVal % 2, yVal % 2) match {
				case (1, 1) => xVal - yVal;
				case (1, 0) => -1;
				case (0, 1) => 1;
				case (0, 0) => yVal - xVal;
				}
		}
	}

	/// Test Collaborators
	implicit val solvable : SolverUsage;
	implicit val unsolvable : SolverUsage;
	implicit val monad : Monad[M];
	
	val allDiff : List[Symbol] => Boolean =
		candidate => candidate.to[Set].size == candidate.size;
	
	
	feature ("Solving equations based on the 'position' of the answers")
	{
		info ("There is a category of problems which the actual value of each");
		info ("answer is not the concern, but instead *where* the value is");
		info ("in relation to other values.");
		
		scenario ("Requiring adjacency")
		{
			import solvable._
			
			Given ("the Equation 'a @== 'b - 1");
			
			val equation = new PositionalEquation {
				def apply = ('a @== 'b - 1) && ('a > first);
				}
			
			And ("a Domain of ['v1, 'v2, 'v3, 'v4]");
			
			val values = List ('v1, 'v2, 'v3, 'v4);
			
			When ("solving a positional Equation");
			
			val answers = withSolver {
				solver =>
					
				for {
					_ <- solver.newVars[List] (domain (solver, values)) {
						List ('a, 'b, 'c);
						}
					_ <- solver.add (equation)
					_ <- solver.impose (allDiff)
					resultSet <- solver.run[Vector]
					} yield resultSet;
				}
			
			Then ("there should be an answer");
			answers should not be ('empty);
			
			And ("each one should have adjacent (a, b) values");
			answers.forall {
				answer =>
					
				implicitly[Ordering[Symbol]].lt (
					answer (0).value,
					answer (1).value
					);
				} should be === (true);
				
			And ("each answer should not have 'a <> 'v1");
			answers.toList.map {
				answer =>
					
				answer (0).value;
				}.filter (_ == 'v1) should be ('empty);
		}
	}
}
