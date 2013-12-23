/**
 * Created on: Feb 13, 2013
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
import org.scalatest._

import problem._



/**
 * The '''SolveRelationalEquationUseCase''' type defines a
 * [[com.tubros.constraints.api.solver.SolverUseCaseSpec]] which defines the
 * expected behaviour of a CSP solver intended to solve
 * [[com.tubros.constraints.ap.Equation]]s involving only ''relational''
 * [[scala.Ordering]].
 *
 * @author svickers
 *
 */
trait SolveRelationalEquationUseCase[M[+_], SolverT <: Solver[Symbol, M, SolverT]]
	extends SolverUseCaseSpec[Symbol, M, SolverT]
{
	/// Class Imports
	import scalaz.std.AllInstances._
	import scalaz.syntax.monad._
	
	
	/// Class Types
	trait RelationalEquation
		extends Equation[Symbol]
			with PropositionalSupport[Symbol]
			with RelationalSupport[Symbol]
			
	implicit object OrderSymbols
		extends Ordering[Symbol]
	{
		/// Class Imports
		import Scalaz._
		
		
		/// Instance Properties
		private val mapping : Map[Symbol, Int] =
			('a' |=> 'z').map (c => Symbol (c.toString)).zipWithIndex.toMap;
		
		
		override def compare (x : Symbol, y : Symbol) : Int =
			mapping (x) - mapping (y);
	}
			
	/// Test Collaborators
	implicit val solvable : SolverUsage;
	implicit val unsolvable : SolverUsage;
	implicit val monad : Monad[M];

	
	feature ("Solving relational problems")
	{
		info ("The constraint solvers can work with types other than Numeric");
		info ("so long as whatever is used has an Ordering implicit defined");
		
		scenario ("Solving a problem where one object should preceed another")
		{
			import solvable._
			
			Given ("an ordering problem");
			
			val problem = Problem (
				new RelationalEquation {
					def apply = 'first < 'next && 'first <> 'head
					},
				new RelationalEquation {
					def apply = 'next <> lit ('b)
					}
				);
			val symbolValues = List ('a, 'b, 'c);
			
			When ("looking for a solution");
			
			val answers = withSolver[List] {
				solver =>
					
				for {
					_ <- solver.newVar ('head, domain (solver, List ('a)))
					_ <- solver.newVars[List] (domain (solver, symbolValues)) {
						List ('first, 'next);
						}
					_ <- solver.add (problem)
					stream <- solver.run[List]
					} yield stream;
				}.valueOr (_ => Stream.empty);
			
			Then ("there should be one answer");
			
			answers should not be ('empty);
			answers.head.sortBy (_.name.toString) shouldBe (
				List (
					Answer ('first -> 'b),
					Answer ('head -> 'a),
					Answer ('next -> 'c)
					)
				);
		}
	}
}
