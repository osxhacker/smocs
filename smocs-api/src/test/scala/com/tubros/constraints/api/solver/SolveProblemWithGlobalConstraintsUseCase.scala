/**
 * Created on: Feb 17, 2013
 */
package com.tubros.constraints.api
package solver

import Predef.{
	any2stringadd => _,					// disable (x : Any) + "foo" conversions
	_
	}
import scala.language.{
	higherKinds,
	postfixOps
	}

import scalaz._

import problem._


/**
 * The '''SolveProblemWithGlobalConstraintsUseCase''' type embodies the Use
 * Case of a [[com.tubros.constraints.api.solver.Solver]] having ''global''
 * constraints it must satisfy when producing a solution.  These types of
 * constraints differ from an [[com.tubros.constraints.api.problem.Equation]]
 * in that they typically involve concerns which transcend a particular
 * formalization of a sub-problem.
 *
 * @author svickers
 *
 */
trait SolveProblemWithGlobalConstraintsUseCase[
	M[+_],
	SolverT <: Solver[Double, M, SolverT]
	]
	extends SolverUseCaseSpec[Double, M, SolverT]
{
	/// Class Imports
	import Scalaz._
	import SolverUseCaseSpec._
	
	
	/// Test Collaborators
	implicit val monad : Monad[M];
	val allDiffConstrained : SolverUsage;
	val allSameConstrained : SolverUsage;
	
	val allDiff : Seq[Double] => Boolean = candidate =>
		candidate.distinct.size == candidate.size;
	val allSame : Set[Double] => Boolean = candidate =>
		candidate.size == 1;
	val increasing : Seq[Double] => Boolean = candidate =>
		candidate.sorted == candidate;

	
	feature ("Applying 'global' constraints to a Problem")
	{
		info ("Many CSP's have specialized, or 'higher-kinded', constraints");
		info ("which they must satisfy in addition to Answers which conform");
		info ("to the given Equations.");
		
		scenario ("Enforcing 'alldiff'")
		{
			import allDiffConstrained._
			
			Given ("an Equation of 'x ** 3 @== 'y * 2")
			
			val polynomial = new PolynomialEquation[Double] {
				def apply = 'x ** 3 @== 'y * 2;
				}
			val range = (0 |=> 10) map (_.toDouble) toSeq;
				
			When ("solving the Equation having an alldiff requirement");
			
			val answers = withSolver {
				solver =>
					
				for {
					_ <- solver.newVars[List] (domain (solver, range)) {
						List ('x, 'y);
						}
					_ <- solver.add (polynomial)
					_ <- solver.impose (allDiff)
					resultSet <- solver.run[Vector]
					} yield resultSet;
				}.valueOr (_ => Stream.empty);
			
			Then ("there should be an answer");
			answers should not be ('empty);
			
			And ("each one should have different (x, y) values");
			answers.forall {
				answer =>
					
				allDiff (answer.map (_.copoint)) &&
				increasing (answer.map (_.copoint));
				} shouldBe (true);
		}
		
		scenario ("Enforcing 'alldiff' and 'increasing'")
		{
			import allDiffConstrained._
			
			Given ("an Equation of 'x ** 3 @== 'y * 2")
			
			val polynomial = new PolynomialEquation[Double] {
				def apply = 'x ** 3 @== 'y * 2;
				}
			val range = (0 |=> 10) map (_.toDouble) toSeq;
				
			When ("solving with alldiff and increasing requirements");
			
			val answers = withSolver {
				solver =>
					
				for {
					_ <- solver.newVars[List] (domain (solver, range)) {
						List ('x, 'y);
						}
					_ <- solver.add (polynomial)
					_ <- solver.impose (allDiff)
					_ <- solver.impose (increasing)
					resultSet <- solver.run[Vector]
					} yield resultSet;
				}.valueOr (_ => Stream.empty);
			
			Then ("there should be an answer");
			answers should not be ('empty);
			
			And ("each one should have different (x, y) values");
			answers.forall {
				answer =>
					
				allDiff (answer.map (_.copoint));
				} shouldBe (true);
			
			And ("each answer should have ascending positional values");
			answers.forall {
				answer =>
					
				increasing (answer.map (_.copoint));
				} shouldBe (true);
		}
		
		scenario ("Enforcing 'allsame', causing no satisfactory answers")
		{
			import allSameConstrained._
			
			Given ("an Equation of 'x ** 3 @== 'y * 2")
			
			val polynomial = new PolynomialEquation[Double] {
				def apply = 'x ** 3 @== 'y * 2;
				}
			val range = (1 |=> 10) map (_.toDouble) toSeq;
				
			When ("solving the Equation having an allsame requirement");
			
			val answers = withSolver {
				solver =>
					
				for {
					_ <- solver.newVars[List] (domain (solver, range)) {
						List ('x, 'y);
						}
					_ <- solver.add (polynomial)
					_ <- solver.impose (allSame)
					resultSet <- solver.run[Vector]
					} yield resultSet;
				}
			
			Then ("there should be no answer");
			answers should be ('right);
		}
	}
}
