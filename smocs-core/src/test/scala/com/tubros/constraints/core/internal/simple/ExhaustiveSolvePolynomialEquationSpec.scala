/**
 * Created on: Feb 4, 2013
 */
package com.tubros.constraints.core.internal.simple

import scala.language.higherKinds

import scalaz._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api.UseCaseSpec
import com.tubros.constraints.api.solver._


/**
 * The '''ExhaustiveSolvePolynomialEquationSpec''' type defines a concrete
 * Use Case implementation of the
 * [[com.tubros.constraints.api.solver.SolvePolynomialEquationUseCase]] by
 * using
 * [[com.tubros.constraints.core.internal.simple.ExhaustiveFiniteDomain]] to
 * solve the equations given.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class ExhaustiveSolvePolynomialEquationSpec
	extends SolvePolynomialEquationUseCase[
		ExhaustiveFiniteDomainSolver[Int]#SolverState,
		ExhaustiveFiniteDomainSolver[Int]
		]
{
	/// Class Imports
	import algebraic._
	
	
	/// Class Types
	type MonadType[T] = SolverType#SolverState[T]
	type SolverType = ExhaustiveFiniteDomainSolver[Int]
	
	
	/// Test Collaborators
	private val solver = new ExhaustiveFiniteDomainSolver[Int];
	
	override val monad = Monad[ExhaustiveFiniteDomainSolver[Int]#SolverState];
	
	override val solvable = new SolverUsage {
		override def withSolver[C[_]] (
			block : SolverType => MonadType[Stream[C[Answer[Int]]]]
			)
			(implicit a : Applicative[C], mo : Monoid[C[Answer[Int]]])
			: Stream[C[Answer[Int]]] =
		{
			return (solver (block));
		}
		
		override def domain (solver : SolverType, range : Range)
			: solver.DomainType[Int] =
			FiniteDiscreteDomain (range);
		}
	
	override val unsolvable = solvable;
}
