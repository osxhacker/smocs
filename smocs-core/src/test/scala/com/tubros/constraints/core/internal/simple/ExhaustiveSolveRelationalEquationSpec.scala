/**
 * Created on: Feb 13, 2013
 */
package com.tubros.constraints.core.internal.simple

import scala.language.higherKinds

import scalaz._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api.UseCaseSpec
import com.tubros.constraints.api.solver._


/**
 * The '''ExhaustiveSolveRelationalEquationSpec''' type defines a concrete
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
class ExhaustiveSolveRelationalEquationSpec
	extends SolveRelationalEquationUseCase[
		ExhaustiveFiniteDomainSolver[Symbol]#SolverState,
		ExhaustiveFiniteDomainSolver[Symbol]
		]
{
	/// Class Imports
	import relational._
	
	
	/// Class Types
	type MonadType[T] = SolverType#SolverState[T]
	type SolverType = ExhaustiveFiniteDomainSolver[Symbol]
	
	
	/// Test Collaborators
	override val monad = Monad[ExhaustiveFiniteDomainSolver[Symbol]#SolverState];
	private val solver = new ExhaustiveFiniteDomainSolver[Symbol];
	
	override val solvable = new SolverUsage {
		override def withSolver[C[_]] (
			block : SolverType => MonadType[Stream[C[Answer[Symbol]]]]
			)
			(implicit a : Applicative[C], mo : Monoid[C[Answer[Symbol]]])
			: Stream[C[Answer[Symbol]]] =
		{
			return (solver (block));
		}
		
		override def domain (solver : SolverType, range : Seq[Symbol])
			: solver.DomainType[Symbol] =
			FiniteDiscreteDomain (range);
		}
	
	override val unsolvable = solvable;

}