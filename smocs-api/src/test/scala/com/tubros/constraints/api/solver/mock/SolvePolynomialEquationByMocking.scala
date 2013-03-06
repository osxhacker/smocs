/**
 * Created on: Feb 4, 2013
 */
package com.tubros.constraints.api.solver.mock

import scala.language.higherKinds

import scalaz._

import org.scalamock.scalatest.MockFactory
import org.scalatest._

import com.tubros.constraints.api.solver._

import error.SolverError


/**
 * The '''SolvePolynomialEquationByMocking''' type provides the API with a
 * ''mocked'' implementation for use by the
 * [[com.tubros.constraints.api.solver.SolvePolynomialEquationUseCase]].
 * By factoring this "stub" out into its own mix-in `trait`, the API can
 * define the Use Case independent of ''how'' the equations are solved.
 *
 * @author svickers
 *
 */
trait SolvePolynomialEquationByMocking
	extends SuiteMixin
{
	/// Self Type Constraints
	this : Suite
		with MockFactory
		with SolvePolynomialEquationUseCase[Option, MockSolver[Int]]
		=>
			
			
	/// Class Imports
	import Scalaz._
	
	
	override val monad = Monad[Option];
	
	override val solvable = new SolverUsage {
		override def withSolver[C[_]] (
			block : MockSolver[Int] => Option[SolverError \/ Stream[C[Answer[Int]]]]
			)
			(implicit a : Applicative[C], mo : Monoid[C[Answer[Int]]])
			: SolverError \/ Stream[C[Answer[Int]]] =
		{
			val answer = mo.zero |+|
				Answer ('x -> 2).point[C] |+|
				Answer ('y -> 2).point[C];
			
			\/- (Stream (answer));
		}
		
		override def domain (solver : MockSolver[Int], range : Seq[Int])
			: solver.DomainType[Int] = 
			FiniteDiscreteDomain (range);
		}
	
	override val unsolvable = new SolverUsage {
		override def withSolver[C[_]] (
			block : MockSolver[Int] => Option[SolverError \/ Stream[C[Answer[Int]]]]
			)
			(implicit a : Applicative[C], mo : Monoid[C[Answer[Int]]])
			: SolverError \/ Stream[C[Answer[Int]]] =
		{
			\/- (Stream.empty);
		}
		
		override def domain (solver : MockSolver[Int], range : Seq[Int])
			: solver.DomainType[Int] = 
			FiniteDiscreteDomain (range);
		}
}


sealed trait MockSolver[A]
	extends Solver[A, Option, MockSolver[A]]
{
	type DomainType[T] = DiscreteDomain[T]
}
