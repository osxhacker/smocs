/**
 * Created on: Feb 4, 2013
 */
package com.tubros.constraints.api.solver.mock

import scala.language.higherKinds

import scalaz._

import org.scalamock.scalatest.MockFactory
import org.scalatest._

import com.tubros.constraints.api.solver._


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
		with SolvePolynomialEquationUseCase[Option, MockSolver]
		=>
			
			
	/// Class Imports
	import Scalaz._
	
	
	override val monad = Monad[Option];
	
	override val solvable = new SolverUsage {
		def withSolver[C[_]] (
			block : MockSolver => Option[Stream[C[Answer[Int]]]]
			)
			(implicit a : Applicative[C], mo : Monoid[C[Answer[Int]]])
			: Stream[C[Answer[Int]]] =
		{
			val answer = mo.zero |+|
				Answer ('x -> 2).point[C] |+|
				Answer ('y -> 2).point[C];
			
			Stream (answer);
		}
		
		def domain (solver : MockSolver, range : Range) : solver.DomainType[Int] = 
			FiniteDiscreteDomain (range);
		}
	
	override val unsolvable = new SolverUsage {
		def withSolver[C[_]] (
			block : MockSolver => Option[Stream[C[Answer[Int]]]]
			)
			(implicit a : Applicative[C], mo : Monoid[C[Answer[Int]]])
			: Stream[C[Answer[Int]]] =
		{
			Stream ();
		}
		
		def domain (solver : MockSolver, range : Range) : solver.DomainType[Int] = 
			FiniteDiscreteDomain (range);
		}
}


sealed trait MockSolver
	extends Solver[Int, Option, MockSolver]
{
	type DomainType[T] = DiscreteDomain[T]
}
