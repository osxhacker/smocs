/**
 * Created on: Feb 19, 2013
 */
package com.tubros.constraints.api.solver.mock

import scala.language.higherKinds

import scalaz._

import org.scalamock.scalatest.MockFactory
import org.scalatest._

import com.tubros.constraints.api.solver._

import error.SolverError


/**
 * The '''SolvePositionalEquationByMocking''' type provides the API with a
 * ''mocked'' implementation for use by the
 * [[com.tubros.constraints.api.solver.SolvePositionallEquationUseCase]].
 * By factoring this "stub" out into its own mix-in `trait`, the API can
 * define the Use Case independent of ''how'' the equations are solved.
 *
 * @author svickers
 *
 */
trait SolvePositionalEquationByMocking
	extends SuiteMixin
{
	/// Self Type Constraints
	this : Suite
		with MockFactory
		with SolvePositionalEquationUseCase[Option, MockSolver[Symbol]]
		=>
			
			
	/// Class Imports
	import Scalaz._
	
	
	override val monad = Monad[Option];
	
	override val solvable = new SolverUsage {
		override def withSolver[C[_]] (
			block : MockSolver[Symbol] => Option[Stream[C[Answer[Symbol]]]]
			)
			(implicit a : Applicative[C], mo : Monoid[C[Answer[Symbol]]])
			: SolverError\/ Stream[C[Answer[Symbol]]] =
		{
			val answer = mo.zero |+|
				Answer ('a -> 'v3).point[C] |+|
				Answer ('b -> 'v2).point[C] |+|
				Answer ('c -> 'v1).point[C];
			
			\/- (Stream (answer));
		}
		
		override def domain (solver : MockSolver[Symbol], range : Seq[Symbol])
			: solver.DomainType[Symbol] = 
			FiniteDiscreteDomain (range);
		}
	
	override val unsolvable = new SolverUsage {
		override def withSolver[C[_]] (
			block : MockSolver[Symbol] => Option[Stream[C[Answer[Symbol]]]]
			)
			(implicit a : Applicative[C], mo : Monoid[C[Answer[Symbol]]])
			: SolverError\/ Stream[C[Answer[Symbol]]] =
		{
			\/- (Stream.empty);
		}
		
		override def domain (solver : MockSolver[Symbol], range : Seq[Symbol])
			: solver.DomainType[Symbol] = 
			FiniteDiscreteDomain (range);
		}
}
