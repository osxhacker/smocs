/**
 * Created on: Mar 5, 2013
 */
package com.tubros.constraints.api.solver.mock

import scala.language.higherKinds

import scalaz._

import org.scalamock.scalatest.MockFactory
import org.scalatest._

import com.tubros.constraints.api._

import problem.ArrayNamingPolicy
import solver._
import solver.error.SolverError



/**
 * The '''SolveAllIntervalsUseCaseByMocking''' type provides the API with a
 * ''mocked'' implementation for use by the
 * [[com.tubros.constraints.api.solver.SolveAllIntervalsUseCase]].
 * By factoring this "stub" out into its own mix-in `trait`, the API can
 * define the Use Case independent of ''how'' the equations are solved.
 *
 * @author svickers
 *
 */
trait SolveAllIntervalsUseCaseByMocking
	extends SuiteMixin
{
	/// Self Type Constraints
	this : Suite
		with MockFactory
		with SolveAllIntervalsUseCase[Option, MockSolver[Int]]
		=>
			
			
	/// Class Imports
	import Scalaz._
	
	
	override val monad = Monad[Option];
	
	override val fiveElements : SolverUsage =
		new SolverUsage with ArrayNamingPolicy {
			override def withSolver[C[_]] (
				block : MockSolver[Int] => Option[SolverError \/ Stream[C[Answer[Int]]]]
				)
				(implicit a : Applicative[C], mo : Monoid[C[Answer[Int]]])
				: SolverError \/ Stream[C[Answer[Int]]] =
			{
				val items = List (
					Answer (compose ('x, 0), 3),
					Answer (compose ('x, 1), 2),
					Answer (compose ('x, 2), 0),
					Answer (compose ('x, 3), 4),
					Answer (compose ('x, 4), 1),
					
					Answer (compose ('diff, 0), 1),
					Answer (compose ('diff, 1), 2),
					Answer (compose ('diff, 2), 4),
					Answer (compose ('diff, 3), 3)
					);
				val answer = items.foldLeft (mo.zero) {
					(accum, item) =>
						
					accum |+| item.point[C];
					}
				
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
