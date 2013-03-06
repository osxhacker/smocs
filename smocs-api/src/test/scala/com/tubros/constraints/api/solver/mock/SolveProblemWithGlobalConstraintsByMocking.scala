/**
 * Created on: Feb 17, 2013
 */
package com.tubros.constraints.api.solver.mock

import scala.language.higherKinds

import scalaz._

import org.scalamock.scalatest.MockFactory
import org.scalatest._

import com.tubros.constraints.api.solver._

import error.SolverError


/**
 * The '''SolveProblemWithGlobalConstraintsByMocking''' type fulfills the
 * [[com.tubros.constraints.api.solver.SolveProblemWithGlobalConstraintsUseCase]]
 * Use Case dependencies by employing mocking of the solver interaction.
 *
 * @author svickers
 *
 */
trait SolveProblemWithGlobalConstraintsByMocking
	extends SuiteMixin
{
	/// Self Type Constraints
	this : Suite
		with MockFactory
		with SolveProblemWithGlobalConstraintsUseCase[
			Option,
			MockSolver[Double]
			]
		=>
			
			
	/// Class Imports
	import Scalaz._
	
	
	override val monad = Monad[Option];
	
	override val allDiffConstrained = new SolverUsage {
		override def withSolver[C[_]] (
			block : MockSolver[Double] => Option[SolverError \/ Stream[C[Answer[Double]]]]
			)
			(implicit a : Applicative[C], mo : Monoid[C[Answer[Double]]])
			: SolverError \/ Stream[C[Answer[Double]]] =
		{
			val answer = mo.zero |+|
				Answer ('x -> 2.0).point[C] |+|
				Answer ('y -> 4.0).point[C];
			
			\/- (Stream (answer));
		}
		
		override def domain (solver : MockSolver[Double], range : Seq[Double])
			: solver.DomainType[Double] = 
			FiniteDiscreteDomain (range);
		}
	
	override val allSameConstrained = new SolverUsage {
		override def withSolver[C[_]] (
			block : MockSolver[Double] => Option[SolverError \/ Stream[C[Answer[Double]]]]
			)
			(implicit a : Applicative[C], mo : Monoid[C[Answer[Double]]])
			: SolverError \/ Stream[C[Answer[Double]]] =
		{
			\/- (Stream.empty);
		}
		
		override def domain (solver : MockSolver[Double], range : Seq[Double])
			: solver.DomainType[Double] = 
			FiniteDiscreteDomain (range);
		}
	
}