/**
 * Created on: Feb 17, 2013
 */
package com.tubros.constraints.core.internal.tree

import scala.language.higherKinds

import scalaz._

import com.tubros.constraints.api.problem._
import com.tubros.constraints.api.solver._
import com.tubros.constraints.core.spi.solver._


/**
 * The '''TreeSolverUsage''' type factors the common definition of
 * [[com.tubros.constraints.api.solver.SolverUsage]] into one place, to
 * reduce code duplication.
 *
 * @author svickers
 *
 */
trait TreeSolverUsage[A]
{
	/// Self Type Constraint
	this : SolverUseCaseSpec[
		A,
		TreeFiniteDomainSolver[A]#SolverState,
		TreeFiniteDomainSolver[A]
		]
		=>
			
			
	/// Class Types
	type MonadType[T] = SolverType#SolverState[T]
	type SolverType = TreeFiniteDomainSolver[A]
	
	class DefaultTreeSolverUsage (implicit cc : CanConstrain[Equation, A])
		extends SolverUsage
	{
		private val solver = new TreeFiniteDomainSolver[A];
		
		override def withSolver[C[_]] (
			block : SolverType => MonadType[Stream[C[Answer[A]]]]
			)
			(implicit a : Applicative[C], mo : Monoid[C[Answer[A]]])
			: Stream[C[Answer[A]]] =
		{
			return (solver (block));
		}
		
		override def domain (solver : SolverType, range : Seq[A])
			: solver.DomainType[A] =
			FiniteDiscreteDomain (range);
		}
			
			
	/// Instance Properties
	implicit val monad = Monad[TreeFiniteDomainSolver[A]#SolverState];
}
