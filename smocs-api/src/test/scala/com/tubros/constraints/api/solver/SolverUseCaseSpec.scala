/**
 * Created on: Feb 8, 2013
 */
package com.tubros.constraints.api.solver

import scala.language.higherKinds

import scalaz._

import com.tubros.constraints.api.UseCaseSpec


/**
 * The '''SolverUseCaseSpec''' type defines `solver` specific Use Case
 * functionality for the Smocs API bundle.
 * 
 * @author svickers
 *
 */
trait SolverUseCaseSpec[M[+_], SolverT <: Solver[Int, M, SolverT]]
	extends UseCaseSpec
{
	/// Class Types
	trait SolverUsage
	{
		def withSolver[C[_]] (block : SolverT => M[Stream[C[Answer[Int]]]])
			(implicit a : Applicative[C], mo : Monoid[C[Answer[Int]]])
			: Stream[C[Answer[Int]]];
		
		def domain (solver : SolverT, range : Range) : solver.DomainType[Int];
		
	}
}
