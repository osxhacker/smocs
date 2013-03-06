/**
 * Created on: Feb 8, 2013
 */
package com.tubros.constraints.api.solver

import scala.language.higherKinds

import scalaz._

import com.tubros.constraints.api.UseCaseSpec
import com.tubros.constraints.api.problem._

import error.SolverError


/**
 * The '''SolverUseCaseSpec''' type defines `solver` specific Use Case
 * functionality for the Smocs API bundle.
 * 
 * @author svickers
 *
 */
trait SolverUseCaseSpec[A, M[+_], SolverT <: Solver[A, M, SolverT]]
	extends UseCaseSpec
{
	/// Class Types
	trait SolverUsage
	{
		def withSolver[C[_]] (
			block : SolverT => M[SolverError \/ Stream[C[Answer[A]]]]
			)
			(implicit a : Applicative[C], mo : Monoid[C[Answer[A]]])
			: SolverError \/ Stream[C[Answer[A]]];
		
		def domain (solver : SolverT, range : Seq[A]) : solver.DomainType[A];
	}
}


object SolverUseCaseSpec
{
	/// Class Types
	trait PolynomialEquation[A]
		extends Equation[A]
			with ArithmeticSupport[A]
			with RelationalSupport[A]
}
