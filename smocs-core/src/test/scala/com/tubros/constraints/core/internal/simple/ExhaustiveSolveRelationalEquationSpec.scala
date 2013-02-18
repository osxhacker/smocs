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
		with ExhaustiveSolverUsage[Symbol]
{
	/// Class Imports
	import relational._
	
	
	/// Test Collaborators
	override val solvable = new DefaultExhaustiveSolverUsage;
	override val unsolvable = solvable;
}
