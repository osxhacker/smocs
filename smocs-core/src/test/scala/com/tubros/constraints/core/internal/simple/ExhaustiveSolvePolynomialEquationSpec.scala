/**
 * Created on: Feb 4, 2013
 */
package com.tubros.constraints.core.internal.simple

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api.solver._


/**
 * The '''ExhaustiveSolvePolynomialEquationSpec''' type defines a concrete
 * Use Case implementation of the
 * [[com.tubros.constraints.api.solver.SolvePolynomialEquationUseCase]] by
 * using
 * [[com.tubros.constraints.core.internal.simple.ExhaustiveFiniteDomainSolver]]
 * to solve the equations given.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class ExhaustiveSolvePolynomialEquationSpec
	extends SolvePolynomialEquationUseCase[
		ExhaustiveFiniteDomainSolver[Int]#SolverState,
		ExhaustiveFiniteDomainSolver[Int]
		]
		with ExhaustiveSolverUsage[Int]
{
	/// Class Imports
	import algebraic._
	
	
	/// Instance Properties
	override val solvable = new DefaultExhaustiveSolverUsage;
	override val unsolvable = solvable;
}
