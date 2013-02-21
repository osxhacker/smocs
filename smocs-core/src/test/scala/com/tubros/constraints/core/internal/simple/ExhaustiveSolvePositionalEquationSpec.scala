/**
 * Created on: Feb 20, 2013
 */
package com.tubros.constraints.core.internal.simple

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api.problem._
import com.tubros.constraints.api.solver._
import com.tubros.constraints.core.spi.solver._


/**
 * The '''ExhaustiveSolvePositionalEquationSpec''' type
 * a concrete Use Case implementation of the
 * [[com.tubros.constraints.api.solver.SolvePositionalEquationUseCase]]
 * by using the
 * [[com.tubros.constraints.core.internal.simple.ExhaustiveFiniteDomainSolver]]
 * to solve the equations given.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class ExhaustiveSolvePositionalEquationSpec
	extends SolvePositionalEquationUseCase[
		ExhaustiveFiniteDomainSolver[Symbol]#SolverState,
		ExhaustiveFiniteDomainSolver[Symbol]
		]
		with ExhaustiveSolverUsage[Symbol]
{
	/// Class Imports
	import positional._
	
	
	/// Instance Properties
	override val solvable = new DefaultExhaustiveSolverUsage;
	override val unsolvable = solvable;
}
