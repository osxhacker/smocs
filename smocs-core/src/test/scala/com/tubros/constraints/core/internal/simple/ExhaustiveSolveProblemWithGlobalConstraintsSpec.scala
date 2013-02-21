/**
 * Created on: Feb 17, 2013
 */
package com.tubros.constraints.core.internal.simple

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api.solver._


/**
 * The '''ExhaustiveSolveProblemWithGlobalConstraintsSpec''' type defines
 * a concrete Use Case implementation of the
 * [[com.tubros.constraints.api.solver.SolveProblemWithGlobalConstraintsUseCase]]
 * by using the
 * [[com.tubros.constraints.core.internal.simple.ExhaustiveFiniteDomainSolver]]
 * to solve the equations given.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class ExhaustiveSolveProblemWithGlobalConstraintsSpec
	extends SolveProblemWithGlobalConstraintsUseCase[
		ExhaustiveFiniteDomainSolver[Double]#SolverState,
		ExhaustiveFiniteDomainSolver[Double]
		]
		with ExhaustiveSolverUsage[Double]
{
	/// Class Imports
	import algebraic._
	
	
	/// Instance Properties
	override val allDiffConstrained = new DefaultExhaustiveSolverUsage;
	override val allSameConstrained = allDiffConstrained;
}