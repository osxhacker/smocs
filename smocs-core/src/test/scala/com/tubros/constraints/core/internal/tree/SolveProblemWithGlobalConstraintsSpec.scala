/**
 * Created on: Mar 7, 2013
 */
package com.tubros.constraints.core.internal.tree

import scalaz._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api.solver._


/**
 * The '''SolveProblemWithGlobalConstraintsSpec''' type defines a Use-Case
 * implementation of the 
 * [[com.tubros.constraints.api.solver.SolvePositionalEquationUseCase]] by
 * using
 * [[com.tubros.constraints.core.internal.tree.TreeFiniteDomainSolver]] to
 * solve the equations given.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class SolveProblemWithGlobalConstraintsSpec
	extends SolveProblemWithGlobalConstraintsUseCase[
		TreeFiniteDomainSolver[Double]#SolverState,
		TreeFiniteDomainSolver[Double]
		]
		with TreeSolverUsage[Double]
{
	/// Class Imports
	import algebraic._
	import Scalaz._
	
	
	/// Instance Properties
	override implicit val equal = Equal.equalA[Double];
	override val allDiffConstrained = new DefaultTreeSolverUsage;
	override val allSameConstrained = allDiffConstrained;
}
