/**
 * Created on: Feb 28, 2013
 */
package com.tubros.constraints.core.internal.tree

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api.solver._


/**
 * The '''SolvePolynomialEquationSpec''' type defines a Use-Case implementation
 * of the 
 * [[com.tubros.constraints.api.solver.SolvePolynomialEquationUseCase]] by
 * using
 * [[com.tubros.constraints.core.internal.tree.TreeFiniteDomainSolver]] to
 * solve the equations given.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class SolvePolynomialEquationSpec
	extends SolvePolynomialEquationUseCase[
		TreeFiniteDomainSolver[Int]#SolverState,
		TreeFiniteDomainSolver[Int]
		]
		with TreeSolverUsage[Int]
{
	/// Class Imports
	import algebraic._
	
	
	/// Instance Properties
	override val solvable = new DefaultTreeSolverUsage;
	override val unsolvable = solvable;
}
