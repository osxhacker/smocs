/**
 * Created on: Mar 7, 2013
 */
package com.tubros.constraints.core.internal.tree

import scalaz._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api.solver._


/**
 * The '''SolvePositionalEquationSpec''' type defines a Use-Case implementation
 * of the 
 * [[com.tubros.constraints.api.solver.SolvePositionalEquationUseCase]] by
 * using
 * [[com.tubros.constraints.core.internal.tree.TreeFiniteDomainSolver]] to
 * solve the equations given.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class SolvePositionalEquationSpec
	extends SolvePositionalEquationUseCase[
		TreeFiniteDomainSolver[Symbol]#SolverState,
		TreeFiniteDomainSolver[Symbol]
		]
		with TreeSolverUsage[Symbol]
{
	/// Class Imports
	import positional._
	import Scalaz._
	
	
	/// Instance Properties
	override implicit val equal = Equal.equalA[Symbol];
	override val solvable = new DefaultTreeSolverUsage;
	override val unsolvable = solvable;
}