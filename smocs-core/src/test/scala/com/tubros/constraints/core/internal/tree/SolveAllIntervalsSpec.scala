/**
 * Created on: Mar 5, 2013
 */
package com.tubros.constraints.core.internal.tree

import scala.language.postfixOps

import scalaz._

import org.junit.runner.RunWith
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.junit.JUnitRunner
import org.scalatest.time.SpanSugar._

import com.tubros.constraints.api.solver._


/**
 * The '''SolveAllIntervalsSpec''' type defines a Use-Case implementation of the 
 * [[com.tubros.constraints.api.solver.SolveAllIntervalsUseCase]] by using
 * [[com.tubros.constraints.core.internal.tree.TreeFiniteDomainSolver]] to
 * solve the equations given.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class SolveAllIntervalsSpec
	extends SolveAllIntervalsUseCase[
		TreeFiniteDomainSolver[Int]#SolverState,
		TreeFiniteDomainSolver[Int]
		]
		with TreeSolverUsage[Int]
		with TimeLimitedTests
{
	/// Class Imports
	import algebraic._
	import Scalaz._
	
	
	/// Instance Properties
	override val timeLimit = 10 seconds;
	override implicit val equal = Equal.equalA[Int];
	override val fiveElements = new DefaultTreeSolverUsage;
	override val unsolvable = fiveElements;
}
