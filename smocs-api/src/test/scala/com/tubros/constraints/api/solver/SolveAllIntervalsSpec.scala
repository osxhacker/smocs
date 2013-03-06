/**
 * Created on: Mar 5, 2013
 */
package com.tubros.constraints.api
package solver

import org.junit.runner.RunWith
import org.scalamock.scalatest.MockFactory
import org.scalatest._
import org.scalatest.junit.JUnitRunner


/**
 * The '''SolveAllIntervalsSpec''' type uses the
 * [[com.tubros.constraints.api.solver.mock.SolveAllIntervalsUseCaseByMocking]]
 * ''mock''-based implementation of the
 * [[com.tubros.constraints.api.solver.SolvePolynomialEquationUseCase]]
 * dependencies to ensure the API operates consistently ''without'' having to
 * use/depend on `core` implementations.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class SolveAllIntervalsSpec
	extends UseCaseSpec
		with MockFactory
		with SolveAllIntervalsUseCase[Option, mock.MockSolver[Int]]
		with mock.SolveAllIntervalsUseCaseByMocking
