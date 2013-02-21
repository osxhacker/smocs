/**
 * Created on: Feb 19, 2013
 */
package com.tubros.constraints.api.solver

import org.junit.runner.RunWith
import org.scalamock.scalatest.MockFactory
import org.scalatest._
import org.scalatest.junit.JUnitRunner


/**
 * The '''SolvePositionalEquationSpec''' type uses the
 * [[com.tubros.constraints.api.solver.mock.SolvePositionalEquationByMocking]]
 * ''mock''-based implementation of the
 * [[com.tubros.constraints.api.solver.SolvePositionalEquationUseCase]]
 * dependencies to ensure the API operates consistently ''without'' having to
 * use/depend on `core` implementations.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class SolvePositionalEquationSpec
	extends SolvePositionalEquationUseCase[Option, mock.MockSolver[Symbol]]
		with MockFactory
		with mock.SolvePositionalEquationByMocking
		