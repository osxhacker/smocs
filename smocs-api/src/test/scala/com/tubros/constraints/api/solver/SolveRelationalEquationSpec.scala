/**
 * Created on: Feb 13, 2013
 */
package com.tubros.constraints.api
package solver

import org.junit.runner.RunWith
import org.scalamock.scalatest.MockFactory
import org.scalatest._
import org.scalatest.junit.JUnitRunner


/**
 * The '''SolveRelationalEquationSpec''' type * uses the
 * [[com.tubros.constraints.api.solver.mock.SolveRelationalEquationByMocking]]
 * ''mock''-based implementation of the
 * [[com.tubros.constraints.api.solver.SolveRelationalEquationUseCase]]
 * dependencies to ensure the API operates consistently ''without'' having to
 * use/depend on `core` implementations.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class SolveRelationalEquationSpec
	extends UseCaseSpec
		with MockFactory
		with SolveRelationalEquationUseCase[Option, mock.MockSolver[Symbol]]
		with mock.SolveRelationalEquationByMocking
