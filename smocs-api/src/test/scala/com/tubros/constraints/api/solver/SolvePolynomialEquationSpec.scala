/**
 * Created on: Feb 4, 2013
 */
package com.tubros.constraints.api
package solver

import org.junit.runner.RunWith
import org.scalamock.scalatest.MockFactory
import org.scalatest._
import org.scalatest.junit.JUnitRunner


/**
 * The '''SolvePolynomialEquationSpec''' type uses the
 * [[com.tubros.constraints.api.solver.mock.SolvePolynomialEquationByMocking]]
 * ''mock''-based implementation of the
 * [[com.tubros.constraints.api.solver.SolvePolynomialEquationUseCase]]
 * dependencies to ensure the API operates consistently ''without'' having to
 * use/depend on `core` implemenations.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class SolvePolynomialEquationSpec
	extends UseCaseSpec
		with MockFactory
		with SolvePolynomialEquationUseCase[Option, mock.MockSolver]
		with mock.SolvePolynomialEquationByMocking
{

}