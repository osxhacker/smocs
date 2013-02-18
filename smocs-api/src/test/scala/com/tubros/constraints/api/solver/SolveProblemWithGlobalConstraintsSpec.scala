/**
 * Created on: Feb 17, 2013
 */
package com.tubros.constraints.api.solver

import org.junit.runner.RunWith
import org.scalamock.scalatest.MockFactory
import org.scalatest.junit.JUnitRunner


/**
 * The '''SolveProblemWithGlobalConstraintsSpec''' type uses
 * [[com.tubros.constraints.api.solver.mock.SolveProblemWithGlobalConstraintsByMocking]]
 * to provide the collaborators needed by
 * [[com.tubros.constraints.api.solver.SolveProblemWithGlobalConstraintsUseCase]].
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class SolveProblemWithGlobalConstraintsSpec
	extends SolveProblemWithGlobalConstraintsUseCase[Option, mock.MockSolver[Double]]
		with MockFactory
		with mock.SolveProblemWithGlobalConstraintsByMocking
		