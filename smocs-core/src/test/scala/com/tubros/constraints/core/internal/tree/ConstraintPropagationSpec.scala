/**
 * Created on: Feb 28, 2013
 */
package com.tubros.constraints.core.internal.tree

import org.junit.runner.RunWith
import org.scalamock.scalatest.MockFactory
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api._
import com.tubros.constraints.core.spi.solver._


/**
 * The '''ConstraintPropagationSpec''' type defines unit tests to ensure that
 * the [[com.tubros.constraints.core.internal.tree.ConstraintPropagation]]
 * `class` properly filters, or ''propagates'', a
 * [[com.tubros.constraints.api.solver.Domain]] by way of one or more
 * [[com.tubros.constraints.core.spi.solver.Constraint]]s.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class ConstraintPropagationSpec
	extends ProjectSpec
{
	// TODO: add coverage with mocked Constraint instances
}
