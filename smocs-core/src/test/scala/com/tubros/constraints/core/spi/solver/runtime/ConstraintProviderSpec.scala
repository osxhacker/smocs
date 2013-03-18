/**
 * Created on: Mar 12, 2013
 */
package com.tubros.constraints.core.spi.solver
package runtime

import org.junit.runner.RunWith
import org.scalamock.scalatest.MockFactory
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api.ProjectSpec


/**
 * The '''ConstraintProviderSpec''' type defines unit tests which certify the
 * [[com.tubros.constraints.core.spi.solver.runtime.ConstraintProvider]]
 * concept for use as a source of
 * [[com.tubros.constraints.core.spi.solver.Constraint]]s.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class ConstraintProviderSpec
	extends ProjectSpec
		with MockFactory
		with ConstraintTestingSupport
{
	/// Testing Collaborators
	lazy val mockProvider = mock[ConstraintProvider[Int]];
	val noConstraints = Set.empty[Constraint[Int]];
	
	
	"A ConstraintProvider" should "be able to provide all known constraints" in
	{
		(mockProvider.constraints _).expects ().returning (noConstraints);
		
		mockProvider.constraints must not be === (null);
	}
}
