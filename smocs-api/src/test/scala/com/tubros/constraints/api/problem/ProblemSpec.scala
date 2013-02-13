/**
 * Created on: Jan 28, 2013
 */
package com.tubros.constraints.api
package problem

import scalaz._

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner


/**
 * The '''ProblemSpec''' type verifies
 * [[com.tubros.constraints.api.problem.Problem]] to ensure it provides the
 * behaviour needed by the primary CSP problem abstraction.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class ProblemSpec
	extends ProjectSpec
{
	"A Problem" should "require at least one Equation" in
	{
		val problem = Problem (
			new Equation[Int] {
				def apply = 'x := 99;
				}
			);
		
		problem.equations.size must be === (1);
	}
}

