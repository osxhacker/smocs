/**
 * Created on: Feb 11, 2013
 */
package com.tubros.constraints.core.spi.solver

import Predef.{
	any2stringadd => _,
	_
	}

import scalaz._

import org.junit.runner.RunWith
import org.scalamock.scalatest.MockFactory
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api._

import problem._
import problem.ast._
import solver.error._


/**
 * The '''AlgebraicConstraintSpec''' type is a unit test which verifies
 * the [[com.tubros.constraints.core.spi.solver.AlgebraicConstraint]] `trait`
 * in a very specific, controlled, manner.  As such, the scaffolding present
 * here is typically not needed when the
 * [[com.tubros.constraints.core.spi.solver.AlgebraicConstraint]] is employed
 * in production code.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class AlgebraicConstraintSpec
	extends ProjectSpec
{
	/// Class Types
	trait PolyEquation
		extends Equation[Int]
			with ArithmeticSupport[Int]
	
	class IntConstraints (override val equation : Equation[Int])
		extends AlgebraicEquationConstraint.IntegralConstraint[Int] (equation)
	{
		def computed (in : Env[Int]) : Option[Int] =
			interpreter (in) (equation.expression).toOption;
	}
	
	
	"An AlgebraicConstraint" should "evaluate an expression with one var" in
	{
		val c = new IntConstraints (
			new PolyEquation {
				def apply = 'x * 3 + 5
				}
			);
		val vars = Map[VariableName, Int] (
			VariableName ('x) -> 5
			);
		
		c (vars) should be === (\/- (vars));
		c.computed (vars) should be === (Some (20));
	}
	
	it should "be able to evaluate an arbitrarily complex expression" in
	{
		val c = new IntConstraints (
			new PolyEquation {
				def apply = ('x * 'y + 5) / -(('z + 1) ** 3)
				}
			);
		val vars = Map[VariableName, Int] (
			VariableName ('x) -> 6,
			VariableName ('y) -> 20,
			VariableName ('z) -> 4
			);
		
		c (vars) should be === (\/- (vars));
		c.computed (vars) should be === (Some (-1));
	}
	
	it should "ignore variables not needed by the equation" in
	{
		val c = new IntConstraints (
			new PolyEquation {
				def apply = 'a ** 4;
				}
			);
		val vars = Map[VariableName, Int] (
			VariableName ('a) -> 2,
			VariableName ('b) -> 20
			);
		
		
		c (vars) should be === (\/- (vars));
		c.computed (vars) should be === (Some (16));
	}
}
