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
 * The '''AlgebraicConstraintSpec''' type
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
	
	abstract class TestConstraints[T : Numeric] (val equation : Equation[T])
		extends Constraint[T]
			with Interpreted[T]
	{
		override val variables = equation.variables;
		
		
		override def apply (in : Env[T]) : SolverError \/ Env[T] =
			interpreter (in) (equation.expression) match {
				case \/- (_) => \/- (in);
				case -\/ (_) => -\/ (new SolverError { val message = "failed" });
				}
		
		
		override protected def interpreter
			: Env[T] => PartialFunction[Expression[T], Result[T]] =
			env => numericOps (env);
			
			
		def computed (in : Env[T]) : Option[T] =
			interpreter (in) (equation.expression).toOption;
		
		
		protected def numericOps (env : Env[T])
			: PartialFunction[Expression[T], Result[T]];
	}
	
	class IntConstraints (override val equation : Equation[Int])
		(implicit override val numeric : Numeric[Int])
		extends TestConstraints[Int] (equation)
			with AlgebraicConstraint[Int]
	{
		protected def numericOps (env : Env[Int])
			: PartialFunction[Expression[Int], Result[Int]] =
			_ match {
				case Quotient (n, d) =>
					for {
						num <- interpreter (env) (n)
						denom <- interpreter (env) (d)
						} yield num / denom;
				}
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
}
