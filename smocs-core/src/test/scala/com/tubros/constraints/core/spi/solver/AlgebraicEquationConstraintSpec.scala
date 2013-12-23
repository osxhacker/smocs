/**
 * Created on: Feb 8, 2013
 */
package com.tubros.constraints.core
package spi.solver

import scalaz._

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner
import com.tubros.constraints.api._
import com.tubros.constraints.api.problem._
import com.tubros.constraints.api.solver._
import spi.solver._
import com.tubros.constraints.api.solver.error.UnsolvableError


/**
 * The '''AlgebraicEquationConstraintSpec''' type provides unit testing for
 * the
 * [[com.tubros.constraints.core.internal.simple.AlgebraicEquationConstraint]]
 * core [[com.tubros.constraints.core.spi.solver.Constraint]] type.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class AlgebraicEquationConstraintSpec
	extends ProjectSpec
{
	/// Class Types
	trait ArithEquation[T]
		extends Equation[T]
			with PropositionalSupport[T]
			with ArithmeticSupport[T]
			with RelationalSupport[T]
			
			
	"An AlgebraicEquationConstraint" should "be constructable" in
	{
		val cc = new FractionalAlgebraicEquationConstraint[Double];
	}
	
	it should "be able to create a Constraint" in
	{
		val cc = new IntegralAlgebraicEquationConstraint[Long];
		val equation = new ArithEquation[Long] {
			def apply = 'x * 2 < 5
			}
		
		val constraint = cc.constrains (equation);
		
		constraint shouldNot be (null);
	}
	
	it should "constrain the domain of Variables given" in
	{
		val cc = new IntegralAlgebraicEquationConstraint[Int];
		val equation = new ArithEquation[Int] {
			def apply = 'x * 2 < 5
			}
		val input = Map[VariableName, Int] (VariableName ('x) -> 3);
		
		val answer = cc.constrains (equation) (input);
		
		answer shouldNot be (null);
		answer should be ('left);
		answer shouldBe (-\/ (UnsolvableError));
	}
	
	it should "detect when variables are missing" in
	{
		val cc = new IntegralAlgebraicEquationConstraint[Int];
		val equation = new ArithEquation[Int] {
			def apply = 'x * 2 < 5
			}
		val input = Map[VariableName, Int] (VariableName ('a) -> 0);
		
		val answer = cc.constrains (equation) (input);
		
		answer shouldNot be (null);
		answer should be ('left);
	}
	
	it should "ignore variables not needed in the equation" in
	{
		val cc = new IntegralAlgebraicEquationConstraint[Int];
		val equation = new ArithEquation[Int] {
			def apply = 'x * 2 < 5
			}
		val input = Map[VariableName, Int] (
			VariableName ('a) -> 0,
			VariableName ('x) -> 42
			);
		
		val answer = cc.constrains (equation) (input);
		
		answer shouldNot be (null);
		answer should be ('left);
	}
}
