/**
 * Created on: Jan 27, 2013
 */
package com.tubros.constraints.api
package problem

import scala.Predef.{
	any2stringadd => _,
	_
    }

import scalaz._

import org.junit.runner.RunWith
import org.scalamock.scalatest.MockFactory
import org.scalatest._
import org.scalatest.junit.JUnitRunner


/**
 * The '''EquationSpec''' type provides unit-tests for the
 * [[com.tubros.constraints.api.problem.Equation]] `problem` abstraction.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class EquationSpec
	extends ProjectSpec
{
	/// Class Imports
	import ast._
	import EquationSpec.RelationalEquation
	
	
	"An Equation" should "support a constant assignment" in
	{
		val const = new Equation[Int] with DerivedValueSupport[Int] {
			def apply = 'a := 5;
			};
		
		const.arity shouldBe (1);
		const.derived shouldBe (Some (FastSymbol ("a")));
		const.variables should be ('empty);
	}
	
	it should "not require an assigment operator" in
	{
		val greaterThanZero = new RelationalEquation {
			def apply = 'x > 0;
			};
			
		greaterThanZero.arity shouldBe (1);
		greaterThanZero () shouldBe (
			GreaterThan (
				VariableUse ('x),
				Constant (0)
				)
			);
	}
	
	it should "support compound statements" in
	{
		val complex = new RelationalEquation {
            def apply = ('x < 0 || 'x > 0) && ('x <> 99);
			};
			
		complex.arity shouldBe (1);
		complex () shouldBe (
			LogicalAnd (
				LogicalOr (
					LessThan (
						VariableUse ('x),
						Constant (0)
						),
					GreaterThan (
						VariableUse ('x),
						Constant (0)
						)
					),
				NotEqualTo (
					VariableUse ('x),
					Constant (99)
					)
				)
			);
	}
	
	it should "allow definitions consisting only of symbolic variables" in
	{
		val xBeforeY = new RelationalEquation {
			def apply = 'x < 'y;
			}
		
		xBeforeY.arity shouldBe (2);
	}
	
	it should "support array syntax" in
	{
		/// Due to Scalatest having a conversion for Symbols, the definition of
		/// the Equation must be outside of the test case.
		val equation = EquationSpec.usingSubscripts;
		val nameComposer = new ArrayNamingPolicy {};
		
		equation.arity shouldBe (2);
		equation.variables shouldBe (
			Set (
				nameComposer.compose ('array, 0),
				nameComposer.compose ('array, 1)
				)
			);
	}
	
	it should "be able to show its symbolic definition" in
	{
		import syntax.show._
		
		
		val equation = new RelationalEquation with ArithmeticSupport[Int] {
			def apply = 'x ** 3 - 'x ** 2 + lit (4) * 'x;
            }
		
		equation.show shouldNot be ('empty);
		equation.shows shouldBe (
			"Equation(((('x) ** (3)) - (('x) ** (2))) + ((4) * ('x)))"
			);
	}
}

object EquationSpec
{
	/// Class Types
	trait RelationalEquation
		extends Equation[Int]
			with PropositionalSupport[Int]
			with RelationalSupport[Int]
	
	val usingSubscripts = new RelationalEquation {
		def apply = 'array (1) < 'array (0);
		}
}
