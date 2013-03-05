/**
 * Created on: Jan 27, 2013
 */
package com.tubros.constraints.api
package problem

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
		val const = new Equation[Int] {
			def apply = 'a := 5;
			};
		
		const.arity should be === (1);
		const.variables should be === (Set ('a));
	}
	
	it should "not require an assigment operator" in
	{
		val greaterThanZero = new RelationalEquation {
			def apply = 'x > 0;
			};
			
		greaterThanZero.arity should be === (1);
		greaterThanZero () should be === (
			GreaterThan (
				VariableUse ('x),
				Constant (0)
				)
			);
	}
	
	it should "support compound statements" in
	{
		val complex = new RelationalEquation {
			def apply = ('x < 0 || 'x > 0) && ('x !== 99);
			};
			
		complex.arity should be === (1);
		complex () should be === (
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
		
		xBeforeY.arity should be === (2);
	}
	
	it should "support array syntax" in
	{
		/// Due to Scalatest having a conversion for Symbols, the definition of
		/// the Equation must be outside of the test case.
		val equation = EquationSpec.usingSubscripts;
		val nameComposer = new ArrayNamingPolicy {};
		
		equation.arity should be === (2);
		equation.variables should be === (
			Set (
				nameComposer.compose ('array, 0),
				nameComposer.compose ('array, 1)
				)
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
