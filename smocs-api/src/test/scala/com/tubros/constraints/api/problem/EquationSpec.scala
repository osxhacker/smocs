/**
 * Created on: Jan 27, 2013
 */
package com.tubros.constraints.api
package problem

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
	/// Class Types
	trait MathEquation
		extends Equation
			with RelationalSupport


	"An Equation" should "support a constant assignment" in
	{
		val const = new Equation {
			def apply = 'a := 5;
			};
		
		const.arity should be === (1);
		const.variables should be === (Set ('a));
	}
	
	it should "not require an assigment operator" in
	{
		val greaterThanZero = new MathEquation {
			def apply = 'x > 0;
			};
	}
	
	it should "support compound statements" in
	{
		val notZero = new MathEquation {
			def apply = 'x < 0 || 'x > 0;
			};
			
		notZero.arity should be === (1);
	}
}
