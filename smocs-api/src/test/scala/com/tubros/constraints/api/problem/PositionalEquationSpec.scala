/**
 * Created on: Feb 18, 2013
 */
package com.tubros.constraints.api
package problem

import Predef.{
	any2stringadd => _,					// disable (x : Any) + "foo" conversions
	_
	}

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner


/**
 * The '''PositionalEquationSpec''' type defines the unit tests used to
 * certify the [[com.tubros.constraints.api.problem.PositionalSupport]] `trait`
 * as being able to express 
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class PositionalEquationSpec
	extends ProjectSpec
{
	/// Class Types
	trait PositionalEquation[T]
		extends Equation[T]
			with RelationalSupport[T]
			with PositionalSupport[T]
	
	
	"PositionalSupport" should "support expressing postfix distance" in
	{
		val equation = new PositionalEquation[Symbol] {
			def apply = 'after @== 'before + 5;
			}
	}
	
	it should "support expressing absolute postfix distance" in
	{
		val equation = new PositionalEquation[Symbol] {
			def apply = 'after @== first + 1;
			}
	}
	
	it should "support expressing relative prefix distance" in
	{
		val equation = new PositionalEquation[Symbol] {
			def apply = 'after - 5 @== 'before;
			}
	}
	
	it should "support expressing absolute prefix distance" in
	{
		val equation = new PositionalEquation[Symbol] {
			def apply = 'after @== last - 2;
			}
	}
}