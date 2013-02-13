/**
 * Created on: Jan 27, 2013
 */
package com.tubros.constraints.api.problem

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api.UseCaseSpec


/**
 * The '''DefiningEquationsUseCase''' type is a
 * [[com.tubros.constraints.api.UseCaseSpec]] serving to illustrate expected
 * [[com.tubros.constraints.api.Equation]] definition.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class DefiningEquationsUseCase
	extends UseCaseSpec
{
	feature ("Defining smocs equations")
	{
		scenario ("Defining a constant Equation")
		{
			Given ("An Equation specifying a constant value");
			
			val e = new Equation[Int] {
				def apply = 'a := 5;
				}
			
			Then ("The definition should be what was given");
			
			e.arity should be === (1);
			e.variables should be === (Set ('a));
		}
		
		scenario ("Using a more complex Equation")
		{
			Given ("A quadratic Equation");
			
			val quadratic = new Equation[Int]
				with ArithmeticSupport[Int]
				with RelationalSupport[Int] {
					def apply = 'y @== 'x ** 2;
					}
			 
			Then ("The definition should be what was given");
			
			quadratic.arity should be === (2);
			quadratic.variables should be === (Set ('x, 'y));
		}
	}
}