/**
 * Created on: Jan 27, 2013
 */
package com.tubros.constraints.api.problem

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api.{
	FastSymbol,
	UseCaseSpec
	}


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
	/// Class Types
	trait MinimalEquation[T]
		extends Equation[T]
			with DerivedValueSupport[T]
	
	
	feature ("Defining smocs equations")
	{
		scenario ("Defining a constant Equation")
		{
			Given ("An Equation specifying a constant value");
			
			val e = new MinimalEquation[Int] {
				def apply = 'a := 5;
				}
			
			Then ("The definition should be what was given");
			
			e.arity shouldBe (1);
			e.derived shouldBe (Some (FastSymbol ("a")));
			e.variables should be ('empty);
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
			
			quadratic.arity shouldBe (2);
			quadratic.variables shouldBe (
				Set (FastSymbol ("x"), FastSymbol ("y"))
				);
		}
	}
}