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
			val e = new Equation {
				def apply = 'a := 5;
				}
			
			e.arity should be === (1);
			e.variables should be === (Set ('a));
		}
		
		/*
		scenario ("Using a more complex Equation")
		{
			import Equation._
			
			 val quadratic = 'y := 'x * 'x;
		}
		* 
		*/
	}
}