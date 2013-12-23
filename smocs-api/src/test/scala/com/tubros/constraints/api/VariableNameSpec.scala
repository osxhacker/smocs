/**
 * Created on: Jan 25, 2013
 */
package com.tubros.constraints.api

import org.junit.runner.RunWith
import org.scalacheck._
import org.scalatest.junit.JUnitRunner


/**
 * The '''VariableNameSpec''' type exercises the
 * [[com.tubros.constraints.api.VariableName]] abstraction.  Since this is a
 * fundamental (and simple) type, there's not a bunch to test.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class VariableNameSpec
	extends ProjectSpec
{
	/// Class Imports
	val nameGenerator = Gen.alphaStr;
	
	
	"A VariableName" should "be creatable with only a Symbol" in
	{
		val name = VariableName ('x);
		
		name shouldNot be (null);
	}
	
	it should "retain the symbolic name it was given" in
	{
		val name = VariableName ('a);
		
		name.name shouldBe ("a");
	}
	
	it should "accept any valid Symbol" in
	{
		check (
			Prop.forAll (nameGenerator) {
				candidate =>
					
				VariableName (Symbol (candidate)).name == candidate;
				}
			);
	}
	
	it should "be able to promote a Symbol" in
	{
		val converted : VariableName = 'anId;
		
		converted should be (FastSymbol ("anId"));
	}
}
