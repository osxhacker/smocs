/**
 * Created on: Mar 5, 2013
 */
package com.tubros.constraints.api
package problem

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner


/**
 * The '''ArrayNamingPolicySpec''' type defines the unit tests applicable to
 * certifying the [[com.tubros.constraints.api.problem.ArrayNamingPolicy]]
 * type for use in Smocs.
 *
 * @author svickers
 *
 */
class ArrayNamingPolicySpec
	extends ProjectSpec
		with ArrayNamingPolicy
{
	"The ArrayNamingPolicy" should "be able to create array variable names" in
	{
		val root = 'myArrayName;
		val arrayName = compose (root, 0);
		
		arrayName shouldNot be (null);
		arrayName.toString should startWith (root.toString);
		arrayName.toString should endWith ("0");
		arrayName.toString.length should be > (root.toString.length + 1);
	}
	
	it should "be able to break apart a previously composed name" in
	{
		val maybeParts = decompose (compose ('aName, 22));
		
		maybeParts should be ('defined);
		maybeParts foreach {
			parts =>
				
			parts.productArity shouldBe (2);
			parts._1 shouldNot be (null);
			parts._2 should be >= (0);
			}
	}
	
	it should "not overly burden variable names" in
	{
		val withUnderscore = VariableName ("name_with_underscore");
		val maybeParts = decompose (compose (withUnderscore, 22));
		
		maybeParts should be ('defined);
		maybeParts foreach {
			parts =>
				
			parts._1 shouldBe (withUnderscore);
			}
	}
	
	it should "support higher-kinded operations" in
	{
		whenArrayName (compose ('test, 2)) ((a, b) => a) should be ('defined);
		whenArrayName ('scalarName) ((a, b) => a) should not be ('defined);
		
		unlessArrayName (compose ('test, 2)) (a => Some (a)) should not be ('defined);
		unlessArrayName ('scalarName) (a => Some (a)) should be ('defined);
	}
}
