/**
 * Created on: Mar 5, 2013
 */
package com.tubros.constraints.api
package solver

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import problem.ArrayNamingPolicy


/**
 * The '''AnswerExtractorsSpec''' type defines unit tests which exercise
 * various extractors used when [[com.tubros.constraints.api.solver.Answer]]s
 * are being processed.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class AnswerExtractorsSpec
	extends ProjectSpec
		with ArrayNamingPolicy
{
	/// Testing Collaborators
	val answers = Seq (
		Answer ('a, 1),
		Answer ('b, 2),
		Answer ('c, 3),
		Answer (compose ('array1, 0), 100),
		Answer (compose ('array1, 1), 101),
		Answer (compose ('array2, 0), 200),
		Answer (compose ('array2, 1), 201)
		);
	
	
	"ScalarNamed" should "match when a Seq has a corresponding Answer" in
	{
		answers match {
			case _ :: ScalarNamed ("b", v) :: tail =>
				v shouldBe (2);
				
			case _ =>
				fail ("Did not match an existing scalar");
			}
	}
	
	it should "match arrays in the Answer collection" in
	{
		answers match {
			case ArrayVariables (a1, a2) =>
				a1 shouldNot be (null);
				a1 should not be ('empty);
				a1 should contain key (0);
				a1 should contain key (1);
				a1 should not contain key (99);
			}
	}
	
	it should "produce a sequence of arrays when wanted" in
	{
		answers match {
			case AllArrays (all) =>
				all.size shouldBe (2);
				
			case other =>
				fail ("unexpected match: %s".format (other));
			}
	}
}
