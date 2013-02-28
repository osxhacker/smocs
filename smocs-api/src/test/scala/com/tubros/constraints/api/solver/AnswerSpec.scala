/**
 * Created on: Feb 17, 2013
 */
package com.tubros.constraints.api.solver

import scalaz._

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api._


/**
 * The '''AnswerSpec''' type defines the unit tests used to certify the
 * [[com.tubros.constraints.api.solver.Answer]] `solver` type.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class AnswerSpec
	extends ProjectSpec
{
	/// Class Imports
	import scalaz.std.AllInstances._
	import scalaz.syntax.comonad._
	import scalaz.syntax.show._
	
	
	"An Answer" should "have both a VariableName and a value" in
	{
		val answer = Answer ('x, 42);
		
		answer.name must be === ('x);
		answer.value must be === (42);
	}
	
	it should "support transformations to/from Tuple2" in
	{
		val answer = Answer ('y, "just for test");
		
		answer.toTuple should be === (('y, "just for test"));
	}
	
	it should "support Scalaz Functor" in
	{
		val functor = implicitly[Functor[Answer]];
		val laws = functor.functorLaw;
		
		laws.identity (Answer ('a, 99)) must be === (true);
		laws.composite (
			Answer ('a, 99),
			(_ : Int).toString,
			(_ : String).length
			) should be === (true);
	}
	
	it should "support Scalaz Equal" in
	{
		val a1 = Answer ('a, 1);
		val a2 = Answer ('a, 2);
		val b = Answer ('b, 1);
		val e = implicitly[Equal[Answer[Int]]];
		
		e.equal (a1, a1) should be === (true);
		e.equal (a1, a2) should be === (false);
		e.equal (a1, b) should be === (false);
	}
	
	it should "support Scalaz Comonad" in
	{
		val a = Answer ('a, 42);
		
		a.copoint should be === (42);
	}
	
	it should "support Scalaz Show" in
	{
		val cord = Answer ('z, 99).show;
		
		cord must not be === (null);
	}
}
