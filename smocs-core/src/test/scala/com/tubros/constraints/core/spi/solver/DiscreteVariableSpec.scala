/**
 * Created on: Feb 1, 2013
 */
package com.tubros.constraints.core.spi.solver

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api._

import solver._


/**
 * The '''DiscreteVariableSpec''' type defines the unit tests related to
 * certifying the [[com.tubros.constraints.core.spi.solver.DiscreteVariable]]
 * SPI type.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class DiscreteVariableSpec
	extends ProjectSpec
{
	"A DiscreteVariable" should "retain its name when created" in
	{
		val myName = 'isMud;
		val empty = DiscreteVariable[Int] (myName);
		
		empty.name must be === (myName);
	}
	
	it should "be able to 'enumerate' its domain" in
	{
		val expected = Stream ('a -> 1, 'a -> 2, 'a -> 3);
		val a = DiscreteVariable[Int] ('a, FiniteDiscreteDomain (1 to 3));
		
		a.enumerate.to[List] should be === (expected.to[List]);
	}
	
	it should "be able to 'map' its domain" in
	{
		val expected = FiniteDiscreteDomain (1 to 5);
		val five = DiscreteVariable[Int] ('x, expected) map (_.toString);
		
		five.domain should be === (expected.map (_.toString));
	}
	
	it should "be able to 'flatMap' its contents" in
	{
		val initial = FiniteDiscreteDomain (1 to 5);
		val five = DiscreteVariable[Int] ('x, initial);
		val ten = five flatMap {
			(n, d) =>
				
			DiscreteVariable (n, d ++ (d map (_ + 5)));
		}
		
		ten.name must be === (five.name);
		ten.domain should have size (10);
	}
	
	it should "be able to filter its domain" in
	{
		val domain = FiniteDiscreteDomain ("A", "b", "Cee", "dee");
		val original = DiscreteVariable ('orig, domain);
		
		original.filter (_ == "b") should be === (
			DiscreteVariable (original.name, FiniteDiscreteDomain ("b"))
			);
	}
	
	it should "be usable as a Functor" in
	{
		import scalaz._
		import Scalaz._
		
		val original = DiscreteVariable ('x, FiniteDiscreteDomain (1 to 5));
		
		original.fpair.domain.headOption should be ('defined);
	}
}
