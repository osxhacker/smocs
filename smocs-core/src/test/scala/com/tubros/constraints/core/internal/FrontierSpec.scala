/**
 * Created on: Feb 25, 2013
 */
package com.tubros.constraints.core.internal

import scalaz._

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api.ProjectSpec


/**
 * The '''FrontierSpec''' type defines the unit tests responsible for ensuring
 * that the [[com.tubros.constraints.core.internal.Frontier]] type manages
 * [[com.tubros.constraints.core.internal.SolutionSpace]] unexplored nodes
 * properly.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class FrontierSpec
	extends ProjectSpec
{
	/// Class Imports
	import scalaz.std.AllInstances._
	import scalaz.std.boolean._
	import scalaz.syntax.monoid._
	import scalaz.syntax.std.boolean._
	import scalaz.syntax.std.option._
	import Lens._
	
	
	"The Frontier" should "support a LIFO configuration" in
	{
		val lifo = Frontier.lifo[String]
		
		lifo must not be === (null);
	}
	
	it should "support a FIFO configuration" in
	{
		val fifo = Frontier.fifo[Double]
		
		fifo must not be === (null);
	}
	
	it should "satisfy LIFO expectations when enqueuing" in
	{
		val lifo = Frontier (
			add = lensu[Vector[String], Option[String]] (
				get = _.headOption,
				set = (l, e) => e.fold (l) (l :+ _)
				),
			remove = lensu[Vector[String], Option[String]] (
				get = _.headOption,
				set = (l, _) => l.tail
				)
			);
		
		val populated = lifo.enqueue ("first").enqueue ("second");
		
		populated should not be ('empty);
		
		val (oldest, frontier) = populated.dequeue;
		
		oldest should be ('defined);
		oldest should be === (Some ("first"));
		frontier should not be ('empty);
	}
	
	it should "satisfy FIFO expectations when enqueuing" in
	{
		val lifo = Frontier (
			add = lensu[Vector[String], Option[String]] (
				get = _.headOption,
				set = (l, e) => e.fold (l) (_ +: l)
				),
			remove = lensu[Vector[String], Option[String]] (
				get = _.headOption,
				set = (l, _) => l.tail
				)
			);
		
		val populated = lifo.enqueue ("first").enqueue ("second");
		
		populated should not be ('empty);
		
		val (oldest, frontier) = populated.dequeue;
		
		oldest should be ('defined);
		oldest should be === (Some ("second"));
		frontier should not be ('empty);
	}
}
