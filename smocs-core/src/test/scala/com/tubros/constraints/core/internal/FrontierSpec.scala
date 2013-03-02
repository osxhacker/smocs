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
		
		lifo must not be === (null);
	}
	
	it should "support a FIFO configuration" in
	{
		val fifo = Frontier (
			add = lensu[Vector[String], Option[String]] (
				get = _.headOption,
				set = (l, e) => e.fold (l) (_ +: l)
				),
			remove = lensu[Vector[String], Option[String]] (
				get = _.headOption,
				set = (l, _) => l.tail
				)
			);
		
		fifo must not be === (null);
	}
	
	it should "satisfy LIFO expectations when enqueuing" in
	{
		val lifo = Frontier.lifo[String];
		val populated = lifo.enqueue ("first").enqueue ("second");
		
		populated should not be ('empty);
		
		val (newest, frontier) = populated.dequeue;
		
		newest should be ('defined);
		newest should be === (Some ("second"));
		frontier should not be ('empty);
	}
	
	it should "satisfy FIFO expectations when enqueuing" in
	{
		val fifo = Frontier.fifo[String]
		val populated = fifo.enqueue ("first").enqueue ("second");
		
		populated should not be ('empty);
		
		val (oldest, frontier) = populated.dequeue;
		
		oldest should be ('defined);
		oldest should be === (Some ("first"));
		frontier should not be ('empty);
	}
	
	it should "conform to the Monoid laws" in
	{
		implicit object FifoEqual
			extends Equal[FifoFrontier[Int]]
		{
			override def equal (a : FifoFrontier[Int], b : FifoFrontier[Int]) =
				a.queue.to[List] == b.queue.to[List];
		}
		
		val fifo = Frontier.fifo[Int].enqueue (1).enqueue (2);
		val more = Frontier.fifo[Int].enqueue (3).enqueue (4);
		val someMore = Frontier.fifo[Int].enqueue (5);
		val monoid = implicitly[scalaz.Monoid[FifoFrontier[Int]]];
		val laws = monoid.monoidLaw;
		
		laws.leftIdentity (fifo) must be === (true);
		laws.rightIdentity (fifo) must be === (true);
		laws.associative (fifo, more, someMore) must be === (true);
	}
}
