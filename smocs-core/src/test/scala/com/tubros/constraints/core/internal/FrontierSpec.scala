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
	import Scalaz._
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
		
		lifo shouldNot be (null);
	}
	
	it should "support a FIFO configuration" in
	{
		val fifo = Frontier (
			add = lensu[Vector[String], Option[String]] (
				get = _.headOption,
				set = (l, e) => e.fold (l) (l :+ _)
				),
			remove = lensu[Vector[String], Option[String]] (
				get = _.headOption,
				set = (l, _) => l.tail
				)
			);
		
		fifo shouldNot be (null);
	}
	
	it should "be able to stream its contents" in
	{
		val fifo = Frontier (
			add = lensu[Vector[String], Option[String]] (
				get = _.headOption,
				set = (l, e) => e.fold (l) (l :+ _)
				),
			remove = lensu[Vector[String], Option[String]] (
				get = _.headOption,
				set = (l, _) => l.tail
				)
			);
		val withThree = fifo.enqueue ("a").enqueue ("b").enqueue ("c");
		val contents = withThree.toStream.toList;
		
		contents should have size (3);
		contents shouldBe (List ("a", "b", "c"));
	}
	
	it should "satisfy LIFO expectations when enqueuing" in
	{
		val lifo = Frontier.lifo[String];
		val populated = lifo.enqueue ("first").enqueue ("second");
		
		populated should not be ('empty);
		
		val (newest, frontier) = populated.dequeue;
		
		newest should be ('defined);
		newest shouldBe (Some ("second"));
		frontier should not be ('empty);
	}
	
	it should "satisfy FIFO expectations when enqueuing" in
	{
		val fifo = Frontier.fifo[String]
		val populated = fifo.enqueue ("first").enqueue ("second");
		
		populated should not be ('empty);
		
		val (oldest, frontier) = populated.dequeue;
		
		oldest should be ('defined);
		oldest shouldBe (Some ("first"));
		frontier should not be ('empty);
	}
	
	it should "conform to the Monoid laws" in
	{
		implicit object FrontierEqual
			extends Equal[Frontier[Int]]
		{
			override def equal (a : Frontier[Int], b : Frontier[Int]) =
				a.toStream.toList == b.toStream.toList;
		}
		
		val fifo = Frontier.fifo[Int].enqueue (1).enqueue (2);
		val more = Frontier.fifo[Int].enqueue (3).enqueue (4);
		val someMore = Frontier.fifo[Int].enqueue (5);
		val laws = fifo.monoid.monoidLaw;
		implicit val m = fifo.monoid;
		
		laws.leftIdentity (fifo) shouldBe (true);
		laws.rightIdentity (fifo) shouldBe (true);
		laws.associative (fifo, more, someMore) shouldBe (true);
	}
	
	it should "retain ordering when using mappend" in
	{
		val five = Frontier.lifo[Int].enqueue (List.range (0, 5));
		implicit val m = five.monoid;
		val expected = List (0, 1, 2, 3, 4);
		val copied = Frontier.lifo[Int].mappend (five);
		
		copied.toStream.toList shouldBe (expected);
		
		val another = Frontier.lifo[Int].enqueue (10).enqueue (20);
		val copiedAgain = copied |+| another;
		
		copiedAgain.toStream.toList shouldBe (List (20, 10) ::: expected);
	}
}
