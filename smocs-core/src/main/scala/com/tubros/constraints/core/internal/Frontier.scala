/**
 * Created on: Feb 24, 2013
 */
package com.tubros.constraints.core.internal

import scala.collection.immutable.{
	Stack,
	Queue
	}
import scala.language.higherKinds

import scalaz._


/**
 * The '''Frontier''' type formalizes the concept of managing solution nodes
 * at the fringe of the [[com.tubros.constraints.core.internal.SolutionSpace]].
 * It allows for a configurable queuing policy by way of being parameterized
 * on the [[scalaz.Lens]] used to manipulate the collection.
 *
 * @author svickers
 *
 */
sealed trait Frontier[A]
{
	/**
	 * The enqueue method takes an '''element''' and places it in the
	 * '''Frontier''' based on the [[scalaz.Lens]] policy responsible for
	 * addition.
	 */
	def enqueue (element : A) : Frontier[A];
	
	
	/**
	 * The dequeue method attempts to produce the next unexplored ''A''
	 * managed by the '''Frontier''' if one is available.  If not, the tuple
	 * returned will have a [[scala.None]] for its first element.
	 */
	def dequeue : (Option[A], Frontier[A]);
	
	
	/**
	 * The isEmpty method indicates whether or not the '''Frontier''' has no
	 * more unexplored elements within it.
	 */
	def isEmpty : Boolean;
	
	
	/**
	 * The size method provides the caller with how large the '''Frontier'' is.
	 */
	def size : Int;
}


private case class DefaultFrontier[A, C[+_]] (
	override val size : Int,
	private val container : C[A],
	private val add : Lens[C[A], Option[A]],
	private val remove : Lens[C[A], Option[A]]
	)
	(implicit ie : IsEmpty[C])
	extends Frontier[A]
{
	/// Class Imports
	import std.option._
	import syntax.monoid._
	import syntax.std.boolean._
	import syntax.std.option._
	
	
	override def enqueue (element : A) : Frontier[A] =
		copy (size = size + 1, container = add.set (container, element.some));
	
	
	override def dequeue : (Option[A], Frontier[A]) =
		isEmpty fold (
			(none[A], this),
			{
				val element = remove.get (container);
			
				(
					element,
					copy (
						size = size - 1,
						container = remove.set (container, element)
						)
				)
			});
	
		
	override def isEmpty : Boolean = IsEmpty[C].isEmpty (container);
}


object Frontier
{
	/// Class Imports
	import syntax.monoid._
	import syntax.std.option._
	import Lens._
	
	
	/// Class Types
	implicit object StackIsEmpty
		extends IsEmpty[Stack]
	{
		override def empty[A] = Stack.empty[A];
		override def isEmpty[A] (sa : Stack[A]) = sa.isEmpty;
		override def plus[A] (a : Stack[A], b : => Stack[A]) = a.pushAll (b);
	}
	
	
	implicit object QueueIsEmpty
		extends IsEmpty[Queue]
	{
		override def empty[A] = Queue.empty[A];
		override def isEmpty[A] (qa : Queue[A]) = qa.isEmpty;
		override def plus[A] (a : Queue[A], b : => Queue[A]) = a.enqueue (b);
	}
	
	
	/**
	 * The apply method allows clients to create a '''Frontier''' instance
	 * which will use the '''add'' [[scalaz.Lens]] when an element needs to
	 * be retained and the '''remove''' [[scalaz.Lens]] to remove one.
	 */
	def apply[A, C[+_]] (
		add : Lens[C[A], Option[A]],
		remove : Lens[C[A], Option[A]]
		)
		(implicit ie : IsEmpty[C], mo : Monoid[C[A]])
		: Frontier[A] =
		DefaultFrontier[A, C] (0, mzero[C[A]], add, remove);
	
	
	/**
	 * The fifo method creates a '''Frontier''' where an `enqueue` invocation
	 * results in the element being the first available for `dequeue`.
	 */
	def fifo[A] : Frontier[A] =
		DefaultFrontier[A, Stack] (
			0,
			Stack.empty[A],
			add = lensu[Stack[A], Option[A]] (
				get = s => s.headOption,
				set = (s, e) => e.fold (s) (a => s.push (a))
				),
			remove = lensu[Stack[A], Option[A]] (
				get = s => s.headOption,
				set = (s, e) => e.fold (s) (_ => s.pop)
				)
			);
	
	
	/**
	 * The lifo method creates a '''Frontier''' where an `enqueue` invocation
	 * results in the element being the last available for `dequeue`.
	 */
	def lifo[A] : Frontier[A] =
		DefaultFrontier[A, Queue] (
			0,
			Queue.empty[A],
			add = lensu[Queue[A], Option[A]] (
				get = q => q.headOption,
				set = (q, e) => e.fold (q) (a => q.enqueue (a))
				),
			remove = lensu[Queue[A], Option[A]] (
				get = q => q.headOption,
				set = (q, e) => e.fold (q) (_ => q.dequeue._2)
				)
			);
	
	
	/// Implicit Conversions
	implicit def frontierShow[A] : Show[Frontier[A]] =
		new Show[Frontier[A]]
		{
			/// Class Imports
			import syntax.std.boolean._
			import Cord._
			
			
			/// Instance Properties
			private val maxToPrint = 3;
			
			
			override def show (frontier : Frontier[A]) : Cord =
				Cord ("Frontier(") ++
				mkCord (Cord (", "), nodes (frontier, 0) : _ *) ++
				Cord (")");
			
			private def nodes (frontier : Frontier[A], count : Int)
				: List[Cord] =
			{
				val (cur, next) = frontier.dequeue;
				lazy val emptyCord = next.isEmpty.fold (
					List.empty[Cord],
					List (Cord ("... %d more".format (frontier.size)))
					)
				
				cur.filter (_ => count < maxToPrint).fold (emptyCord) {
					node =>
						
					stringToCord (node.toString) :: nodes (next, count + 1);
					}
				}
			}
}
