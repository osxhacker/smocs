/**
 * Created on: Feb 24, 2013
 */
package com.tubros.constraints.core.internal

import scala.collection.immutable.{
	Stack,
	Queue
	}
import scala.language.{
	higherKinds,
	postfixOps
	}

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
	extends FrontierLike[A, Frontier[A]]
{
	/**
	 * The isEmpty method indicates whether or not the '''Frontier''' has no
	 * more unexplored elements within it.
	 */
	def isEmpty : Boolean;
	
	
	def monoid : Monoid[Frontier[A]];
	
	
	/**
	 * The size method provides the caller with how large the '''Frontier'' is.
	 */
	def size : Int;
}


sealed trait FrontierLike[A, +This <: FrontierLike[A, This] with Frontier[A]]
{
	/**
	 * The enqueue method takes an '''element''' and places it in the
	 * '''Frontier''' based on the [[scalaz.Lens]] policy responsible for
	 * addition.
	 */
	def enqueue (element : A) : This;
	
	
	/**
	 * This version of enqueue allows an arbitrary number of ''elements'' to
	 * be placed into the '''Frontier'''.
	 */
	def enqueue (elements : Traversable[A]) : This;
	
	
	/**
	 * The dequeue method attempts to produce the next unexplored ''A''
	 * managed by the '''Frontier''' if one is available.  If not, the tuple
	 * returned will have a [[scala.None]] for its first element.
	 */
	def dequeue : (Option[A], This);
}


final case class LifoFrontier[A] (private[internal] val stack : Stack[A])
	extends Frontier[A]
		with FrontierLike[A, LifoFrontier[A]]
{
	/// Class Imports
	import syntax.std.boolean._
	
	
	/// Instance Properties
	override val isEmpty : Boolean = stack.isEmpty;
	override val monoid : Monoid[Frontier[A]] =
		new Monoid[Frontier[A]] with Frontier.RecursiveAppend[A] {
			lazy val zero : Frontier[A] = Frontier.lifo[A];
			}
	override val size : Int = stack.size;
	
	
	override def enqueue (element : A) : LifoFrontier[A] =
		copy (stack = stack.push (element));
	
	override def enqueue (elements : Traversable[A]) : LifoFrontier[A] =
		copy (stack = stack.pushAll (elements));
	
	override def dequeue : (Option[A], LifoFrontier[A]) =
		stack.isEmpty.fold (
			(None, this),
			(stack.headOption, copy (stack = stack.pop))
			);
}


final case class FifoFrontier[A] (private[internal] val queue : Queue[A])
	extends Frontier[A]
		with FrontierLike[A, FifoFrontier[A]]
{
	/// Class Imports
	import std.tuple._
	import syntax.bifunctor._
	import syntax.std.boolean._
	import syntax.std.option._
	
	
	/// Instance Properties
	override val isEmpty : Boolean = queue.isEmpty;
	override val monoid : Monoid[Frontier[A]] =
		new Monoid[Frontier[A]] with Frontier.RecursiveAppend[A] {
			lazy val zero : Frontier[A] = Frontier.fifo[A];
			}
	
	override val size : Int = queue.size;
	
	
	override def enqueue (element : A) : FifoFrontier[A] =
		copy (queue = queue.enqueue (element));
	
	override def enqueue (elements : Traversable[A]) : FifoFrontier[A] =
		copy (queue = queue ++ elements);
	
	override def dequeue : (Option[A], FifoFrontier[A]) =
		queue.isEmpty.fold (
			(None, this),
			((_ : A).some) <-: queue.dequeue :-> {
				q =>
					
				copy (queue = q);
				}
			);
}


private case class GenericFrontier[A, C[+_]] (
	override val size : Int,
	private val container : C[A],
	private val add : C[A] @> Option[A],
	private val remove : C[A] @> Option[A]
	)
	(implicit IE : IsEmpty[C], MO : Monoid[C[A]])
	extends Frontier[A]
		with FrontierLike[A, GenericFrontier[A, C]]
{
	/// Class Imports
	import std.option._
	import syntax.monoid._
	import syntax.std.boolean._
	import syntax.std.option._
	
	
	/// Instance Properties
	override val monoid : Monoid[Frontier[A]] =
		new Monoid[Frontier[A]] with Frontier.RecursiveAppend[A] {
			override def zero : Frontier[A] =
				new GenericFrontier[A, C] (0, mzero[C[A]], add, remove);
			}
	
	
	override def enqueue (element : A) : GenericFrontier[A, C] =
		copy (size = size + 1, container = add.set (container, element.some));
	
	
	override def enqueue (elements : Traversable[A]) : GenericFrontier[A, C] =
		elements.foldLeft (this) {
			case (q, e) =>
			
			q.enqueue (e);
			}
		
	override def dequeue : (Option[A], GenericFrontier[A, C]) =
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
	trait RecursiveAppend[A]
	{
		/// Self Type Constraints
		this : Monoid[Frontier[A]] =>
			
			
		override def append (a : Frontier[A], b : => Frontier[A])
			: Frontier[A] =
		{
			lazy val other = b;
			
			if (a.isEmpty)
				other;
			else if (other.isEmpty)
				a;
			else
			{
				val (e, f) = other.dequeue;
				
				append (a enqueue (e), f);
			}
		}
	}
	
	
	/**
	 * The apply method allows clients to create a '''Frontier''' instance
	 * which will use the '''add'' [[scalaz.Lens]] when an element needs to
	 * be retained and the '''remove''' [[scalaz.Lens]] to remove one.
	 */
	def apply[A, C[+_]] (
		add : C[A] @> Option[A],
		remove : C[A] @> Option[A]
		)
		(implicit ie : IsEmpty[C], mo : Monoid[C[A]])
		: Frontier[A] =
		GenericFrontier[A, C] (0, mzero[C[A]], add, remove);
	
	
	/**
	 * The fifo method creates a '''Frontier''' where an `enqueue` invocation
	 * results in the element being the first available for `dequeue`.
	 */
	def fifo[A] : FifoFrontier[A] = FifoFrontier[A] (Queue.empty[A]);
	
	
	/**
	 * The lifo method creates a '''Frontier''' where an `enqueue` invocation
	 * results in the element being the last available for `dequeue`.
	 */
	def lifo[A] : LifoFrontier[A] = LifoFrontier[A] (Stack.empty[A]);

	
	/// Implicit Conversions
	implicit def frontierLifoMonoid[A] : Monoid[LifoFrontier[A]] =
		new Monoid[LifoFrontier[A]]
		{
			lazy val zero : LifoFrontier[A] = lifo[A];
			
			override def append (a : LifoFrontier[A], b : => LifoFrontier[A])
				: LifoFrontier[A] =
				if (a.isEmpty)
					b;
				else
					LifoFrontier[A] (a.stack.pushAll (b.stack));
		}
	
	
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
