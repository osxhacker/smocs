/**
 * Created on: Jan 29, 2013
 */
package com.tubros.constraints.api.solver


/**
 * The '''FiniteDiscreteDomain''' type is a
 * [[com.tubros.constraints.api.solver.DiscreteDomain]] which has a
 * quantifiable `size`.
 *
 * @tparam	T	The value type within the '''FiniteDiscreteDomain'''
 * 
 * @author svickers
 *
 */
final case class FiniteDiscreteDomain[T] (private val self : Set[T])
	extends DiscreteDomain[T]
{
	/// Class Imports
	import scalaz.syntax.std.boolean._
	
	
	/// Instance Properties
	override val hasDefiniteSize : Boolean = true;
	override lazy val iterator : Iterator[T] = self.iterator;
	override lazy val size : Int = self.size;
	
	def this () = this (Set.empty[T]);
	def this (elem : T) = this (Set (elem));
	
	override def + (elem : T) : DiscreteDomain[T] =
		FiniteDiscreteDomain (self + elem);
	
	override def - (elem : T) : DiscreteDomain[T] =
		FiniteDiscreteDomain (self - elem);
	
	override def contains (elem : T) : Boolean = self.contains (elem);
	
	override def foreach[R] (f : T => R) : Unit = self.foreach (f);
	
	override def bounds (implicit ev : Ordering[T]) : Option[(T, T)] =
		(!isEmpty).option ((self.min, self.max));
}


object FiniteDiscreteDomain
{
	/// Class Imports
	import scalaz._
	
	
	/**
	 * This apply method is provided to support functional creation style
	 * when the collaborator desires a variadic form.
	 */
	def apply[T] (head : T, tail : T *) : FiniteDiscreteDomain[T] =
		new FiniteDiscreteDomain[T] ((head :: tail.to[List]).to[Set]);
	
	
	/**
	 * This apply method is provided to support functional creation style
	 * when the collaborator has a '''collection'''.
	 */
	def apply[T] (collection : TraversableOnce[T]) : FiniteDiscreteDomain[T] =
		new FiniteDiscreteDomain[T] (collection.to[Set]);
	
	
	/**
	 * This apply method is provided to support functional creation style
	 * when the collaborator has a [[scalaz.EphemeralStream]].
	 */
	def apply[T] (stream : EphemeralStream[T]) : FiniteDiscreteDomain[T] =
		new FiniteDiscreteDomain[T] (stream.to[Set]);
}
