/**
 * Created on: Jan 29, 2013
 */
package com.tubros.constraints.api.solver

import scala.collection.{
	GenTraversableOnce,
	SetLike
	}
import scala.collection.generic._

import scalaz.{
	Ordering => ScalazOrdering,
	_
	}


/**
 * The '''DiscreteDomain''' type represents
 * [[com.tubros.constraints.api.solver.Domain]]s in which their values are
 * separate and distinct.
 *
 * @tparam	T	The value type within the '''DiscreteDomain'''
 * 
 * @author svickers
 *
 */
trait DiscreteDomain[T]
	extends Domain[T]
		with Set[T]
		with SetLike[T, DiscreteDomain[T]]
		with Serializable
{
	/// Instance Properties
	final override val isDiscrete : Boolean = true;
	override def companion : GenericCompanion[DiscreteDomain] = DiscreteDomain;
	override def empty : DiscreteDomain[T] = DiscreteDomain.empty[T];
	override def seq : DiscreteDomain[T] = this;
}


trait DiscreteDomainInstances
{
	/// Class Types
	sealed trait EmptyDomain[T]
		extends DiscreteDomain[T]
	{
		/// Instance Properties
		override val isEmpty : Boolean = true;
		override val iterator : Iterator[T] = Iterator.empty;
		override val size : Int = 0;
		
		override def + (elem : T) : DiscreteDomain[T] =
			FiniteDiscreteDomain (elem);
		override def - (elem : T) : DiscreteDomain[T] = this;
		override def bounds (implicit ev : Ordering[T]) : Option[(T, T)] = None;
		override def contains (elem : T) : Boolean = false;
		override def foreach[R] (f : T => R) : Unit = {}
	}
	
	
	/// Implicit Conversions
	implicit def equal[T] : Equal[DiscreteDomain[T]] = new Equal[DiscreteDomain[T]] {
		override def equal (lhs : DiscreteDomain[T], rhs : DiscreteDomain[T])
			: Boolean =
			(lhs.size == rhs.size) && (lhs == rhs);
		}
	
	
	/**
	 * '''DiscreteDomain''' types are models of [[scalaz.Monoid]].
	 */
	implicit def monoid[T] : Monoid[DiscreteDomain[T]] =
		new Monoid[DiscreteDomain[T]] {
			override def zero = DiscreteDomain.empty[T];
			override def append (a : DiscreteDomain[T], b : => DiscreteDomain[T])
				: DiscreteDomain[T] =
				a ++ b;
			}
	
	
	/**
	 * Every '''DiscreteDomain'' gives rise to a new [[scalaz.Category]].
	 */
	implicit def category[T] = Monoid[DiscreteDomain[T]].category;
}


object DiscreteDomain
	extends ImmutableSetFactory[DiscreteDomain]
		with DiscreteDomainInstances
{
	override def empty[A] : DiscreteDomain[A] = new EmptyDomain[A] {};
}
