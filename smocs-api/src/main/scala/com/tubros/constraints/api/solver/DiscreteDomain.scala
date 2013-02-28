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
	
	
	/**
	 * Since '''DiscreteDomain''' is a model of [[scala.collection.Set]], it
	 * can support all of the Scalaz constructs applicable to a
	 * [[scala.collection.Set]].
	 */
	implicit object ScalazDiscreteDomain
		extends Each[DiscreteDomain]
			with Foldable[DiscreteDomain]
			with IsEmpty[DiscreteDomain]
			with Length[DiscreteDomain]
			with MonadPlus[DiscreteDomain]
	{
		override def bind[A, B] (fa : DiscreteDomain[A])
			(f : A => DiscreteDomain[B])
			: DiscreteDomain[B] = fa flatMap f;
		
		override def each[A] (fa : DiscreteDomain[A])
			(f : A => Unit)
			: Unit = fa foreach f;
		
		override def empty[A] : DiscreteDomain[A] = DiscreteDomain.empty[A];
		
		override def foldMap[A, B] (fa : DiscreteDomain[A])
			(f : A => B)
			(implicit MO : Monoid[B])
			: B =
			fa.foldLeft (MO.zero) {
				case (accum, element) =>
					
				MO.append (accum, f (element));
				}
		
		override def foldRight[A, B] (fa : DiscreteDomain[A], z : => B)
			(f : (A, => B) => B)
			: B =
			fa.foldRight (z) (f (_, _));
		
		override def isEmpty[A] (fa : DiscreteDomain[A]) : Boolean =
			fa.isEmpty;
		
		override def length[A] (fa : DiscreteDomain[A]) : Int = fa.size;
		
		override def map[A,B] (fa : DiscreteDomain[A])
			(f : A => B)
			: DiscreteDomain[B] = fa map f;
		
		override def plus[A] (a : DiscreteDomain[A], b : => DiscreteDomain[A])
			: DiscreteDomain[A] = a ++ b;
		
		override def point[A] (a : => A) : DiscreteDomain[A] =
			DiscreteDomain[A] (a);
	}
}


object DiscreteDomain
	extends ImmutableSetFactory[DiscreteDomain]
		with DiscreteDomainInstances
{
	override implicit def setCanBuildFrom[A]
		: CanBuildFrom[Coll,A, DiscreteDomain[A]] =
		super.setCanBuildFrom[A];
	
	
	override def empty[A] : DiscreteDomain[A] = new EmptyDomain[A] {};
}
