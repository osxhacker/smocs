/**
 * Created on: Jan 28, 2013
 */
package com.tubros.constraints.api.solver

import scala.language.higherKinds

import scalaz.{
	Cord,
	Show
	}


/**
 * The '''Domain''' type represents the complete legal values an arbitrary
 * object (of type ''T'') can take.  This does not have to be the entire
 * set of values ''T'' can take, however, and will often not be that case.
 * 
 * So, for example, if ''T'' is defined as an `Int`, then the '''Domain'' of
 * all natural numbers would not include negatives.
 *
 * @tparam	T	The value type within the '''Domain'''
 * 
 * @author svickers
 *
 */
trait Domain[T]
	extends collection.Iterable[T]
{
	/// Class Types
	type MemberType = T
	
	
	/// Instance Properties
	/**
	 * The empty property must produce a '''Domain''' which has no contents,
	 * for whatever definition of that means to the concrete type.
	 */
	def empty : Domain[T];
	
	/**
	 * The isDiscrete property indicates whether or not this '''Domain'''
	 * contains values which are not contiguous.
	 */
	def isDiscrete : Boolean;
	
	
	/**
	 * The bounds method produces a minima/maxima [[scala.Tuple2]] for the
	 * '''Domain'' value type, if possible.
	 */
	def bounds (implicit ev : Ordering[T]) : Option[(T, T)];
	
	
	/**
	 * The contains method determines whether or not the given '''item''' is
	 * within this '''Domain'''.
	 */
	def contains (item : T) : Boolean;
}


trait DomainInstances
{
	/// Class Types
	private class ShowDomain[T : Show, DomainT[T] <: Domain[T]]
		extends Show[DomainT[T]]
	{
		override def show (d : DomainT[T]) : Cord =
		{
			sampling (d, 10) match {
				case (firstTen, more) if (more) =>
					Cord ("Domain(", firstTen, "...)");
					
				case (empty, false) if (empty.length == 0) =>
					Cord ("Domain(<empty>)");
					
				case (tenOrLess, _) =>
					Cord ("Domain(", tenOrLess, ")");
				}
			}
		
		
		private def sampling (d : DomainT[T], howMany : Int)
			: (Cord, Boolean) =
		{
			val entries = d.iterator.take (howMany).toSeq;

			(
				Cord.mkCord (",", entries.map (Show[T].show) : _ *),
				!d.iterator.drop (howMany).isEmpty
			);
		}
	}
	
	
	/// Implicit Conversions
	implicit def domainShow[T : Show, DomainT[T] <: Domain[T]]
		: Show[DomainT[T]] =
		new ShowDomain[T, DomainT];
}


object Domain
	extends DomainInstances
	
