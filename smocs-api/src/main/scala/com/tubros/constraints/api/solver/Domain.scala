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
	def isDiscrete : Boolean;
	
	def empty : Domain[T];
	
	
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
			sampling (d) match {
				case (more, n) if (n < d.size) =>
					Cord ("Domain(", more, "...)");
					
				case (_, 0) =>
					Cord ("Domain(<empty>)");
					
				case (tenOrLess, _) =>
					Cord ("Domain(", tenOrLess, ")");
				}
			}
		
		
		private def sampling (d : DomainT[T])
			: (Cord, Int) =
		{
			val entries = d.iterator.take (10).toSeq
			
			(
				Cord.mkCord (",", entries.map (Show[T].show) : _ *),
				entries.size
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
	
