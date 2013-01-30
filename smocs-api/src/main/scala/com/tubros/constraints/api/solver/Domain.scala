/**
 * Created on: Jan 28, 2013
 */
package com.tubros.constraints.api.solver

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
	/// Instance Properties
	def isDiscrete : Boolean;
	def isInfinite : Boolean;
	
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
	private class ShowDomain[T : Show]
		extends Show[Domain[T]]
	{
		override def show (d : Domain[T]) : Cord =
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
		
		
		private def sampling (d : Domain[T])
			: (Cord, Int) =
		{
			val entries = d.iterator.take (10).toSeq;
			
			(
				Cord.mkCord (",", entries.map (Show[T].show) : _ *),
				entries.size
			);
		}
	}
	
	
	/// Implicit Conversions
	implicit def domainShow[T] (implicit S : Show[T]) : Show[Domain[T]] =
		new ShowDomain[T];
}


object Domain
	extends DomainInstances
	