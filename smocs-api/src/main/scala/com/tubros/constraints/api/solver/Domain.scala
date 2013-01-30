/**
 * Created on: Jan 28, 2013
 */
package com.tubros.constraints.api.solver


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
