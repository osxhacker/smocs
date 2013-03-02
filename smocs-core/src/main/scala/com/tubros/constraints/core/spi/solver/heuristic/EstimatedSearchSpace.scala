/**
 * Created on: Mar 2, 2013
 */
package com.tubros.constraints.core.spi.solver.heuristic

import scala.language.{
	higherKinds,
	postfixOps
	}
import scalaz._

import com.tubros.constraints.api.solver._


/**
 * The '''EstimatedSearchSpace''' type is a heuristic for approximating how
 * large an exhaustive search of a solution space would be.
 *
 * @author svickers
 *
 */
object EstimatedSearchSpace
{
	/// Class Imports
	import std.anyVal._
	import std.tuple._
	import Reducer._
	import Tags._
	
	
	def apply[A, C[_] : Foldable, DomainT[X] <: Domain[X]] (
		domains : C[DomainT[A]]
		) : Long =
	{
		implicit val r = countAndProduct[A, DomainT];
		
		foldReduce[C, DomainT[A], (Long, Long @@ Multiplication)] (domains) match {
			case (0L, _) => 0L
			case (_, size) => size;
			}
	}
	
	
	private def countAndProduct[A, DomainT[X] <: Domain[X]]
		: Reducer[DomainT[A], (Long, Long @@ Multiplication)] =
		unitReducer {
			domain =>
				
			(domain.size.toLong, Tag[Long, Multiplication] (domain.size.toLong));
			}
}