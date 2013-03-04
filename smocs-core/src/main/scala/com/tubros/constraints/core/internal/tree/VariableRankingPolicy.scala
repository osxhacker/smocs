/**
 * Created on: Mar 3, 2013
 */
package com.tubros.constraints.core.internal.tree

import scala.language.higherKinds

import scalaz._

import com.tubros.constraints.api.solver.{
	DiscreteDomain,
	Variable
	}
import com.tubros.constraints.core.spi.solver.Constraint
import com.tubros.constraints.core.spi.solver.heuristic.AssignmentImpact


/**
 * The '''VariableRankingPolicy''' type defines the ability to determine the
 * relative importance of each [[com.tubros.constraints.api.solver.Variable]]
 * in the context of zero or more
 * [[com.tubros.constraints.core.spi.solver.Constraint]]s.  Since this is a
 * tree-specific heuristic, the ability to have problem-specific rankings is
 * provided by being able to compose 1+ '''VariableRankingPolicy''' instances
 * using the
 * [[com.tubros.constraints.core.internal.tree.CompositeRanking]] type.
 *
 * @author svickers
 *
 */
abstract class VariableRankingPolicy[A : Equal]
{
	def apply[N[_] : Foldable] : N[Constraint[A]] =>
		List[Variable[A, DiscreteDomain]] =>
		List[Variable[A, DiscreteDomain]];
		
		
	def andThen (next : VariableRankingPolicy[A]) : VariableRankingPolicy[A] =
		CompositeRanking (this, next);
		
		
	def compose (next : VariableRankingPolicy[A]) : VariableRankingPolicy[A] =
		CompositeRanking (next, this);
}


final case class CompositeRanking[A : Equal] (
	head : VariableRankingPolicy[A],
	tail : VariableRankingPolicy[A]
	)
	extends VariableRankingPolicy[A]
{
	override def apply[N[_] : Foldable] : N[Constraint[A]] =>
		List[Variable[A, DiscreteDomain]] =>
		List[Variable[A, DiscreteDomain]] =
		constraints => head[N].apply (constraints) andThen tail[N].apply (constraints);
}

	
final case class ImpactRankingPolicy[A : Equal] ()
	extends VariableRankingPolicy[A]
{
	/// Class Imports
	import Scalaz._
	
	
	override def apply[N[_] : Foldable] : N[Constraint[A]] =>
		List[Variable[A, DiscreteDomain]] =>
		List[Variable[A, DiscreteDomain]] =
		constraints => available => {
			val assess = AssignmentImpact (available, constraints).ofVariable _;
			val impacts = available.fpair.map {
				paired =>
				
				assess <-: paired;
				};
			
			impacts.toList.sortBy (_._1).map (_._2).reverse;
		}
}


final case class PreferSmallerDomain[A : Equal] ()
	extends VariableRankingPolicy[A]
{
	/// Class Imports
	import Scalaz._
	
	
	override def apply[N[_] : Foldable] : N[Constraint[A]] =>
		List[Variable[A, DiscreteDomain]] =>
		List[Variable[A, DiscreteDomain]] =
		constraints => available => {
			available match {
				case (first :: second :: tail)
					if (sameImpact (constraints, available, first, second))
					=>
						
					(first :: second :: Nil).sortWith {
						(a, b) =>
							
						a.domain.size < b.domain.size;
						} ::: tail;
						
				case l =>
					l;
				}
			}
		
		
	private def sameImpact[N[_] : Foldable] (
		constraints : N[Constraint[A]],
		available : List[Variable[A, DiscreteDomain]],
		a : Variable[A, DiscreteDomain],
		b : Variable[A, DiscreteDomain]
		)
		: Boolean =
	{
		val assessor = AssignmentImpact (available, constraints).ofVariable _;
		
		return (assessor (a) === assessor (b));
	}
}