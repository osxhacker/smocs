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
import com.tubros.constraints.core.spi.solver.runtime.{
	ConstraintProvider,
	SymbolTableProvider
	}


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
	/// Class Types
	type ProviderType = ConstraintProvider[A] with SymbolTableProvider
	
	
	def apply (provider : ProviderType) :
		List[Variable[A, DiscreteDomain]] => List[Variable[A, DiscreteDomain]];
		
		
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
	override def apply (provider : ProviderType) :
		List[Variable[A, DiscreteDomain]] => List[Variable[A, DiscreteDomain]] =
		head (provider) andThen tail (provider);
}


sealed trait Score
{
	/// Class Imports
	import Scalaz._
	
	
	def score[A : Equal] (
		provider : VariableRankingPolicy[A]#ProviderType,
		variables : List[Variable[A, DiscreteDomain]],
		subjects : List[Variable[A, DiscreteDomain]]
		)
		: List[(Double, Variable[A, DiscreteDomain])] =
	{
		val assess = AssignmentImpact (variables, provider).ofVariable _;
		val impacts = subjects.fpair.map {
			paired =>
			
			assess <-: paired;
			};
		
		impacts.toList.sortBy (_._1).reverse;
	}
}


final case class ImpactRankingPolicy[A : Equal] ()
	extends VariableRankingPolicy[A]
		with Score
{
	/// Class Imports
	import Scalaz._
	
	
	override def apply (provider : ProviderType) :
		List[Variable[A, DiscreteDomain]] => List[Variable[A, DiscreteDomain]] =
		available => score (provider, available, available).map (_._2);
}


final case class PreferSmallerDomain[A : Equal] ()
	extends VariableRankingPolicy[A]
		with Score
{
	/// Class Imports
	import Scalaz._
	
	
	override def apply (provider : ProviderType) :
		List[Variable[A, DiscreteDomain]] => List[Variable[A, DiscreteDomain]] =
		available => {
			available match {
				case (first :: second :: tail)
					if (sameImpact (provider, available, first, second))
					=>
						
					(first :: second :: Nil).sortWith {
						(a, b) =>
							
						a.domain.size < b.domain.size;
						} ::: tail;
						
				case l =>
					l;
				}
			}
		
		
	private def sameImpact (
		provider : ProviderType,
		available : List[Variable[A, DiscreteDomain]],
		a : Variable[A, DiscreteDomain],
		b : Variable[A, DiscreteDomain]
		)
		: Boolean =
	{
		val scores = score (provider, available, a :: b :: Nil).map (_._1);
		
		return (scores (0) === scores (1));
	}
}
