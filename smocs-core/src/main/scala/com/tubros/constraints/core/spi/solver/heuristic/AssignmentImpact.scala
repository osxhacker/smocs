/**
 * Created on: Mar 2, 2013
 */
package com.tubros.constraints.core.spi.solver.heuristic

import scala.language.higherKinds

import scalaz._

import com.tubros.constraints.api._
import com.tubros.constraints.api.solver._
import com.tubros.constraints.core.spi.solver._

import runtime.{
	ConstraintProvider,
	SymbolTableProvider
	}


/**
 * The '''AssignmentImpact''' type is a heuristic which determines how much a
 * [[com.tubros.constraints.api.solver.Variable]] impacts the estimated
 * solution space size given an assignment to one of its
 * [[com.tubros.constraints.api.solver.Domain]] values.
 * 
 * In this context, a score closer to `1.0` indicates a ''higher'' impact on
 * the solution space than those closer to `1.0`.
 *
 * @author svickers
 *
 */
case class AssignmentImpact[
	A : Equal,
	M[_] : Foldable : Functor
	] (
		val variables : M[Variable[A, DiscreteDomain]],
		val provider : ConstraintProvider[A] with SymbolTableProvider
		)
{
	/// Class Imports
	import Scalaz._
	
	
	/// Instance Properties
	lazy val estimatedSolutionSize = Option (
		EstimatedSearchSpace (variables map (_.domain))
		) filterNot (_ === 0L);
	val propagator = ConstraintPropagation[A, DiscreteDomain] (
		provider,
		provider.symbols
		);
	
	
	def apply[VariableT <: Variable[A, DiscreteDomain]] (
		assignments : Seq[Answer[A]],
		variable : VariableT,
		value : A
		) : Double =
		estimatedSolutionSize.fold (1.0) {
			calculate (assignments, variable, value);
			}
	
	
	def ofVariable[VariableT <: Variable[A, DiscreteDomain]] (
		variable : VariableT
		) : Double =
		estimatedSolutionSize.fold (1.0) {
			originalSize =>
				
			estimate (
				originalSize,
				propagator (Seq.empty[Answer[A]], variable)
				);
			}
	
	
	@inline
	private def calculate[VariableT <: Variable[A, DiscreteDomain]] (
		assignments : Seq[Answer[A]],
		variable : VariableT,
		value : A
		)
		(originalSize : Long)
		: Double =
	{
		val afterAssignment = propagator (
			assignments,
			variable flatMap {
				(name, _) =>
					
				DiscreteVariable[A] (name, FiniteDiscreteDomain (value));
				}
			);
		
		return (estimate (originalSize, afterAssignment));
	}
	
	
	@inline
	private def estimate[VariableT <: Variable[A, DiscreteDomain]] (
		originalSize : Long,
		after : VariableT
		)
		: Double =
	{
		val domainsAfter = (
			after :: variables.toList.filter (_.name =/= after.name)
			).map (_.domain);
		val updatedEstimate = EstimatedSearchSpace (domainsAfter);
		
		1.0 - (updatedEstimate.toDouble / originalSize.toDouble);
	}
}
