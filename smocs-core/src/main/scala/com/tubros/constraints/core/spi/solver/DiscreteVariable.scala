/**
 * Created on: Feb 1, 2013
 */
package com.tubros.constraints.core.spi.solver

import scalaz._

import com.tubros.constraints.api.VariableName
import com.tubros.constraints.api.solver.{
	DiscreteDomain,
	Variable
	}


/**
 * The '''DiscreteVariable''' type provides a general-case implementation of
 * the [[com.tubros.constraints.api.solver.Variable]] `trait` for use when
 * implementing [[com.tubros.constraints.api.solver.Solver]]s.  It should only
 * be used where it makes sense and concrete
 * [[com.tubros.constraints.api.solver.Solver]]s are free to define their own
 * implementations as they see fit.
 *
 * @author svickers
 *
 */
final case class DiscreteVariable[A] (
	override val name : VariableName,
	override val domain : DiscreteDomain[A]
	)
	extends Variable[A, DiscreteDomain]
{
	override def filter (predicate : A => Boolean) : DiscreteVariable[A] =
		new DiscreteVariable (name, domain.filter (predicate));
	
	
	override def flatMap[B] (
		f : (VariableName, DiscreteDomain[A]) => Variable[B, DiscreteDomain]
		)
		: DiscreteVariable[B] =
	{
		val result = f (name, domain);
		
		return (
			new DiscreteVariable[B] (
				name = result.name,
				domain = result.domain
				)
			);
	}
	
	
	override def map[B] (f : A => B) : DiscreteVariable[B] =
		new DiscreteVariable[B] (name = this.name, domain = domain.map (f));
}


object DiscreteVariable
{
	def apply[A] (name : VariableName) : DiscreteVariable[A] =
		new DiscreteVariable (name, DiscreteDomain.empty[A]);
	
	
	/// Implicit Conversions
	implicit def DiscreteVariableFunctor : Functor[DiscreteVariable] =
		new Functor[DiscreteVariable] {
			override def map[A, B] (fa : DiscreteVariable[A])
				(f : A => B)
				: DiscreteVariable[B] =
				fa.map (f);
			}
}