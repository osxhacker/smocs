/**
 * Created on: Jan 30, 2013
 */
package com.tubros.constraints.api
package solver

import scala.language.{
	higherKinds,
	implicitConversions
	}

import scalaz._


/**
 * The '''Variable''' type defines the API contract all types representing a
 * [[com.tubros.constraints.api.solver.Domain]]-constrained symbolic variable
 * must satisfy in order to be able to minimally participate in the evaluation
 * of a [[com.tubros.constraints.api.solver.Solver]].  This largely concerns
 * identification of ''which'' variable an instance represents (provided by
 * its `name`) and what [[com.tubros.constraints.api.solver.Domain]] of values
 * it can take on.
 * 
 * To assist in `for` comprehension use, '''Variable''' is ''domain-leaning''
 * in its `map' operation.  However, situations which call for `map`ping with
 * both the [[com.tubros.constraints.api.VariableName]] and
 * [[com.tubros.constraints.api.solver.Domain]] can use the `withName` method.
 *
 * @author svickers
 *
 */
trait Variable[A, DomainT[A] <: Domain[A]]
{
	/// Instance Properties
	/**
	 * The name property uniquely identifies the '''Variable''' by providing
	 * its symbolic [[com.tubros.constraints.api.VariableName]].
	 */
	def name : VariableName;
	
	/**
	 * Each '''Variable''' has a domain of values which it can possibly
	 * represent.
	 */
	def domain : DomainT[A];
	
	/**
	 * The enumerate property provides a [[scala.collection.Iterator]] which
	 * contains the '''name''' and each '''domain''' value paired as a
	 * [[scala.Tuple2]].
	 */
	lazy val enumerate : Iterable[(VariableName, A)] = domain.map {
		(name, _)
		};
	
	
	/**
	 * The filter method allows all '''domain''' values to which the
	 * given '''predicate''' yields `true` into the resulting '''Variable'''.
	 */
	def filter (predicate : A => Boolean) : Variable[A, DomainT];
	
	
	/**
	 * The flatMap method builds a new '''Variable''' by applying a function
	 * to the [[com.tubros.constraints.api.VariableName]] and
	 * [[com.tubros.constraints.api.solver.Domain]] pair.
	 */
	def flatMap[B] (
		f : (VariableName, DomainT[A]) => Variable[B, DomainT]
		)
		: Variable[B, DomainT];
	
	
	/**
	 * The map method applies a ''domain-leaning'' function to this
	 * '''Variable'' in order to produce a new '''Variable''' instance.
	 */
	def map[B] (f : A => B) : Variable[B, DomainT];
}


object Variable
{
	/// Implicit Conversions
	/**
	 * For situations where a `map` needs both the
	 * [[com.tubros.constraints.api.solver.Domain]] and
	 * [[com.tubros.constraints.api.VariableName]], withName can be used to
	 * produce a [[WithName]] instance having the requisite `map` functor
	 * signature.
	 */
	implicit class WithName[
		A,
		DomainT[A] <: Domain[A],
		V <: Variable[A, DomainT]
		] (val variable : V)
		{
			def withName = new {
				def map[B] (f : (VariableName, A) => B)
					: Variable[B, DomainT] =
					variable.map (f (variable.name, _));
				}
		}
}
