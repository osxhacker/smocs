/**
 * Created on: Feb 22, 2013
 */
package com.tubros.constraints.core.internal

import scalaz.{
	Ordering => _,
	_
	}

import com.tubros.constraints.api._
import com.tubros.constraints.core.spi.solver._

import problem._
import solver._

/**
 * The '''VariableStore''' type is an `internal` type used to manage
 * [[com.tubros.contraints.api.solver.Variable]]s and the
 * [[com.tubros.constraints.core.spi.solver.Constraint]]s placed on them for
 * various [[com.tubros.contraints.api.solver.Solver]]s.
 *
 * @author svickers
 *
 */
final case class VariableStore[A] (
	variables : Vector[Variable[A, DiscreteDomain]],
	constraints : Set[Constraint[A]],
	answerFilters : Set[Constraint[A]]
	)
{
	def addAnswerFilter (entry : Constraint[A]) =
		copy (answerFilters = answerFilters + entry);
	
	def addConstraint (entry : Constraint[A]) =
		copy (constraints = constraints + entry);
		
	def addVariable (entry : DiscreteVariable[A]) =
		copy (variables = variables :+ entry);
	
	def addVariables (entries : Seq[DiscreteVariable[A]]) =
		copy (variables = variables ++ entries);
}


object VariableStore
{
	/// Class Types
	class AnswerOrdering[A] (store : VariableStore[A])
		extends Ordering[Answer[A]]
	{
		/// Class Imports
		import std.tuple._
		import syntax.bifunctor._
		
		
		/// Instance Properties
		private val positions : Map[VariableName, Int] =
			store.variables.zipWithIndex.map {
				paired =>
					
				((_ : Variable[A, DiscreteDomain]).name) <-: paired;
				}.toMap;
		
		
		override def compare (x : Answer[A], y : Answer[A]) : Int =
			positions (x.name) - positions (y.name);
	}
	
	
	/**
	 * The empty method is a model of the FACTORY pattern for creating
	 * '''VariableStore''' instances having no contents.
	 */
	def empty[A] = new VariableStore[A] (
		variables = Vector.empty,
		constraints = Set.empty,
		answerFilters = Set.empty
		);
}
