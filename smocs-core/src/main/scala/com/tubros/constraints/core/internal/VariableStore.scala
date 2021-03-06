/**
 * Created on: Feb 22, 2013
 */
package com.tubros.constraints.core.internal

import scala.collection.mutable.LinkedHashSet

import scalaz.{
	Ordering => _,
	_
	}

import com.tubros.constraints.api._
import com.tubros.constraints.api.solver._
import com.tubros.constraints.core.spi.solver._

import error.SolverError
import problem._
import runtime._


/**
 * The '''VariableStore''' type is an `internal` type used to manage
 * [[com.tubros.constraints.api.solver.Variable]]s and the
 * [[com.tubros.constraints.core.spi.solver.Constraint]]s placed on them for
 * various [[com.tubros.constraints.api.solver.Solver]]s.
 *
 * @author svickers
 *
 */
final case class VariableStore[A] (
	val variables : Vector[Variable[A, DiscreteDomain]],
	override val constraints : Set[Constraint[A]],
	val answerFilters : Set[Constraint[A]],
	override val symbols : SymbolTable
	)
	extends ConstraintProvider[A]
		with SymbolTableProvider
{
	/// Class Imports
	import std.vector._
	
	
	override def constraintsFor (available : Set[VariableName])
		: Set[Constraint[A]] =
	{
		val atomicDefinitions = available flatMap (symbols.apply);
		val resolved = atomicDefinitions ++ symbols.derivedFrom (
			atomicDefinitions
			);
		val (producers, constrainers) =
			constraints.filter (_.isDefinedAt (resolved)).partition {
				_.derived.isDefined;
				}
		
		return ((LinkedHashSet.empty ++ producers ++ constrainers).toSet);
	}
	
		
	override def globalConstraints ()
		: Kleisli[
			({ type L[+A] = SolverError \/ A})#L,
			Constraint[A]#Env[A],
			Constraint[A]#Env[A]
			] =
		Constraint.chained (answerFilters.to[Vector]);
	
	
	def addAnswerFilter (entry : Constraint[A]) =
		copy (answerFilters = answerFilters + entry);
	
	
	def addConstraint (entry : Constraint[A]) =
		copy (constraints = constraints + entry);
		
	
	def addVariable (entry : DiscreteVariable[A]) =
		copy (
			variables = variables :+ entry,
			symbols = symbols addSymbol (entry.name)
			);
	
	
	def addVariables (entries : Seq[DiscreteVariable[A]]) =
		copy (
			variables = variables ++ entries,
			symbols = entries.foldLeft (symbols) {
				case (st, entry) =>
					
				st addSymbol (entry.name);
				}
			);
	

	def defining (
		derived : VariableName,
		participants : Traversable[VariableName]
		) =
		copy (symbols = symbols addDerivedSymbol (derived, participants.toSet));
	
	
	def ordering : Ordering[Answer[A]] = 
		new VariableStore.AnswerOrdering[A] (this);
}


object VariableStore
{
	/// Class Types
	class AnswerOrdering[A] (store : VariableStore[A])
		extends Ordering[Answer[A]]
	{
		override def compare (x : Answer[A], y : Answer[A]) : Int =
			VariableNameOrdering.compare (x.name, y.name);
	}
	
	
	implicit object VariableNameOrdering
		extends Ordering[VariableName]
	{
		override def compare (x : VariableName, y : VariableName) : Int =
			x.name.compareTo (y.name);
	}
	
	
	/**
	 * The empty method is a model of the FACTORY pattern for creating
	 * '''VariableStore''' instances having no contents.
	 */
	def empty[A] = new VariableStore[A] (
		variables = Vector.empty,
		constraints = Set.empty,
		answerFilters = Set.empty,
		symbols = SymbolTable.empty
		);
}
