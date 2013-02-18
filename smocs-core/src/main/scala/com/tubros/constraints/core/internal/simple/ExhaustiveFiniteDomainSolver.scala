/**
 * Created on: Jan 31, 2013
 */
package com.tubros.constraints.core.internal.simple

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.LinkedHashMap
import scala.language.higherKinds

import scalaz._

import com.tubros.constraints.api._
import com.tubros.constraints.core.spi.solver._

import problem.{
	Equation,
	Problem
	}
import solver._
import solver.error._
import ExhaustiveFiniteDomainSolver._


/**
 * The '''ExhaustiveFiniteDomainSolver''' type is a
 * [[com.tubros.constraints.api.solver.Solver]] which searches the ''entire''
 * solution space for all solutions satisfying the CSP constraints.  Since
 * this type of solver can take exponential resources, it should only be used
 * for ''very'' simple problem domains.
 *
 * @author svickers
 *
 */
class ExhaustiveFiniteDomainSolver[A]
	(implicit val cc : CanConstrain[Equation, A])
	extends Solver[
		A,
		({ type L[+X] = State[VariableStore[A], X]})#L,
		ExhaustiveFiniteDomainSolver[A]
		]
{
	/// Class Imports
	import constraints._
	import scalaz.std.list._
	import scalaz.std.stream._
	import scalaz.syntax.applicative._
	import scalaz.syntax.id._
	import scalaz.syntax.traverse._
	import State._
	
	
	/// Class Types
	type DomainType[T] = DiscreteDomain[T]
	type Map[K, +V] = scala.collection.Map[K, V]
	type SolverState[+T] = ({ type L[+X] = State[VariableStore[A], X]})#L[T]
	
	
	override def add (equation : Equation[A]) : SolverState[Unit] =
		modify {
			_.addConstraint (equation.constrains);
			}
	
	
	override def add (problem : Problem[A]) : SolverState[Unit] =
		problem.equations.traverseS (add) map (_ => ());
	
	
	override def apply[C[_]] (
		context : ExhaustiveFiniteDomainSolver[A] =>
			SolverState[Stream[C[Answer[A]]]]
		)
		: Stream[C[Answer[A]]] =
		context (this).eval (VariableStore.empty[A]);
	
	
	override def impose[C[_]] (constraint : C[A] => Boolean)
		(implicit cbf : CanBuildFrom[Nothing, A, C[A]])
		: SolverState[Unit] =
		modify {
			_.addAnswerFilter (new AnswerConstraint (constraint));
			}
	
	
	override def newVar (name : VariableName, domain : DomainType[A])
		: SolverState[Variable[A, DomainType]] =
		State {
			vs =>
				
			val variable = DiscreteVariable (name, domain);
			
			(vs.addVariable (variable), variable);
			}
	
	
	override def newVars[C[_]] (domain : DomainType[A])
		(names : C[VariableName])
		(implicit F : Foldable[C])
		: SolverState[List[Variable[A, DomainType]]] =
		State {
			vs =>
				
			val created = F.foldMap (names) {
				name =>
					
				List (DiscreteVariable (name, domain));
				}
			
			(vs.addVariables (created), created);
			}
		
		
	/**
	 * The '''ExhaustiveFiniteDomainSolver''' version of the run method
	 * simply produces the combination of all remaining
	 * [[com.tubros.constraints.core.spi.solver.DiscreteVariable]]s if and only
	 * if ''all'' have at least one value remaining in their
	 * [[com.tubros.constraints.api.solver.DiscreteDomain]].
	 */
	override def run[C[_]] (
		implicit mo : Monoid[C[Answer[A]]],
		a : Applicative[C]
		)
		: SolverState[Stream[C[Answer[A]]]] =
		for {
			vars <- variables ()
			filters <- filterAnswers ()
			constrained <- applyConstraints (vars, filters)
			answers <- label (constrained)
			} yield answers;
	
	
	private def applyConstraints (
		vars : Seq[Variable[A, DomainType]],
		filters : Seq[Constraint[A]]
		)
		: SolverState[Stream[Map[VariableName, A]]] =
		gets {
			vs =>
				
			val streams = vars.view.to[Stream].map (_.enumerate.to[Stream]);
			val c = filters.foldLeft (Constraint.kleisliUnit[A]) {
				case (accum, c) =>
					
				accum >==> c;
				}
			
			// TODO: this needs to return SolverError \/ Stream
			streams.sequence.map (LinkedHashMap.apply).filter {
				candidate =>
					
				c.run (candidate.toMap).isRight;
				}
			}
			
	
	private def filterAnswers ()
		: SolverState[Seq[Constraint[A]]] =
		gets {
			vs =>
				
			vs.answerFilters.toVector ++ vs.constraints;
			}
	
	
	private def label[C[_]] (variables : Stream[Map[VariableName, A]])
		(implicit mo : Monoid[C[Answer[A]]], a : Applicative[C])
		: SolverState[Stream[C[Answer[A]]]] =
		state {
			val answers = variables.map {
				_.to[List].foldMap {
					cur =>
						
					Answer (cur).point[C];
					}
				}
			
			answers;
			}
			
	
	private def variables ()
		: SolverState[Seq[Variable[A, DomainType]]] =
		gets {
			_.variables;
			}
}


object ExhaustiveFiniteDomainSolver
{
	/// Class Imports
	import scalaz.std.AllInstances._
	import scalaz.std.AllFunctions._
	import scalaz.syntax.monoid._
	
	
	/// Class Types
	case class VariableStore[A] (
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
		def empty[A] = new VariableStore[A] (
			variables = Vector.empty,
			constraints = Set.empty,
			answerFilters = Set.empty
			);
	}
}
