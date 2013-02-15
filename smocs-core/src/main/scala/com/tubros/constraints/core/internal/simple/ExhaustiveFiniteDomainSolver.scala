/**
 * Created on: Jan 31, 2013
 */
package com.tubros.constraints.core.internal.simple

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
			constrained <- applyConstraints
			answers <- label (constrained)
			} yield answers;
			
	
	private def applyConstraints ()
		: SolverState[Stream[Map[VariableName, A]]] =
		gets {
			vs =>
				
			val streams = vs.variables.view.to[Stream].map (_.enumerate.to[Stream]);
			val c = vs.constraints.foldLeft (Constraint.kleisliUnit[A]) {
				case (accum, c) =>
					
				accum >==> c;
				}
			
			// TODO: this needs to return SolverError \/ Stream
			streams.sequence.map (_.toMap).filter {
				candidate =>
					
				c.run (candidate).isRight;
				}
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
}


object ExhaustiveFiniteDomainSolver
{
	/// Class Imports
	import scalaz.std.AllInstances._
	import scalaz.std.AllFunctions._
	import scalaz.syntax.monoid._
	
	
	/// Class Types
	case class VariableStore[A] (
		variables : Set[Variable[A, DiscreteDomain]],
		constraints : Set[Constraint[A]]
		)
	{
		def addConstraint (entry : Constraint[A]) =
			copy (constraints = constraints + entry);
			
		def addConstraints (entries : Seq[Constraint[A]]) =
			copy (constraints = constraints ++ entries);
			
		def addVariable (entry : DiscreteVariable[A]) =
			copy (variables = variables + entry);
		
		def addVariables (entries : Seq[DiscreteVariable[A]]) =
			copy (variables = variables ++ entries);
	}
	
	object VariableStore
	{
		def empty[A] = new VariableStore[A] (
			variables = Set.empty,
			constraints = Set.empty
			);
	}
}
