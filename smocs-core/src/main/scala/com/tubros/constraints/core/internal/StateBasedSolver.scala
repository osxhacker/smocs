/**
 * Created on: Feb 22, 2013
 */
package com.tubros.constraints.core.internal

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

import scalaz._

import com.tubros.constraints.api._
import com.tubros.constraints.core.spi.solver._

import problem._
import solver._
import solver.error._


/**
 * The '''StateBasedSolver''' type captures behavior common to all
 * [[com.tubros.constraints.api.solver.Solver]]s which use [[scalaz.State]]
 * as their implementation [[scalaz.Monad]].
 *
 * @author svickers
 *
 */
trait StateBasedSolver[
	A,
	+SolverT <: Solver[A, ({ type L[+X] = State[VariableStore[A], X]})#L, SolverT]
	]
	extends ArrayNamingPolicy
{
	/// Self Type Constraints
	self : SolverT =>
		

	/// Class Imports
	import constraints._
	import scalaz.std.list._
	import scalaz.syntax.applicative._
	import scalaz.syntax.id._
	import scalaz.syntax.traverse._
	import State._
	
	
	/// Class Types
	override type DomainType[T] = DiscreteDomain[T]
	type Map[K, +V] = scala.collection.Map[K, V]
	type SolverState[+T] = ({ type L[+X] = State[VariableStore[A], X]})#L[T]
	
	
	/// Instance Properties
	implicit def canConstrain : CanConstrain[Equation, A];
	
	
	override def add (equation : Equation[A]) : SolverState[Unit] =
		modify {
			_.addConstraint (equation.constrains);
			}
	
	
	override def add (problem : Problem[A]) : SolverState[Unit] =
		problem.equations.traverseS (add) map (_ => ());
	
	
	override def apply[C[_]] (
		context : SolverT => SolverState[SolverError \/ Stream[C[Answer[A]]]])
		: SolverError \/ Stream[C[Answer[A]]] =
		context (self).eval (VariableStore.empty[A]);
		
		
	override def impose[C[_]] (constraint : C[A] => Boolean)
		(implicit cbf : CanBuildFrom[Nothing, A, C[A]])
		: SolverState[Unit] =
		modify {
			_.addAnswerFilter (new AnswerValueConstraint (constraint));
			}
	
	
	override def impose (constraint : PartialFunction[Seq[Answer[A]], Boolean])
		: SolverState[Unit] =
		modify {
			_.addAnswerFilter (new AnswerConstraint (constraint));
			}
	
	
	override def newArrayVar (
		name : VariableName,
		size : Int,
		domain : DomainType[A]
		)
		: SolverState[List[Variable[A, DomainType]]] =
		State {
			vs =>
				
			val array = List.tabulate (size) {
				index =>
					
				DiscreteVariable[A] (compose (name, index), domain);
				}
			
			(vs.addVariables (array), array);
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
}
