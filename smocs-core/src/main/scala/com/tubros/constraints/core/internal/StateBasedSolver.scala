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


/**
 * The '''StateBasedSolver''' type captures behavior common to all
 * [[com.tubros.constraints.api.solver.Solver]]s which use [[scalaz.State]]
 * as their implementation [[scalaz.Monad]].
 *
 * @author svickers
 *
 */
trait StateBasedSolver[A, M[+_], +SolverT <: Solver[A, M, SolverT]]
{
	/// Self Type Constraints
	this : Solver[A, M, SolverT] =>
		

	/// Class Imports
	import constraints._
	import scalaz.std.list._
	import scalaz.syntax.applicative._
	import scalaz.syntax.id._
	import scalaz.syntax.traverse._
	import State._
	
	
	/// Class Types
	override type DomainType[T] = DiscreteDomain[T]
	type SolverState[+T] = ({ type L[+X] = State[VariableStore[A], X]})#L[T]
	
	
	/// Instance Properties
	implicit def canConstrain : CanConstrain[Equation, A];
	
	
	override def add (equation : Equation[A]) : SolverState[Unit] =
		modify {
			_.addConstraint (equation.constrains);
			}
	
	
	override def add (problem : Problem[A]) : SolverState[Unit] =
		problem.equations.traverseS (add) map (_ => ());
	
	
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
}
