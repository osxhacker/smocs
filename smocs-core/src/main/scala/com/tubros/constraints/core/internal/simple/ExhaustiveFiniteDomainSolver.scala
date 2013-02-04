/**
 * Created on: Jan 31, 2013
 */
package com.tubros.constraints.core.internal.simple

import scala.language.higherKinds

import scalaz._

import com.tubros.constraints.api._
import com.tubros.constraints.core.spi.solver._

import problem.Equation
import solver._
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
	extends Solver[
		A,
		({ type L[+X] = State[VariableStore[A], X]})#L,
		ExhaustiveFiniteDomainSolver[A]
		]
{
	/// Class Imports
	import scalaz.std.list._
	import scalaz.syntax.applicative._
	import scalaz.syntax.traverse._
	import State._
	
	
	/// Class Types
	type DomainType[T] = DiscreteDomain[T]
	
	type SolverState[+T] = ({ type L[+X] = State[VariableStore[A], X]})#L[T]
	
	
	override def add[T <: this.Constraint] (constraint : T)
		: SolverState[Unit] =
		modify {
			case vs =>
				
			// TODO: Implement constraint creation
			vs;
			}
	
	
	override def add[F[_]] (equation : F[Equation])
		(implicit A : Applicative[F])
		: SolverState[Unit] =
		modify {
			case vs =>
				
			vs;
			}
	
	
	override def apply[C[_]] (
		context : ExhaustiveFiniteDomainSolver[A] =>
			SolverState[Stream[C[Answer[A]]]]
		)
		: Stream[C[Answer[A]]] =
	{
		context (this).eval (VariableStore[A] (Map.empty));
	}
	
	
	override def newVar (name : VariableName, domain : DomainType[A])
		: SolverState[Variable[A, DomainType]] =
		State {
			case vs =>
				
			val variable = DiscreteVariable (name, domain);
			
			(vs + variable, variable);
			}
	
	
	override def newVars[C[_]] (domain : DomainType[A])
		(names : C[VariableName])
		(implicit F : Foldable[C])
		: SolverState[List[Variable[A, DomainType]]] =
		State {
			case vs =>
				
			val created = F.foldMap (names) {
				name =>
					
				List (DiscreteVariable (name, domain));
				}
			
			(vs ++ created, created);
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
		gets {
			vs =>
				
			// TODO: This is horribly inefficient!
			val lists = vs.variables.values.to[List].map (_.enumerate.toList).sequence;
			val answers = lists.map {
				answer =>
					
				answer.foldLeft (mo.zero) {
					case (accum, cur) =>
						
					mo.append (accum, Answer (cur).point[C]);
					}
				}
			
			answers.toStream;
			}
}


object ExhaustiveFiniteDomainSolver
{
	/// Class Types
	case class VariableStore[A] (
		variables : Map[VariableName, Variable[A, DiscreteDomain]]
		)
	{
		def + (entry : DiscreteVariable[A]) =
			copy (variables = variables + (entry.name -> entry));
		
		def ++ (entries : Seq[DiscreteVariable[A]]) =
			copy (variables = variables ++ (entries map (e => (e.name -> e))));
	}
}