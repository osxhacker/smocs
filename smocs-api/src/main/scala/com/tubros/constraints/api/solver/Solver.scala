/**
 * Created on: Jan 25, 2013
 */
package com.tubros.constraints.api
package solver

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

import scalaz._

import error.SolverError
import problem.{
	Equation,
	Problem
	}


/**
 * The '''Solver''' type defines a generic interface for running a CPS solution
 * algorithm. 
 *
 * @author svickers
 *
 */
abstract class Solver[A, M[+_] : Monad, +SolverT <: Solver[A, M, SolverT]]
{
	/// Class Types
	type DomainType[T] <: Domain[T]
	
	
	/**
	 * The add method internalizes an '''equation''' for use when producing
	 * the solution.  Any number of
	 * [[com.tubros.constraints.api.problem.Equation]]s can be provided through
	 * this method.
	 */
	def add (equation : Equation[A]) : M[Unit];
	
	
	/**
	 * This add method allows collaborators to provide a '''problem'' to use
	 * when solving the CSP.
	 */
	def add (problem : Problem[A]) : M[Unit];
	
	
	/**
	 * The apply method is the entry point into solving the CSP.  By having
	 * the syntax of:
	 * 
	 * {{{
	 * 		val answers = solver (s => defineConstraints (s));
	 * }}}
	 * 
	 * Concrete implementations can perform whatever actions ''they'' deem fit
	 * before and after the '''Solver''' is defined.
	 */
	def apply[C[_]] (context : SolverT => M[Stream[C[Answer[A]]]])
		: SolverError \/ Stream[C[Answer[A]]];
	
	
	/**
	 * This impose method allows '''Solver''' clients to enforce a global
	 * requirement, applicable to all
	 * [[com.tubros.constraints.api.solver.Variable]]s in the CSP.
	 */
	def impose[C[_]] (constraint : C[A] => Boolean)
		(implicit cbf : CanBuildFrom[Nothing, A, C[A]])
		: M[Unit];
	
	
	/**
	 * This version of the impose method allows '''Solver''' clients to use
	 * global requirements of the form:
	 * 
	 * {{{
	 * 		val myConstraint : PartialFunction[Seq[Answer[SomeType]], Boolean] =
	 *   	{
	 *   		case ArrayVariables (first, second, third) =>
	 *     			...
	 *        
	 *        	case ScalarNamed ('foo, v1) :: _ :: ScalarNamed ('bar, v2) =>
	 *         		...
	 *   	}
	 * }}}
	 */
	def impose (constraint : PartialFunction[Seq[Answer[A]], Boolean])
		: M[Unit];
	
	
	/**
	 * Similar to the `newVar` method, the newArrayVar method creates a new
	 * [[com.tubros.constraints.api.solver.Variable]] having the unique
	 * '''name''' given and a specific '''domain''' of values it can take.
	 * Furthermore, it allows for resolution of array-like syntax as defined
	 * in the [[com.tubros.constraints.api.problem.Equation]] class.
	 */
	def newArrayVar (name : VariableName, size : Int, domain : DomainType[A])
		: M[List[Variable[A, DomainType]]];
	
	
	/**
	 * The newVar method creates a new
	 * [[com.tubros.constraints.api.solver.Variable]] having the unique
	 * '''name''' given and a specific '''domain''' of values it can take.
	 * 
	 * Note that creation is done ''through the monad M'' and may not actually
	 * happen at the point of invocation.
	 */
	def newVar (name : VariableName, domain : DomainType[A])
		: M[Variable[A, DomainType]];
	
	
	/**
	 * The newVars method is a convenience method for creating a [[scala.List]]
	 * of [[com.tubros.constraints.api.solver.Variable]]s all having the same
	 * '''domain''.
	 */
	def newVars[C[_]] (domain : DomainType[A])
		(names : C[VariableName])
		(implicit F : Foldable[C])
		: M[List[Variable[A, DomainType]]];


	/**
	 * The run method evaluates the '''Solver''' and produces a
	 * [[scala.collection.immutable.Stream]] of satisfactory results.
	 */
	def run[C[_]] (implicit mo : Monoid[C[Answer[A]]], a : Applicative[C])
		: M[Stream[C[Answer[A]]]];
}
