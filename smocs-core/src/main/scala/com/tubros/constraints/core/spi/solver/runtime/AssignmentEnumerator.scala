/**
 * Created on: Mar 16, 2013
 */
package com.tubros.constraints.core.spi.solver
package runtime

import scala.language.{
	higherKinds,
	postfixOps
	}

import scalaz._
import scalaz.iteratee._

import com.tubros.constraints.api.VariableName
import com.tubros.constraints.api.solver._


/**
 * The '''AssignmentEnumerator''' type is responsible for creating
 * [[com.tubros.constraints.api.VariableName]] assignments based on both an
 * existing collection of ''AssignmentElementType'' instances and a
 * [[com.tubros.constraints.api.solver.Variable]] for which new assignments
 * are to be created.  Since derived values can result in 0+ assignments,
 * clients of '''AssignmentEnumerator''' must be prepared to receive any
 * number of ''AssignmentElementType'' objects produced by this abstraction.
 * 
 * Also, only the ''new'' assignments are produced for each value in the
 * given [[com.tubros.constraints.api.solver.Variable]] `domain`.  For example,
 * if the existing assignments consisted of:
 * 
 * {{{
 * 		val existing = Seq (('a -> 1), ('b -> 2), ('c -> 3));
 * }}}
 *
 * And the candidate [[com.tubros.constraints.api.solver.Variable]] had a name
 * of `xyz`, then each [[scala.collection.immutable.Seq]] produced would look
 * like:
 * 
 * {{{
 * 		val produced = Seq (('xyz -> N));
 * }}}
 * 
 * For some value ''N'' within the `domain` of `xyz`.  This is assuming that
 * there were no additional productions applicable in this example.  Were there
 * a production available to be generated, say having a name of `derived`,
 * then the produced assignment [[scala.collection.immutable.Seq]] instances
 * would look like (note that ordering is not guaranteed):
 * 
 * {{{
 * 		val producedWithExtra = Seq (('xyz -> N), ('derived -> M));
 * }}}
 * 
 * Where ''M'' is the value produced by the definition of `derived`.
 * 
 * @author svickers
 *
 */
case class AssignmentEnumerator[A, C[_]] (
	val steps : AssignmentEnumerator[A, C]#StepsType
	)
	(implicit
		A : Applicative[C],
		F : Foldable[C],
		MO : Monoid[C[Seq[AssignmentEnumerator[A, C]#AssignmentElementType]]]
	)
{
	/// Class Imports
	import syntax.applicative._
	import syntax.std.boolean._
	import syntax.foldable._
	import syntax.monoid._
	import AssignmentEnumerator._
	import EnumeratorT._
	import State._
	
	
	/// Class Types
	type AssignmentElementType = Answer[A]
	type ContextType = (Seq[AssignmentElementType], VariableType)
	type IterateeType[E, R] = IterateeT[E, StateType, R]
	type StepsType = IterateeType[
		Seq[AssignmentEnumerator[A, C]#AssignmentElementType],
		C[Seq[AssignmentEnumerator[A, C]#AssignmentElementType]]
		]
	type StateType[+T] = State[ContextType, T]
	type VariableType = Variable[A, DiscreteDomain]
	

	/// Instance Properties
	private lazy val operations : StateType[C[Seq[AssignmentElementType]]] =
		for {
			context <- init[ContextType]
			produced <- (steps &= enumContext[A, C] (context)).run
			newAssignments <- onlyNewAssignments (produced)
			} yield newAssignments;
			
			
	/**
	 * The generate method creates a ''C'' [[scalaz.Monoid]] having 0+ new
	 * assignment deltas.
	 */
	def generate (
		existingAssignments : Seq[AssignmentElementType],
		candidate : VariableType
		)
		: C[Seq[AssignmentElementType]] =
		operations.eval (existingAssignments -> candidate);


	def map (f : StepsType => StepsType) : AssignmentEnumerator[A, C] =
		copy (steps = f (steps));
	
	
	private def onlyNewAssignments (
		produced : C[Seq[AssignmentElementType]]
		)
		: StateType[C[Seq[AssignmentElementType]]] =
		gets {
			context =>
				
			produced.foldLeft (mzero[C[Seq[AssignmentElementType]]]) {
				case (c, assignments) =>
					
				val additional = assignments filterNot (context._1.contains);
				
				additional.isEmpty.fold (c, c |+| (additional).point[C]);
				}
			}
}


object AssignmentEnumerator
{
	/// Class Imports
	import std.stream._
	import EnumerateeT._
	
	
	/**
	 * This version of the apply method is provided to support functional-style
	 * creation where the client needs only the minimal behavior required.
	 */
	def apply[A, C[_]] ()
		(implicit
			A : Applicative[C],
			F : Foldable[C],
			MO : Monoid[C[Seq[AssignmentEnumerator[A, C]#AssignmentElementType]]]
		)
		: AssignmentEnumerator[A, C] =
		new AssignmentEnumerator[A, C] (
			steps = IterateeT.collectT[
				Seq[AssignmentEnumerator[A, C]#AssignmentElementType],
				AssignmentEnumerator[A, C]#StateType,
				C
				]
			);
			
			
	private def enumContext[A, C[_]] (
		context : AssignmentEnumerator[A, C]#ContextType
		) =
		EnumeratorT.enumStream[
			Seq[AssignmentEnumerator[A, C]#AssignmentElementType],
			AssignmentEnumerator[A, C]#StateType
			] (context._2.enumerate.map (
				e => context._1 :+ Answer.fromTuple (e)
				).toStream
				);
}
