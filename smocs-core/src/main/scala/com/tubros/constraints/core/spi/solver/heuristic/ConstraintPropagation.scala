/**
 * Created on: Feb 28, 2013
 */
package com.tubros.constraints.core.spi.solver.heuristic

import scala.language.higherKinds

import scalaz._
import scalaz.iteratee._

import com.tubros.constraints.api.VariableName
import com.tubros.constraints.api.solver._
import com.tubros.constraints.core.spi.solver._

import runtime._


/**
 * The '''ConstraintPropagation''' type is a functor which reduces the
 * [[com.tubros.constraints.api.solver.Domain]] of a candidate
 * [[com.tubros.constraints.api.solver.Variable]] based on the
 * [[com.tubros.constraints.core.spi.solver.Constraint]]s ''applicable'' to
 * the available (known) variables.
 *
 * @author svickers
 *
 */
final case class ConstraintPropagation[A, DomainT[X] <: Domain[X]] (
	private val provider : ConstraintProvider[A],
	private val symbolTable : SymbolTable
	)
	extends ((Iterable[Answer[A]], Variable[A, DomainT]) => Variable[A, DomainT])
{
	override def apply (
		assignments : Iterable[Answer[A]],
		variable : Variable[A, DomainT]
		)
		: Variable[A, DomainT] =
	{
		val available = assignments.map (_.name).to[Set] + variable.name;
		val applicableConstraints = provider.constraintsFor (available);
		val priorVariables = evaluate (
			assignments.map (_.toTuple).toMap,
			variable.name
			) _;
		
		variable.filter {
			value =>
				
			applicableConstraints.forall (priorVariables (value));
			}
	}
	
	
	def apply (variable : Variable[A, DomainT]) : Variable[A, DomainT] =
		apply (Seq.empty[Answer[A]], variable);
	
	
	private def evaluate (variables : Map[VariableName, A], name : VariableName)
		(value : A)
		(constraint : Constraint[A])
		: Boolean =
		constraint (variables updated (name, value)).isRight;
}


final case class ConstraintPropagationEnumeratee[
	A,
	DomainT[X] <: Domain[X],
	M[+_] : Monad
	] (
	val provider : ConstraintProvider[A] with SymbolTableProvider
	)
	extends EnumerateeT[Seq[Answer[A]], Seq[Answer[A]], M]
{
	/// Class Imports
	import syntax.std.option._
	import EnumerateeT.doneOr
	import Input._
	import IterateeT._
	import StepT._
	
	
	/// Class Types
	type ElementType = Seq[Answer[A]]
	type InnerIterateeType[T] = IterateeT[ElementType, M, T]
	type OuterIterateeType[T] = IterateeT[ElementType, M, StepType[T]]
	type StepType[T] = StepT[ElementType, M, T]
	
	
	override def apply[T] : StepType[T] => OuterIterateeType[T] =
	{
		def step : (Input[ElementType] => InnerIterateeType[T]) =>
			(Input[ElementType] => OuterIterateeType[T]) =
			k => in => in (
				el = e => {
					evaluate[T] (e) match {
						case Some (allowable) =>
							k (elInput (allowable)) >>== doneOr (loop)
							
						case None =>
							cont (step (k));
						}
					},
				empty = cont (step (k)),
				eof = done (scont (k), in)
				);
			
		def loop = step andThen cont[ElementType, M, StepType[T]];
		
		doneOr (loop);
	}
	
	
	@inline
	private def evaluate[T] (assignments : Seq[Answer[A]])
		: Option[Seq[Answer[A]]] =
	{
		val params = assignments.map (_.toTuple).toMap;
		val applicableConstraints = provider.constraintsFor (params.keySet);
		
		applicableConstraints.foldLeft (params.some) {
			(accum, constraint) =>
				
			accum.flatMap (p => constraint (p).toOption);
			}.map (_.map (Answer.fromTuple[A]).toSeq);
	}
}
