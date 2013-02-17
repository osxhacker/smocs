/**
 * Created on: Feb 1, 2013
 */
package com.tubros.constraints.api
package solver

import scalaz._


/**
 * The '''Answer''' type embodies the results of running a
 * [[com.tubros.constraints.api.solver.Solver]] which satisfies the CSP given
 * to it.
 *
 * @author svickers
 *
 */
final case class Answer[A] (
	val name : VariableName,
	val value : A
	)
	
	
object Answer
{
	/// Class Imports
	import scalaz.std.tuple._
	import Isomorphism._
	
	
	/// Class Types
	type VariableNameTuple[A] = (VariableName, A)
	
	object AnswerIsoFunctor
		extends IsoFunctorTemplate[Answer, VariableNameTuple]
	{
		override def to[A] (fa : Answer[A]) = (fa.name, fa.value);
		override def from[A] (ga : VariableNameTuple[A]) =
			Answer[A] (ga._1, ga._2);
	}
	
	
	def apply[A] (pair : (Symbol, A)) : Answer[A] =
		new Answer (pair._1, pair._2);
	
	
	private def answerIso[A] : IsoSet[Answer[A], VariableNameTuple[A]] =
		new IsoSet[Answer[A], VariableNameTuple[A]] {
			override def from = (p : VariableNameTuple[A]) => Answer (p);
			override def to = (Answer.unapply[A] _) andThen (_.get);
			}
	
	
	/// Implicit Conversions
	implicit object AnswerComonad
		extends IsomorphismComonad[Answer, VariableNameTuple]
			with IsomorphismFunctor[Answer, VariableNameTuple]
	{
		override val G = implicitly[Comonad[VariableNameTuple]];
		override val iso : Answer <~> VariableNameTuple = AnswerIsoFunctor;
		
		override def cobind[A, B] (fa : Answer[A])
			(f : Answer[A] => B)
			: Answer[B] = Answer (fa.name, f (fa));
	}
	
	
	implicit def answerEqual[A : Equal] : Equal[Answer[A]] =
		new IsomorphismEqual[Answer[A], VariableNameTuple[A]] {
			override val G = implicitly[Equal[VariableNameTuple[A]]];
			override val iso : Answer[A] <=> VariableNameTuple[A] =
				answerIso[A];
			}
}
