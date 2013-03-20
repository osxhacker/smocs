/**
 * Created on: Feb 1, 2013
 */
package com.tubros.constraints.api
package solver

import scala.language.{
	higherKinds,
	postfixOps
	}

import scalaz._

import problem.ArrayNamingPolicy


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
{
	def toTuple = Answer.AnswerIsoFunctor.to (this);
}
	
	
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
	
	
	/**
	 * The fromTuple method is an alias for the `apply` method.
	 */
	def fromTuple[A] (pair : (Symbol, A)) : Answer[A] =
		apply[A] (pair);
	
	
	private def answerIso[A] : IsoSet[Answer[A], VariableNameTuple[A]] =
		new IsoSet[Answer[A], VariableNameTuple[A]] {
			override def from = (p : VariableNameTuple[A]) => fromTuple (p);
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
	
	
	implicit def answerShow[A : Show] : Show[Answer[A]] =
		new Show[Answer[A]]
			with ArrayNamingPolicy
			{
			override def shows (a : Answer[A]) =
				"Answer(%s,%s)".format (
					decompose (a.name).fold (a.name.toString) {
						case (root, index) =>
							
						"%s(%s)".format (root.name, index);
						},
					a.value
					);
			}
}


/**
 * The '''ScalarNamed''' extractor can be used with [[scala.PartialFunction]]s
 * to `match` an individual [[com.tubros.constraints.api.solver.Variable]]
 * when performing global constraints, such as:
 * 
 * {{{
 * 		_ match {
 *   		case ScalarNamed ('a, value) =>
 *				doSomethingWith (value);
 *     		}
 * }}}
 */
object ScalarNamed
	extends ArrayNamingPolicy
{
	/// Class Imports
	import syntax.equal._
	import syntax.std.boolean._
	
	
	def unapply[A] (answer : Answer[A]) : Option[(VariableName, A)] =
		unlessArrayName (answer.name) {
			name =>
				
			(name === answer.name) option (answer.toTuple);
			}
}


object ArrayVariables
	extends ArrayNamingPolicy
{
	def unapplySeq[A] (answers : Seq[Answer[A]]) : Option[Seq[Map[Int, A]]] =
	{
		val arrays = answers.view.filter (a => isArrayName (a.name)).flatMap {
			answer =>
				
			decompose (answer.name) map (parts => (parts._1, parts._2, answer));
			}.groupBy {
				case (name, index, answer) =>
				
				name;
			}.mapValues (triplets => triplets.map(t => (t._2, t._3.value)));
			
		if (arrays.isEmpty)
			None
		else
			Some (arrays.values.view.toSeq.map (_.toMap));
	}
}
