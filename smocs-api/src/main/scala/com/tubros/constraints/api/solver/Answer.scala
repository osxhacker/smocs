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
	/**
	 * The canonical map method allows the application of an '''f'''unctor to
	 * the contained `value`, producing an `Answer[B]` having the same `name`.
	 */
	def map[B] (f : A => B) : Answer[B] = copy (value = f (value));
	
	
	def toTuple = Answer.AnswerIsoFunctor.to (this);
}
	
	
object Answer
{
	/// Class Imports
	import scalaz.syntax.show._
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
	
	
	def apply[A, B] (pair : (A, B))
		(implicit APC : AnswerPairConverter[A])
		: Answer[B] =
		APC.toAnswer (pair);
	
	
	/**
	 * The fromTuple method is an alias for the `apply` method.
	 */
	def fromTuple[A, B] (pair : (A, B))
		(implicit APC : AnswerPairConverter[A])
		: Answer[B] =
		apply[A, B] (pair);
	
	
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
	
	
	implicit def answerEqual[A : Equal] : Equal[Answer[A]] = Equal.equalA;
	
	
	implicit def answerShow[A : Show] : Show[Answer[A]] =
		new Show[Answer[A]]
			with ArrayNamingPolicy
			{
			override def shows (a : Answer[A]) =
				"(%s -> %s)".format (
					decompose (a.name).fold (a.name.toString) {
						case (root, index) =>
							
						"%s(%s)".format (root.name, index);
						},
					a.value.shows
					);
			}
}


/**
 * The '''AnswerPairConverter''' type is a model of the TYPE CLASS pattern and
 * provides transparent conversion from supported `A` types to an
 * [[com.tubros.constraints.api.solver.Answer]] instance.
 * 
 * @author svickers
 */
sealed trait AnswerPairConverter[A]
{
	def toAnswer[B] (pair : (A, B)) : Answer[B];
}


object AnswerPairConverter
{
	/// Implicit Conversions
	implicit object FastSymbolAnswerConverter
		extends AnswerPairConverter[FastSymbol]
	{
		override def toAnswer[B] (pair : (FastSymbol, B)) : Answer[B] =
			new Answer[B] (pair._1, pair._2);
	}
	
	
	implicit object SymbolAnswerConverter
		extends AnswerPairConverter[Symbol]
	{
		override def toAnswer[B] (pair : (Symbol, B)) : Answer[B] =
			new Answer[B] (FastSymbol (pair._1.name), pair._2);
	}
	
	
	implicit object StringAnswerConverter
		extends AnswerPairConverter[String]
	{
		override def toAnswer[B] (pair : (String, B)) : Answer[B] =
			new Answer[B] (FastSymbol (pair._1), pair._2);
	}
}


/**
 * The '''ScalarNamed''' extractor can be used with [[scala.PartialFunction]]s
 * to `match` an individual [[com.tubros.constraints.api.solver.Variable]]
 * when performing global constraints, such as:
 * 
 * {{{
 * 		_ match {
 *   		case ScalarNamed ("a", value) =>
 *				doSomethingWith (value);
 *     		}
 * }}}
 *
 * @author svickers
 *
 */
object ScalarNamed
	extends ArrayNamingPolicy
{
	/// Class Imports
	import Scalaz._
	
	
	def unapply[A] (answer : Answer[A]) : Option[(String, A)] =
		unlessArrayName (answer.name) {
			name =>
				
			(name === answer.name) option {
				((_ : FastSymbol).name) <-: answer.toTuple;
				}
			}
}


sealed trait ArraysToMap
	extends ArrayNamingPolicy
{
	protected def arrays[A] (answers : Seq[Answer[A]]) =
	{
		answers.view.filter (a => isArrayName (a.name)).flatMap {
			answer =>
				
			decompose (answer.name) map (parts => (parts._1, parts._2, answer));
			}.groupBy {
				case (name, index, answer) =>
				
				name;
			}.mapValues (triplets => triplets.map (t => (t._2, t._3.value)));
	}
}


/**
 * The '''ArrayVariables''' extractor produces a `Seq[Map[Int, A]]` representing
 * all array variables and their values, in ascending
 * [[com.tubros.constraints.api.VariableName]] order.  Each `Map[Int, A]` has
 * the array subscript as its key and the value for the variable as the map's
 * value.
 *
 * @author svickers
 *
 */
object ArrayVariables
	extends ArraysToMap
{
	/// Class Imports
	import Scalaz._
	
	
	def unapplySeq[A] (answers : Seq[Answer[A]]) : Option[Seq[Map[Int, A]]] =
		arrays (answers).point[Option] filterNot (_.isEmpty) map {
			_.values.view.toSeq.map (_.toMap);
			}
}


/**
 * The '''AllArrays''' extractor is similar in spirit to the
 * [[com.tubros.constraints.api.solver.ArrayVariables]] extractor, differing
 * in that the result produced (if possible) is a
 * `Map[VariableName, Map[Int, A]]`.
 */
object AllArrays
	extends ArraysToMap
{
	/// Class Imports
	import Scalaz._
	
	
	def unapply[A] (all : Seq[Answer[A]])
		: Option[Map[VariableName, Map[Int, A]]] =
		arrays (all).point[Option] filterNot (_.isEmpty) map {
			_.mapValues (_.toMap);
			}
}
