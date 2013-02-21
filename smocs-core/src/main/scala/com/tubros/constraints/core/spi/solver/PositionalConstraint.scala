/**
 * Created on: Feb 18, 2013
 */
package com.tubros.constraints.core.spi.solver

import scalaz.{
	Ordering => _,
	_
	}

import com.tubros.constraints.api._

import problem.Expression
import problem.ast._


/**
 * The '''PositionalConstraint''' type provides the ability to interpret
 * positional directives, as defined in
 * [[com.tubros.constraints.api.problem.PositionalSupport]].
 *
 * @author svickers
 *
 */
trait PositionalConstraint[A]
	extends Interpreted[A]
{
	/// Self Type Constraints
	this : Constraint[A] =>
		
		
	/// Class Imports
	import PositionalConstraint._
	import Scalaz._
	
	
	/// Instance Properties
	implicit val ordering : Ordering[A];
	

	abstract override protected def interpreter
		: Env[A] => PartialFunction[Expression[A], Result[A]] =
		env => (super.interpreter (env) orElse positionalInterpreter (env));
			
		
	private def positionalInterpreter
		: Env[A] => PartialFunction[Expression[A], Result[A]] =
	{
		implicit env =>
			
		_ match {
			case After (VariableReferenced (name), Offset (offset)) =>
				Result (valueAt (name, offset) (l => l));
				
			case Before (VariableReferenced (name), Offset (offset)) =>
				Result (valueAt (name, offset) (_.reverse));
				
			case FirstPosition () =>
				Result.right (sort (env.to[List]).headOption.map (_._2));
				
			case LastPosition () =>
				Result.right (sort (env.to[List]).lastOption.map (_._2));
			}
	}
		
		
	private def sort (entries : List[(VariableName, A)])
		: List[(VariableName, A)] =
	{
		def insert (
			list : List[(VariableName, A)],
			e : (VariableName, A)
			)
			: List[(VariableName, A)] =
			list match {
				case Nil => List[(VariableName, A)] (e);
				case x :: tail if (ordering.gt (x._2, e._2)) => e :: x :: tail;
				case x :: tail => x :: insert (tail, e);
				}

		entries.foldLeft (List.empty[(VariableName, A)]) (insert);
	}
	
	
	private def valueAt (name : VariableName, offset : Int)
		(f : List[(VariableName, A)] => List[(VariableName, A)])
		(implicit env : Env[A])
		: Option[Boolean \/ A] =
	{
		val sorted = f (sort (env.to[List]));
		
		sorted.dropWhile (p => p._1 != name).drop (offset).headOption.map {
			p =>
				
			\/- (p._2);
			};
	}
}


object PositionalConstraint
{
	/// Class Types
	case object VariableReferenced
	{
		def unapply[T] (expr : Expression[T]) : Option[VariableName] =
			expr match {
				case VariableUse (name) => Some (name);
				case _ => None;
				}
	}
}
