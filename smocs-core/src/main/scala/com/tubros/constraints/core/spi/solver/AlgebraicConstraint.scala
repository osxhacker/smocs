/**
 * Created on: Feb 8, 2013
 */
package com.tubros.constraints.core.spi.solver

import Predef.{
	any2stringadd => _,
	_
	}

import scalaz.{
	Plus => _,
	_
	}

import com.tubros.constraints.api._

import problem._
import ast._


/**
 * The '''AlgebraicConstraint''' type provides the ability to evaluate
 * [[http://en.wikipedia.org/wiki/Algebraic_function algebraic expressions]]
 * which produce a [[scala.math.Numeric]] answer.  These are part of a greater
 * definition of a [[com.tubros.constraints.core.spi.solver.Constraint]].
 *
 * @author svickers
 *
 */
trait AlgebraicConstraint[A]
	extends Interpreted[A]
{
	/// Self Type Constraints
	this : Constraint[A] =>
		
		
	/// Class Imports
	import Scalaz._
	
	
	/// Instance Properties
	implicit val numeric : Numeric[A];
		
	
	private val abs = unary {
		(x : A) => numeric.abs (x).right;
		}
	
	private val minus = binary {
		(x : A, y : A) => numeric.minus (x, y).right;
		}
	
	private val negate = unary {
		(x : A) => numeric.negate (x).right;
		}
	
	private val plus = binary {
		(x : A, y : A) => numeric.plus (x, y).right;
		}
	
	private val pow = maybeBinary {
		(x : A, y : A) =>
			
		InterpretedResult {
			(numeric.signum (y) > 0).option {
				(1 until numeric.toInt (y)).foldLeft (x) {
					case (r, _) =>
						
					numeric.times (r, x);
					}.right[Boolean]
				}
			}
		}
	
	private val times = binary {
		(x : A, y : A) => numeric.times(x, y).right
		}
	
	
	abstract override protected def interpreter
		: Env[A] => PartialFunction[Expression[A], InterpretedResult[A]] =
		env => (super.interpreter (env) orElse algebraicInterpreter (env));
			
		
	private def algebraicInterpreter
		: Env[A] => PartialFunction[Expression[A], InterpretedResult[A]] =
	{
		env =>
			
		_ match {
			case AbsoluteValue (e) => interpreter (env) (e) flatMap (abs);
			case Minus (l, r) => eval (env) (l, r) flatMap (minus);
			case Negate (e) => interpreter (env) (e) flatMap (negate);
			case Plus (l, r) => eval (env) (l, r) flatMap (plus);
			case Power (l, r) => eval (env) (l, r) flatMap (pow);
			case Times (l, r) => eval (env) (l, r) flatMap (times);
			}
	}
}
