/**
 * Created on: Feb 12, 2013
 */
package com.tubros.constraints.core.spi.solver

import scalaz.{
	Ordering => _,
	_
	}

import com.tubros.constraints.api.problem.Expression
import com.tubros.constraints.api.problem.ast._


/**
 * The '''RelationalConstraint''' type provides the ability to evaluate
 * relational operators, such as `<` and `>=', when searching for CSP answers.
 *
 * @author svickers
 *
 */
trait RelationalConstraint[A]
	extends Interpreted[A]
{
	/// Self Type Constraints
	this : Constraint[A] =>
		
		
	/// Class Imports
	import Scalaz._
	
	
	/// Instance Properties
	implicit val ordering : Ordering[A];
	
	private val equiv = binary {
		(x : A, y : A) => ordering.equiv (x, y).left;
		}
	
	private val nequiv = binary {
		(x : A, y : A) => (!ordering.equiv (x, y)).left;
		}
	
	private val gt = binary {
		(x : A, y : A) => ordering.gt (x, y).left;
		}
	
	private val lt = binary {
		(x : A, y : A) => ordering.lt (x, y).left;
		}
	
	private val gte = binary {
		(x : A, y : A) => ordering.gteq (x, y).left;
		}
	
	private val lte = binary {
		(x : A, y : A) => ordering.lteq (x, y).left;
		}
	

	abstract override protected def interpreter
		: Env[A] => PartialFunction[Expression[A], InterpretedResult[A]] =
		env => (super.interpreter (env) orElse relationalInterpreter (env));
			
		
	private def relationalInterpreter
		: Env[A] => PartialFunction[Expression[A], InterpretedResult[A]] =
	{
		env =>
			
		_ match {
			case LogicalAnd (l, r) => evalRelational (env) (l, r) {
				_ && _;
				}
			
			case LogicalOr (l, r) => evalRelational (env) (l, r) {
				_ || _;
				}
			
			case EqualTo (l, r) => eval (env) (l, r) flatMap (equiv);
			case NotEqualTo (l, r) => eval (env) (l, r) flatMap (nequiv);
			case GreaterThan (l, r) => eval (env) (l, r) flatMap (gt);
			case GreaterThanOrEqualTo (l, r) => eval (env) (l, r) flatMap (gte);
			case LessThan (l, r) => eval (env) (l, r) flatMap (lt);
			case LessThanOrEqualTo (l, r) => eval (env) (l, r) flatMap (lte);
		}
	}
		
		
	private def evalRelational (env : Env[A])
		(lhs : Expression[A], rhs : Expression[A])
		(condition : (Boolean, Boolean) => Boolean)
		: InterpretedResult[A] =
	{
		val reversed = for {
			x <- interpreter (env) (lhs).swap
			y <- interpreter (env) (rhs).swap
		} yield condition (x, y);
		
		return (reversed.swap);
	}
}
