/**
 * Created on: Feb 12, 2013
 */
package com.tubros.constraints.core.spi.solver

import scala.language.higherKinds

import scalaz._

import com.tubros.constraints.api.problem.Expression


/**
 * The '''Interpreted''' type serves to reify
 * [[com.tubros.constraints.core.spi.solver.Constraint]]s which interpret the
 * CSP AST.
 *
 * @author svickers
 *
 */
trait Interpreted[A]
{
	/// Self Type Constraints
	this : Constraint[_] =>
		
		
	/// Class Imports
	import Scalaz._
	
	
	/// Class Types
	type Result[X] = Boolean \/ X
	
	
	protected def interpreter
		: Env[A] => PartialFunction[Expression[A], Result[A]];
	
	
	protected def binary (op : (A, A) => Result[A]) = op.tupled;
	
	
	protected def unary (op : A => Result[A]) = x => op (x);
	
	
	protected def eval (env : Env[A])
		(l : Expression[A], r : Expression[A])
		(implicit m : Monad[Result])
		: Result[(A, A)] =
		(interpreter (env) (l) |@| interpreter (env) (r)) {
			(_, _);
			}
}
