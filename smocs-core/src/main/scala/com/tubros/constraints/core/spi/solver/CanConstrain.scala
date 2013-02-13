/**
 * Created on: Feb 5, 2013
 */
package com.tubros.constraints.core.spi.solver

import scala.language.higherKinds

import scalaz.syntax.Ops


/**
 * The '''CanConstrain''' type is a ''type class'' which can create
 * [[com.tubros.constraints.core.spi.solver.Constraint]]s from a container
 * ''F'' which has value types of ''A''.  Specifying the value type at the
 * `trait` level allows the concrete '''CanConstrain''' type to have full
 * control over what requirements ''A'' must fulfill without having to
 * introduce API pollution at this level.
 *
 * @author svickers
 *
 */
trait CanConstrain[F[_], A]
{
	/// Self Type Alias
	self =>

		
	/**
	 * The constrains method will produce a
	 * [[com.tubros.constraints.core.spi.solver.Constraint]] for the type ''T''
	 * which can subsequently be used in solving CSP's.
	 */
	def constrains (t : F[A]) : Constraint[A];
}


object CanConstrain
{
	def apply[F[_], A] (implicit ev : CanConstrain[F, A])
		: CanConstrain[F, A] = ev;
}


trait CanConstrainOps[F[_], A]
	extends Ops[F[A]]
{
	/// Instance Properties
	implicit def F : CanConstrain[F, A];
	
	
	final def constrains : Constraint[A] = F.constrains (self);
}


object ToCanConstrainOps
{
	/// Implicit Conversions
	implicit class ConvertToCanConstrain[A, F[_]] (v : F[A])
		(implicit ev : CanConstrain[F, A])
		extends CanConstrainOps[F, A]
	{
		/// Instance Properties
		val F : CanConstrain[F, A] = ev;
		lazy val self = v;
	}
}
