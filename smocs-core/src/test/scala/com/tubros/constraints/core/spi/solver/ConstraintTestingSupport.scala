/**
 * Created on: Mar 13, 2013
 */
package com.tubros.constraints.core.spi.solver

import scalaz._

import org.scalatest.{
	Suite,
	SuiteMixin
	}

import com.tubros.constraints.api._
import com.tubros.constraints.api.solver._
import com.tubros.constraints.api.solver.error._
import com.tubros.constraints.core.spi.solver._

import runtime.ConstraintProvider


/**
 * The '''ConstraintTestingSupport''' type defines
 * [[com.tubros.constraints.core.spi.solver.Constraint]]-related types used
 * ''strictly'' for testing.  They do not have any systemic value other than
 * assisting to certify other abstractions.
 *
 * @author svickers
 *
 */
trait ConstraintTestingSupport
	extends SuiteMixin
{
	/// Self Type Constraints
	this : Suite =>


	/// Class Types
	class TestConstraint[A] (
		override val variables : Set[VariableName],
		val allow : Set[A]
		)
		extends Constraint[A]
	{
		/// Class Imports
		import syntax.std.boolean._


		override def apply (in : Env[A]) : SolverError \/ Env[A] =
			in.values.forall (allow.contains).fold (
				\/- (in),
				-\/ (UnsolvableError)
				);
	}
	
	
	trait TestConstraintProvider[A]
		extends ConstraintProvider[A]
	{
		override lazy val globalConstraints = Constraint.kleisliUnit[A];
	
		override def constraintsFor (available : Set[VariableName]) =
			constraints filter (_.isDefinedAt (available));
	}
	
	
	object TestConstraintProvider
	{
		def apply[A] (c : Constraint[A]) = new TestConstraintProvider[A] {
			override val constraints = Set (c);
			}
	}
}
