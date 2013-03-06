/**
 * Created on: Feb 28, 2013
 */
package com.tubros.constraints.core.spi.solver.heuristic

import scalaz._

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api._
import com.tubros.constraints.core.spi.solver._
import com.tubros.constraints.api.problem._
import com.tubros.constraints.api.solver._
import com.tubros.constraints.api.solver.error._



/**
 * The '''ConstraintPropagationSpec''' type defines unit tests to ensure that
 * the [[com.tubros.constraints.core.internal.tree.ConstraintPropagation]]
 * `class` properly filters, or ''propagates'', a
 * [[com.tubros.constraints.api.solver.Domain]] by way of one or more
 * [[com.tubros.constraints.core.spi.solver.Constraint]]s.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class ConstraintPropagationSpec
	extends ProjectSpec
{
	/// Class Types
	class TestConstraint (
		override val variables : Set[VariableName],
		val allow : Set[Int]
		)
		extends Constraint[Int]
	{
		/// Class Imports
		import syntax.std.boolean._


		override def apply (in : Env[Int]) : SolverError \/ Env[Int] =
			in.values.forall (allow.contains).fold (
				\/- (in),
				-\/ (UnsolvableError)
				);
	}


	/// Test Collaborators
	val names = Set[VariableName] ('alpha, 'beta, 'gamma);
	val alpha = DiscreteVariable ('alpha, FiniteDiscreteDomain (0 to 100));
	val beta = DiscreteVariable ('beta, FiniteDiscreteDomain (10 to 200));
	val gamma = DiscreteVariable ('gamma, FiniteDiscreteDomain (101 to 200));


	"ConstraintPropagation" should "be able to operate with no Constraints" in
	{
		val unconstrained = new ConstraintPropagation[Int, DiscreteDomain] (
			Set.empty[Constraint[Int]]
			);

		unconstrained (
			createPriorAssignments (alpha, beta), gamma
			) should be === (gamma);
	}

	it should "apply a Constraint and produce a different value set" in
	{
		val allow10 = new TestConstraint (names - 'gamma, (0 to 10).to[Set]);
		val constrain = new ConstraintPropagation[Int, DiscreteDomain] (
			Set[Constraint[Int]] (allow10)
			);

		constrain (createPriorAssignments (alpha), beta) should be === (
			DiscreteVariable (beta.name, FiniteDiscreteDomain (10))
			);
	}
	
	it should "be able to completely filter out a domain" in
	{
		val allowNone = new TestConstraint (names - 'gamma, Set.empty[Int]);
		val constrain = new ConstraintPropagation[Int, DiscreteDomain] (
			Set[Constraint[Int]] (allowNone)
			);

		constrain (createPriorAssignments (alpha), beta) should be ('empty);
	}
	
	it should "not apply a Constraint which needs more variables" in
	{
		val allowAll = new TestConstraint (names, (0 to 200).to[Set]);
		val constrain = new ConstraintPropagation[Int, DiscreteDomain] (
			Set[Constraint[Int]] (allowAll)
			);
		
		constrain (createPriorAssignments (alpha), beta).domain should be === (
			beta.domain
			);
	}


	private def createPriorAssignments (
		variables : Variable[Int, DiscreteDomain] *
		)
		: Seq[Answer[Int]] =
		variables.to[Seq].map (v => Answer (v.name, v.domain.head));
}
