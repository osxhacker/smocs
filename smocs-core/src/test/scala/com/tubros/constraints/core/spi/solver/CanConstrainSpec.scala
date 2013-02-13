/**
 * Created on: Feb 6, 2013
 */
package com.tubros.constraints.core.spi.solver

import scala.language.higherKinds

import scalaz._

import org.junit.runner.RunWith
import org.scalamock.scalatest.MockFactory
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api._

import solver._
import solver.error._


/**
 * The '''CanConstrainSpec''' type unit tests the
 * [[com.tubros.constraints.core.spi.solver.CanConstrain]] ''type class'' to
 * ensure fitness for use in the core bundle.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class CanConstrainSpec
	extends ProjectSpec
{
	/// Class Imports
	import scalaz.syntax.std.option._
	import ToCanConstrainOps._
	
	
	/// Testing Collaborators
	implicit val canConstrainStrings = create[String] (List ('a, 'b, 'c));
	implicit val canConstrainSymbols = create[Symbol] (List ('a, 'b, 'c));
	implicit val canConstrainNumerics = numeric[Int] (List ('x));
	
	
	"CanConstrain" should "be implicitly available" in
	{
		val cc = implicitly[CanConstrain[Option, String]];
		
		cc must not be === (null);
	}
	
	it should "produce a Constraint when requested" in
	{
		val constraint = "foo".some.constrains;
		
		constraint must not be === (null);
	}
	
	it should "produce a Constraint capable of filtering" in
	{
		val constraint = "desired".some.constrains;
		val domain = FiniteDiscreteDomain (
			Set ("find", "the", "desired", "value")
			);
		val aVariable = Map[VariableName, String] (
			VariableName ('x) -> "desired"
			);
		
		constraint (aVariable) should be === (\/- (aVariable));
	}
	
	it should "be able to constrain numeric types" in
	{
		val constraint = 1.some.constrains;
		val domain = FiniteDiscreteDomain (0 to 3);
		val aVariable = Map[VariableName, Int] (
			VariableName ('x) -> 2
			);
		
		constraint (aVariable) should be === (\/- (aVariable));
	}
	
	
	private def create[A <: AnyRef] (names : Seq[VariableName])
		: CanConstrain[Option, A] =
		new CanConstrain[Option, A] {
			override def constrains (t : Option[A]) : Constraint[A] =
				new Constraint[A] {
					override val variables = names.toSet;
					private val valueToAllow = t getOrElse (null.asInstanceOf[A]);

					override def apply (in : Map[VariableName, A])
						: SolverError \/ Map[VariableName, A] =
						\/- (in);
					}
			}
	
	
	private def numeric[T : Numeric] (names : Seq[VariableName])
		: CanConstrain[Option, T] =
		new CanConstrain[Option, T] {
			override def constrains (v : Option[T]) : Constraint[T] =
				new Constraint[T] {
					override val variables = names.toSet;
					val numeric = implicitly[Numeric[T]]; 
					
					override def apply (in : Map[VariableName, T])
						: SolverError \/ Map[VariableName, T] =
						\/- (in);
					}
			}
}
