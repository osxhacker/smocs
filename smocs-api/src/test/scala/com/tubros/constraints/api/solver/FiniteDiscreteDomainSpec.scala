/**
 * Created on: Feb 1, 2013
 */
package com.tubros.constraints.api.solver

import scalaz._

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api._


/**
 * The '''FiniteDiscreteDomainSpec''' type unit tests the
 * [[com.tubros.constraints.api.solver.FiniteDiscreteDomain]] and serves as
 * an exmplar for expected use.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class FiniteDiscreteDomainSpec
	extends ProjectSpec
{
	"A FiniteDiscreteDomain" should "be constructable from a Set" in
	{
		FiniteDiscreteDomain (Set (1, 2, 3)) should have size (3);
	}
	
	it should "support variadic construction" in
	{
		FiniteDiscreteDomain (99) shouldBe (Set (99));
		FiniteDiscreteDomain (99, 100) shouldBe (Set (99, 100));
	}
	
	it should "support construction from a range" in
	{
		FiniteDiscreteDomain (1 to 5) shouldBe (Set (1, 2, 3, 4, 5));
		FiniteDiscreteDomain ('a' to 'c') shouldBe (Set ('a', 'b', 'c'));
	}
	
	it should "support construction from a Scalaz Enum" in
	{
		import scalaz.std.AllInstances._
		import scalaz.syntax.enum._
		
		FiniteDiscreteDomain (1 |==> (2, 5)) shouldBe (Set (1, 3, 5));
	}
}