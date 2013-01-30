/**
 * Created on: Jan 29, 2013
 */
package com.tubros.constraints.api
package solver

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner


/**
 * The '''DiscreteDomainSpec''' type verifies the
 * [[com.tubros.constraints.api.solver.DiscreteDomain]] generic type, ensuring that its
 * contract meets expectations.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class DiscreteDomainSpec
	extends ProjectSpec
{
	"A DiscreteDomain" should "have SetLike characteristics" in
	{
		val empty = DiscreteDomain.empty[Int];
		
		empty must not be === (null);
		empty should be ('empty);
		empty.contains (99) should be === (false);
	}
	
	it should "support 'finite' domains" in
	{
		val ten : Domain[Int] = DiscreteDomain.empty[Int] ++ (1 to 10);
		
		ten should not be ('infinite);
		ten.bounds should be === (Some ((1, 10)));
	}
	
	it should "retain the DiscreteDomain type in its returned instances" in
	{
		val ten = DiscreteDomain.empty[Int] ++ (1 to 10);
		val firstThree : DiscreteDomain[Int] = ten.take (3);
		val last : DiscreteDomain[Int] = ten.init;
		val even : DiscreteDomain[Int] = ten filter (n => (n % 2) == 0);
		val odd : DiscreteDomain[Int] = ten &~ even;
		
		firstThree must have size (3);
		last must have size (9);
		even must have size (5);
		odd must have size (5);
	}
	
	it should "conform to the Monoid laws" in
	{
		val strings = DiscreteDomain.empty[String] ++ List ("a", "b", "c", "d");
		val more = DiscreteDomain.empty[String] ++ List ("e", "f", "g");
		val stillMore = DiscreteDomain.empty[String] ++ List ("h", "i");
		val monoid = implicitly[scalaz.Monoid[DiscreteDomain[String]]];
		val laws = monoid.monoidLaw;
		
		laws.leftIdentity (strings) must be === (true);
		laws.rightIdentity (strings) must be === (true);
		laws.associative (strings, more, stillMore) must be === (true);
	}
	
	it should "conform to the Category laws" in
	{
		val strings = DiscreteDomain.empty[Int] ++ List (1, 2);
		val more = DiscreteDomain.empty[Int] ++ List (3, 4, 5);
		val stillMore = DiscreteDomain.empty[Int] ++ List (6);
		val monoid = implicitly[scalaz.Monoid[DiscreteDomain[Int]]];
		val laws = monoid.monoidLaw;
		
		laws.leftIdentity (strings) must be === (true);
		laws.rightIdentity (strings) must be === (true);
		laws.associative (strings, more, stillMore) must be === (true);
	}
}
