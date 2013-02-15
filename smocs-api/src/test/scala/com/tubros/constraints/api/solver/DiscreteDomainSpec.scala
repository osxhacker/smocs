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
	/// Class Imports
	import scalaz.std.AllInstances._
	import scalaz.syntax.show._
	
	
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
		
		ten.hasDefiniteSize should be === (true);
		ten.bounds should be === (Some ((1, 10)));
	}
	
	it should "retain the DiscreteDomain type in its returned instances" in
	{
		val ten = DiscreteDomain.empty[Int] ++ (1 to 10);
		val firstThree : DiscreteDomain[Int] = ten.take (3);
		val allButLast : DiscreteDomain[Int] = ten.init;
		val even : DiscreteDomain[Int] = ten filter (n => (n % 2) == 0);
		val odd : DiscreteDomain[Int] = ten &~ even;
		
		ten must have size (10);
		firstThree must have size (3);
		allButLast must have size (9);
		even must have size (5);
		odd must have size (5);
	}
	
	it should "support the Show monad" in
	{
		val empty : DiscreteDomain[Double] = DiscreteDomain.empty[Double];
		
		empty.shows should not be ('empty);
	}
	
	it should "conform to the Monad laws" in
	{
		val doubles = DiscreteDomain.empty[Double] ++ Seq (1.0, 2.0, 3.0);
		val monad = implicitly[scalaz.Monad[DiscreteDomain]];
		val laws = monad.monadLaw;
		
		laws.identity (doubles) must be === (true);
		laws.leftIdentity (99.0, singleton[Double]) must be === (true);
		laws.rightIdentity (doubles) must be === (true);
	}
	
	it should "conform to the MonadPlus laws" in
	{
		val doubles = DiscreteDomain.empty[Double] ++ Seq (1.0, 2.0, 3.0);
		val monadPlus = implicitly[scalaz.MonadPlus[DiscreteDomain]];
		val laws = monadPlus.monadPlusLaw;
		
		laws.emptyMap ((n : Double) => n) must be === (true);
		laws.leftZero (singleton[Double]) must be === (true);
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
		val ints = DiscreteDomain.empty[Int] ++ List (1, 2);
		val more = DiscreteDomain.empty[Int] ++ List (3, 4, 5);
		val stillMore = DiscreteDomain.empty[Int] ++ List (6);
		val category = implicitly[scalaz.Monoid[DiscreteDomain[Int]]].category;
		val laws = category.categoryLaw;
		
		laws.leftIdentity (ints) must be === (true);
		laws.rightIdentity (ints) must be === (true);
		laws.associative (ints, more, stillMore) must be === (true);
	}
	
	it should "support Functor use" in
	{
		val ints = DiscreteDomain.empty[Int] ++ Seq (4, 5, 6);
		val functor = implicitly[scalaz.Functor[DiscreteDomain]];
		
		functor (ints) (n => n + 1) must be === (
			DiscreteDomain.empty[Int] ++ Seq (5, 6, 7)
			);
	}
	
	
	private def singleton[A] (a : A) : DiscreteDomain[A] =
		DiscreteDomain.empty[A] ++ Seq (a);
}
