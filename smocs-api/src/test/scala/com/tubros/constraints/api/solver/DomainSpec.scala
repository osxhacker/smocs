/**
 * Created on: Jan 29, 2013
 */
package com.tubros.constraints.api
package solver

import scalaz._

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner


/**
 * The '''DomainSpec''' type verifies behaviour provided by the
 * [[com.tubros.constraints.api.solver.Domain]] `trait`.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class DomainSpec
	extends ProjectSpec
{
	/// Class Imports
	import scalaz.std.AllInstances._
	import scalaz.syntax.show._
	
	
	"A Domain" should "be showable" in
	{
		val domain : Domain[Int] = createDomain (5);
		
		domain.show must not be === (null);
		domain.shows must not be ('empty);
	}
	
	it should "elide 'large' domains" in
	{
		createDomain (1000).shows should endWith ("...)");
	}
	
	it should "handle empty domains" in
	{
		createDomain (0).shows should be === ("Domain(<empty>)");
	}
	
	
	private def createDomain (entries : Int) : Domain[Int] =
		if (entries == 0)
			DiscreteDomain.empty[Int];
		else
			DiscreteDomain.empty[Int] ++ (1 to entries);
}