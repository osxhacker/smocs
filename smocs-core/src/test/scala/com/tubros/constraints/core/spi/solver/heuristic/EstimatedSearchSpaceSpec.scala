/**
 * Created on: Mar 2, 2013
 */
package com.tubros.constraints.core.spi.solver.heuristic

import scalaz._

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api._

import solver._


/**
 * The '''EstimatedSearchSpaceSpec''' type ensures that the
 * [[com.tubros.constraints.core.spi.solver.heuristic.EstimatedSearchSpace]]
 * properly estimates a problem's overall size.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class EstimatedSearchSpaceSpec
	extends ProjectSpec
{
	/// Class Imports
	import std.list._
	
	
	"An EstimatedSearchSpace" should "have a size of zero with no variables" in
	{
		EstimatedSearchSpace (List.empty[DiscreteDomain[Int]]) should be === (0L);
	}
	
	it should "estimate the size as a product of all variable domain sizes" in
	{
		val domains = List (
			FiniteDiscreteDomain (1 to 10),
			FiniteDiscreteDomain (1 to 20),
			FiniteDiscreteDomain (1 to 30),
			FiniteDiscreteDomain (1 to 40)
			);
		
		EstimatedSearchSpace (domains) should be === (10L * 20L * 30L * 40L);
	}
}
