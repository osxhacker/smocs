/**
 * Created on: Sep 19, 2013
 */
package com.tubros.constraints.examples.sudoku

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


/**
 * The '''SquareSpec''' type ensures that the
 * [[com.tubros.constraints.examples.sudoku.Square]] `sudoku` domain type
 * operates as needed for the ''CSP'' example.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class SquareSpec
	extends ProjectSpec
{
	"A Square" should "be capapble of being managed by a unique container" in
	{
		val set = Set (
			Square ('D', 3, None),
			Square ('D', 4, Some (1)),
			Square ('D', 5, Some (2)),
			Square ('D', 6, Some (3))
			);
		
		set should have size (4);
		set (Square ('D', 3, None)) should be === (true);
		set.find (_.column == 5).get.assignment should be === (Some (2));
	}
}
