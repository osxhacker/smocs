/**
 * Created on: Sep 19, 2013
 */
package com.tubros.constraints.examples.sudoku

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner


/**
 * The '''ProjectSpec''' type defines the common unit test shape and behaviour
 * available to ''all'' `sudoku` test suites.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
trait ProjectSpec
	extends FlatSpec
		with Matchers
{

}
