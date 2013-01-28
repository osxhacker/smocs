/**
 * Created on: Jan 25, 2013
 */
package com.tubros.constraints.api

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.prop.Checkers


/**
 * The '''ProjectSpec''' type captures unit-test related capabilities in a
 * centrally defined place.  Concrete unit tests can inherit from this type
 * and simply annotate their `class` definition along the lines of:
 * 
 * {{{
 * @RunWith (classOf[JUnitRunner])
 * class FooSpec extends ProjectSpec
 * }}}
 * 
 * And be picked up by the [[http://junit.org JUnit]] runner.
 *
 * @author svickers
 *
 */
trait ProjectSpec
	extends FlatSpec
		with Checkers
		with MustMatchers
		with ShouldMatchers