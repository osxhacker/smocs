/**
 * Created on: Jan 27, 2013
 */
package com.tubros.constraints.api

import org.scalatest._


/**
 * The '''UseCaseSpec''' type reifies test suites designed to elucidate API
 * capabilities at a behavioural level.
 *
 * @author svickers
 *
 */
trait UseCaseSpec
	extends FeatureSpec
		with GivenWhenThen
		with ShouldMatchers
