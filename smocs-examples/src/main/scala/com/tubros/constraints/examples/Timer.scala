/**
 * Created on: Sep 19, 2013
 */
package com.tubros.constraints.examples

import scala.compat.Platform.currentTime


/**
 * The '''Timer''' type is a simple `trait` which allows the `examples` to
 * produce a rudimentary execution time measurement.  This is _by no means_
 * production code and is only included for illustrative purposes.
 *
 * @author svickers
 *
 */
trait Timer
{
	protected def time[R] (block : => R) : R =
	{
		val start = currentTime;
		val r = block;
		val duration = currentTime - start;
		
		println ("Execution time: %d milliseconds".format (duration));
		
		return (r);
	}

}
