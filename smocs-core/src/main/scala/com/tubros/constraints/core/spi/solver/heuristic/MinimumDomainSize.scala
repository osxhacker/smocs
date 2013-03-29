/**
 * Created on: Mar 25, 2013
 */
package com.tubros.constraints.core.spi.solver.heuristic

import scala.language.{
	higherKinds,
	postfixOps
	}

import scalaz._

import com.tubros.constraints.api.solver.{
	Domain,
	Variable
	}


/**
 * The '''MinimumDomainSize''' type `heuristic` determines which
 * [[com.tubros.constraints.api.solver.Variable]] in a collection has the
 * smallest [[com.tubros.constraints.api.solver.Domain]] size
 *
 * @author svickers
 *
 */
case class MinimumDomainSize[A] ()
{
	def apply[DT[X] <: Domain[X]] (variables : List[Variable[A, DT]])
		: List[Variable[A, DT]] =
		variables.sortBy (_.domain.size);
}
