/**
 * Created on: Feb 21, 2013
 */
package com.tubros.constraints.core.internal

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.constrained.Graph


/**
 * The '''graph''' `package` defines
 * [[com.tubros.constraints.api.solver.Solver]] types related to graph-based
 * implementations.
 *
 * @author svickers
 *
 */
package object graph
	extends BuiltinEquationConstraintCategories
{
	/// Class Types
	type ConstraintGraph = Graph[Int, DiEdge]
	type GraphEdge = DiEdge[String]
}
