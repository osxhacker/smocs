/**
 * Created on: Sep 19, 2013
 */
package com.tubros.constraints.examples
package sudoku

import scalaz._

import com.tubros.constraints._

import api.solver.FiniteDiscreteDomain
import api.solver.error.SolverError
import core.internal.tree._


/**
 * The '''SolvePuzzle''' type is an example application which will solve a
 * Sudoku puzzle using Smocs.
 *
 * @author svickers
 *
 */
object SolvePuzzle
	extends App
		with ConstraintSolver[
			TreeFiniteDomainSolver[Int]#SolverState,
			TreeFiniteDomainSolver[Int]
			]
		with Timer
{
	/// Class Imports
	import relational._
	import std.anyVal._
	import syntax.show._
	import syntax.std.option._
	
	
	/// Class Types
	type SolverType = TreeFiniteDomainSolver[Int]
	
	
	/// Instance Properties
	val options = CommandLineArguments (args);
	private implicit val monad = TreeFiniteDomainSolver.solverMonad[Int];
	
	lazy val usage = """
		usage: SolvePuzzle
		""";

	
	/// Constructor Body
	run ();
	
	
	override def createSolver () : TreeFiniteDomainSolver[Int] =
		new TreeFiniteDomainSolver[Int] (PreferSmallerDomain[Int] ());
	
	
	override def domainOf (solver : SolverType, values : Seq[Int])
		: solver.DomainType[Int] =
		FiniteDiscreteDomain (values);
	
	
	private def run () : Unit =
	{
		// TODO: populate puzzle with CLI Square values
		val puzzle = Puzzle (
			Set (
				Square ('A', 3, 3.some),
				Square ('A', 5, 2.some),
				Square ('A', 7, 6.some),

				Square ('B', 1, 9.some),
				Square ('B', 4, 3.some),
				Square ('B', 6, 5.some),
				Square ('B', 9, 1.some),

				Square ('C', 3, 1.some),
				Square ('C', 4, 8.some),
				Square ('C', 6, 6.some),
				Square ('C', 7, 4.some),

				Square ('D', 3, 8.some),
				Square ('D', 4, 1.some),
				Square ('D', 6, 2.some),
				Square ('D', 7, 4.some),

				Square ('E', 1, 7.some),
				Square ('E', 9, 8.some),

				Square ('F', 3, 6.some),
				Square ('F', 4, 7.some),
				Square ('F', 6, 8.some),
				Square ('F', 7, 2.some),

				Square ('G', 3, 2.some),
				Square ('G', 4, 6.some),
				Square ('G', 6, 9.some),
				Square ('G', 7, 5.some),

				Square ('H', 1, 8.some),
				Square ('H', 4, 2.some),
				Square ('H', 6, 3.some),
				Square ('H', 9, 9.some),

				Square ('I', 3, 5.some),
				Square ('I', 5, 1.some),
				Square ('I', 7, 3.some)
				)
			);
		val solvePuzzle = new SolveSudokuPuzzle ();
		
		time {
			val answer = solvePuzzle (puzzle);
			
			println ("### answer is: %s".format (answer));
			}
	}
}
