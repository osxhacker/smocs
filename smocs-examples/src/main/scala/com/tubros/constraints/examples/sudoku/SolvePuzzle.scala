/**
 * Created on: Sep 19, 2013
 */
package com.tubros.constraints.examples
package sudoku

import scala.concurrent.{
	ExecutionContext,
	Future
	}

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
		|usage: SolvePuzzle [quit-after=N]
		|
		|Where:
		|    quit-after: Optional number of seconds to allow the search to run
		|
		|And 'N' is any positive integer (which can be different for each param)
		|""".stripMargin;


	/// Constructor Body
	if (args.contains ("help") || args.contains ("-?"))
		println (usage);
	else
		run (options.get ("quit-after"));


	/**
	 * For a Sudoku CSP, there's no need to use variable impact analysis
	 * when determining the tree root.  The domain sizes for all variables are
	 * either 1 (assigned at board creation) or 9.
	 */
	override def createSolver () : TreeFiniteDomainSolver[Int] =
		new TreeFiniteDomainSolver[Int] (PreferSmallerDomain[Int] ());


	override def domainOf (solver : SolverType, values : Seq[Int])
		: solver.DomainType[Int] =
		FiniteDiscreteDomain (values);


	private def run (maxRunTime : Option[Int]) : Unit =
	{
		// TODO: populate puzzle with CLI Square values
		val puzzle = Puzzle (
			Set (
				Square ('A', 0, 5.some),
				Square ('A', 1, 3.some),
				Square ('A', 4, 7.some),

				Square ('B', 0, 6.some),
				Square ('B', 3, 1.some),
				Square ('B', 4, 9.some),
				Square ('B', 5, 5.some),

				Square ('C', 1, 9.some),
				Square ('C', 2, 8.some),
				Square ('C', 7, 6.some),

				Square ('D', 0, 8.some),
				Square ('D', 4, 6.some),
				Square ('D', 8, 3.some),

				Square ('E', 0, 4.some),
				Square ('E', 3, 8.some),
				Square ('E', 5, 3.some),
				Square ('E', 8, 1.some),

				Square ('F', 0, 7.some),
				Square ('F', 4, 2.some),
				Square ('F', 8, 6.some),

				Square ('G', 1, 6.some),
				Square ('G', 6, 2.some),
				Square ('G', 7, 8.some),

				Square ('H', 3, 4.some),
				Square ('H', 4, 1.some),
				Square ('H', 5, 9.some),
				Square ('H', 8, 5.some),

				Square ('I', 4, 8.some),
				Square ('I', 7, 7.some),
				Square ('I', 8, 9.some)
				)
			);
		val solvePuzzle = new SolveSudokuPuzzle ();

		maxRunTime foreach {
			seconds =>
				
			import ExecutionContext.Implicits._
			
			Future {
				Thread.sleep (seconds * 1000);

				println (s"Sudoku puzzle solver ran out of time after $seconds seconds");
				System.exit (1);
				}
			}

		time {
			val answer = solvePuzzle (puzzle);

			println ("Sudoku puzzle answer is: %s".format (answer));
			}
	}
}
