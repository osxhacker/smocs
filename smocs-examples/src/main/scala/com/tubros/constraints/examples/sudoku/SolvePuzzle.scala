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
