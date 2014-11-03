/**
 * Created on: Sep 20, 2013
 */
package com.tubros.constraints.examples.sudoku

import scala.language.higherKinds

import scalaz._

import com.tubros.constraints.api._
import com.tubros.constraints.api.problem._
import com.tubros.constraints.api.solver._
import com.tubros.constraints.api.solver.error._


/**
 * The '''ConstraintSolver''' type is a participant in the CAKE pattern used to
 * solve an arbitrary [[com.tubros.constraints.examples.sudoku.Puzzle]] for the
 * `sudoku` example.
 *
 * @author svickers
 *
 */
trait ConstraintSolver[M[+_], SolverT <: Solver[Int, M, SolverT]]
{
	/// Component Dependencies
	def createSolver () : SolverT;
	def domainOf (solver : SolverT, values : Seq[Int]) : solver.DomainType[Int];

	
	/// Class Types
	trait SudokuEquation
		extends Equation[Int]
            with PropositionalSupport[Int]
			with RelationalSupport[Int]
	
	
	class SolveSudokuPuzzle ()
		(implicit M : Monad[M])
		extends (Puzzle => (SolverError \/ Puzzle))
	{
		/// Class Imports
		import Scalaz._
		
		
		/// Instance Properties
		private val unitAllDiff : GlobalConstraint[Int] =
		{
			case ArrayVariables (as, bs, cs, ds, es, fs, gs, hs, is) =>
				println(s"sudoku all diff:\n${as}\n${bs}\n${cs}\n${ds}\n${es}\n${fs}\n${gs}\n${hs}\n${is}\n\n")
				List ((0, 2), (3, 5), (6, 8)) forall {
					case (start, end) =>

					Set (
						as.slice (start, end) ++
						bs.slice (start, end) ++
						cs.slice (start, end)
						).size === 9 &&
					Set (
						ds.slice (start, end) ++
						es.slice (start, end) ++
						fs.slice (start, end)
						).size === 9 &&
					Set (
						gs.slice (start, end) ++
						hs.slice (start, end) ++
						is.slice (start, end)
						).size === 9;
					}
		}
		
		
		override def apply (puzzle : Puzzle) : SolverError \/ Puzzle =
		{
			val answers = createSolver () {
				implicit solver =>
					
				val problem = problemDefinition ();

				for {
					variables <- createGridVariables (puzzle)
					_ <- solver.add (problem)
					_ <- solver.impose (unitAllDiff)
					stream <- solver.run[Vector]
					} yield stream;
				}
			
			answers >>= {
				all =>
					
				(all.headOption >>= solvedPuzzle) \/> UnsolvableError;
				}
		}
	
	
		private def createGridVariables (puzzle : Puzzle)
			(implicit solver : SolverT)
			=
		{
			val domain = domainOf (solver, 1 to Puzzle.columnNames.size);
			val rowVariableNames =
				Puzzle.rowNames.map (row => (row, VariableName (row.toString)));
			
			rowVariableNames.point[M] >>= {
				names =>
					
				names.foldLeft (List.empty[Variable[Int, solver.DomainType]].point[M]) {
					accum => pair =>

					val (row, name) = pair;
					
					for {
						existing <- accum
						vars <- solver.newArrayVar (name, domain.size) {
							n =>
								
							(puzzle (row, n) >>= (_.assignment)) cata (
								value => domainOf (solver, Seq (value)),
								domain
								)
							}
						} yield existing |+| vars;
					}
				}
		}
		
		
		private def problemDefinition ()
			(implicit solver : SolverT)
			: Problem[Int] =
		{
			def rows (name : Char) : List[SudokuEquation] =
			{
				val row = VariableName (name.toString);
				val equations = for {
					involved <- 2 until Puzzle.columnOffsets.last
					offsets <- Puzzle.columnOffsets.combinations (involved)
					} yield new SudokuEquation {
						def apply = offsets.sliding (2).map {
							case Seq (f, s) =>

							row (f) !== row (s);
							}.reduce (_ && _);
						}

				return (equations.toList);
			}

			def columns (offset : Int) : List[SudokuEquation] =
			{
				val variableNames = Puzzle.rowNames.map {
					name =>
						
					VariableName (name.toString);
					}.toList;
				val equations = for {
					involved <- 2 until variableNames.size
					names <- variableNames.combinations (involved)
					} yield new SudokuEquation {
						def apply = names.sliding (2).map {
							case Seq (first, second) =>

							first (offset) !== second (offset);
							}.reduce (_ && _);
						}

				return (equations.toList);
			}

			def grid () : List[SudokuEquation] =
			{
				val variableNames = Puzzle.rowNames.map {
					name =>
						
					VariableName (name.toString);
					}.toList;

				/// r, c -> r, c
				val offsets =
					((0, 0) -> (1, 1)) ::
					((0, 0) -> (1, 2)) ::
					((0, 0) -> (2, 1)) ::
					((0, 0) -> (2, 2)) ::
					((0, 1) -> (1, 0)) ::
					((0, 1) -> (1, 2)) ::
					((0, 1) -> (2, 0)) ::
					((0, 1) -> (2, 2)) ::
					((0, 2) -> (1, 0)) ::
					((0, 2) -> (1, 1)) ::
					((1, 0) -> (2, 1)) ::
					((1, 0) -> (2, 2)) ::
					((1, 1) -> (2, 0)) ::
					((1, 1) -> (2, 2)) ::
					((1, 2) -> (2, 0)) ::
					((1, 2) -> (2, 1)) ::
					Nil;

				val equations = for {
					rows <- variableNames.grouped (3)
					columns <- Puzzle.columnOffsets.grouped (3)
					o <- offsets
					} yield new SudokuEquation {
						def apply = rows (o._1._1) (columns (o._1._2)) !==
							rows (o._2._1) (columns (o._2._2));
						}

				return (equations.toList);
			}

			Problem[Int] (
				(Puzzle.rowNames.toList >>= (rows _)).toNel.get :::>
				((Puzzle.columnOffsets.toList >>= (columns _)) |+| grid ())
				);
		}


		private def solvedPuzzle (answers : Seq[Answer[Int]]) : Option[Puzzle] =
			answers match {
				case AllArrays (grid) =>
					val squares = grid.foldLeft (Set.empty[Square]) {
						case (accum, (name, assignments)) =>
							
							accum |+| assignments.map {
								case (n, value) =>
								
								Square (
									name.name.charAt (0),
									n,
									assignments (n).some
									);
								}.toSet;
						}
					
					Puzzle (squares).point[Option];
					
				case _ =>
					None
				}
	}
}
