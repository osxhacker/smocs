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
 * solve an arbitrary [[com.tubros.constraints.examples.sudoku.Puzzle] for the
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

				Set (
					as.slice (0, 3) ++
					bs.slice (0, 3) ++
					cs.slice (0, 3)
					).size === 9 &&
				Set (
					as.slice (3, 6) ++
					bs.slice (3, 6) ++
					cs.slice (3, 6)
					).size === 9 &&
				Set (
					as.slice (6, 9) ++
					bs.slice (6, 9) ++
					cs.slice (6, 9)
					).size === 9
		}
		
		
		override def apply (puzzle : Puzzle) : SolverError \/ Puzzle =
		{
			val answers = createSolver () {
				implicit solver =>
					
				for {
					variables <- createGridVariables (puzzle)
					_ <- solver.add (problemDefinition ())
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
								
							(puzzle (row, n + 1) >>= (_.assignment)) cata (
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
			@inline
			def rows (name : Char) : List[SudokuEquation] =
			{
				val row = VariableName (name.toString);
				val offsets = Puzzle.columnOffsets.toList;
				val permuted = offsets.map {
					n =>
						
					offsets.drop (n) ::: offsets.take (n);
					}
				val equations = permuted.flatMap {
					case head :: tail =>
						
					tail.map {
						n =>
							
						new SudokuEquation {
							def apply = row (head) !== row (n);
							}
						}
					}
				
				return (equations.toList);
			}
			
			@inline
			def columns (offset : Int) : List[SudokuEquation] =
			{
				val allRowNames = Puzzle.rowNames.toList;
				val permuted = (0 until allRowNames.length).map {
					n =>
						
					allRowNames.drop (n) ::: allRowNames.take (n);
					}
				val names = permuted.map {
					_.map (row => VariableName (row.toString));
					}
				val equations = names.flatMap {
					case head :: tail =>
						
					tail.map {
						row =>
							
						new SudokuEquation {
							def apply = head (offset) !== row (offset);
							}
						}
					}
				
				return (equations.toList);
			}

			Problem[Int] (
				Puzzle.rowNames.toList.flatMap (rows).toNel.get :::>
				Puzzle.columnOffsets.toList.flatMap (columns)
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
