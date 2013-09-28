/**
 * Created on: Apr 3, 2013
 */
package com.tubros.constraints.examples.cargo

import Predef.{
	any2stringadd => _,					// disable (x : Any) + "foo" conversions
	_
	}
import scala.language.higherKinds

import scalaz.{
	Ordering => _,
	_
	}

import com.tubros.constraints._

import api._
import api.problem._
import api.solver._
import api.solver.error._
import model._


/**
 * The '''ConstraintSolver''' type is a participant in the CAKE pattern used to
 * provide a [[com.tubros.constraints.examples.cargo.model.Schedule]] for the
 * example [[com.tubros.constraints.examples.cargo.LoadVessels]] program.
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
	trait LoadingEquation
		extends Equation[Int]
			with ArithmeticSupport[Int]
			with DerivedValueSupport[Int]
            with PropositionalSupport[Int]
			with RelationalSupport[Int]
	
	
	object HasCargo
		extends LoadingEquation
	{
		override def apply = 'anchors + 'guitars + 'pianos + 'plates > 0;
	}
	
	
	object CargoLoaded
		extends LoadingEquation
	{
		override def apply =
			'cargoLoaded := 'anchors + 'guitars + 'pianos + 'plates;
	}
	
	
	case class VesselCapacity (
		vessel : CargoVessel,
		anchors : Volume,
		guitars : Volume,
		pianos : Volume,
		plates : Volume
		)
		extends LoadingEquation
	{
		override def apply = vessel.capacity.area > (
			'anchors * anchors.area +
			'guitars * guitars.area +
			'pianos * pianos.area +
			'plates * plates.area
			)
	}
	
	
	/**
	 * The '''OptimalVesselSchedule''' type defines the `public` API for
	 * driving the example ''CSP'' solver.  The `M` monad is required by the
	 * [[com.tubros.constraints.api.solver.Solver]] (documented there) and
	 * the `Ordering[Vector[Answer[Int]]]` is used to encapsulate how the
	 * "optimal" [[com.tubros.constraints.api.solver.Answer]] is determined.
	 */
	class OptimalVesselSchedule ()
		(implicit M : Monad[M], O : Ordering[Vector[Answer[Int]]])
	{
		/// Class Imports
		import NonEmptyList._
		import std.vector._
		import syntax.monad._
		import syntax.std.option._
		
		
		def apply (vessel : CargoVessel, cargo : Seq[Cargo])
			: SolverError \/ Schedule =
		{
			import ShippingManifest._
				
			implicit val categorizedCargo = cargo groupBy (_.name);
			val problem = Problem (
				HasCargo,
				CargoLoaded,
				VesselCapacity (
					vessel,
					Anchor.volume,
					Guitar.volume,
					Piano.volume,
					Plate.volume
					)
				);
			val answers = createSolver () {
				implicit solver =>

				for {
					anchors <- cargoVariable ('anchors)
					guitars <- cargoVariable ('guitars)
					pianos <- cargoVariable ('pianos)
					plates <- cargoVariable ('plates)
					_ <- solver.add (problem)
					solutions <- solver.run[Vector]
					} yield solutions.sorted;
				}
			
			answers.map (_.head.map (_.toTuple).toMap).map {
				best =>
					
				Schedule (
					vessel,
					Anchor (best ('anchors)),
					Guitar (best ('guitars)),
					Piano (best ('pianos)),
					Plate (best ('plates))
					);
				}
		}
		
		
		private def cargoVariable (name : Symbol)
			(implicit solver : SolverT, present : Map[Symbol, Seq[Cargo]])
			=
			solver.newVar (name, domainOf (solver, 0 to maxNumberOf (name)));
		
		
		private def maxNumberOf (name : Symbol)
			(implicit present : Map[Symbol, Seq[Cargo]])
			: Int =
			present.get (name).map (_.size) | 0;
	}
}
