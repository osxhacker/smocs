/**
 * Created on: Apr 2, 2013
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

import api.solver.FiniteDiscreteDomain
import api.solver.error.SolverError
import core.internal.tree._

import model._


/**
 * The '''ProduceShippingSchedules''' type is where the example application
 * uses Smocs to solve the constraint satisfaction problem of maximizing the
 * [[com.tubros.constraints.examples.cargo.model.Cargo]] loaded into the
 * available [[com.tubros.constraints.examples.cargo.model.CargoVessel]]s.
 *
 * @author svickers
 *
 */
object ProduceShippingSchedules
	extends ((ShippingContainers, ShippingManifest) => Seq[String \/ Schedule])
		with ConstraintSolver[
			TreeFiniteDomainSolver[Int]#SolverState,
			TreeFiniteDomainSolver[Int]
			]
{
	/// Class Imports
	import algebraic._
	import std.anyVal._
	import syntax.bifunctor._
	
	
	/// Class Types
	type SolverType = TreeFiniteDomainSolver[Int]
	
	
	/// Instance Properties
	private implicit val monad = TreeFiniteDomainSolver.solverMonad[Int];
	private val errorMessage : SolverError => String = _.message;
	
	
	override def apply (
		containers : ShippingContainers,
		manifest : ShippingManifest
		)
		: Seq[String \/ Schedule] =
	{
		def loadVessel (
			cargoLoader : OptimalVesselSchedule,
			vessels : List[CargoVessel],
			manifest : ShippingManifest,
			assignments : Seq[String \/ Schedule]
			)
			: Seq[String \/ Schedule] =
			vessels match {
				case Nil =>
					assignments :+ -\/ ("No more vessels available");
					
				case lastOne :: Nil if (!manifest.isEmpty) =>
					assignments :+ (
						errorMessage <-: cargoLoader (lastOne, manifest.cargo)
						);
					
				case vessel :: tail if (!manifest.isEmpty) =>
					cargoLoader (vessel, manifest.cargo) match {
						case stop @ -\/ (error : SolverError) =>
							assignments :+ -\/ (errorMessage (error));
							
						case loaded @ \/- (schedule) =>
							loadVessel (
								cargoLoader,
								tail,
								manifest.loaded (schedule),
								assignments :+ loaded
								);
					}
					
				case _ =>
					assignments;
				}
		
		val loader = new OptimalVesselSchedule ();
		
		loadVessel (loader, containers.vessels ().to[List], manifest, Seq.empty);
	}
	
	
	override def createSolver () : TreeFiniteDomainSolver[Int] =
		new TreeFiniteDomainSolver[Int] (ImpactRankingPolicy[Int] ());
	
	
	override def domainOf (solver : SolverType, values : Seq[Int])
		: solver.DomainType[Int] =
		FiniteDiscreteDomain (values);
}
