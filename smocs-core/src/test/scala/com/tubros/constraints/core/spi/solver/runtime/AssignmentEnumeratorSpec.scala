/**
 * Created on: Mar 17, 2013
 */
package com.tubros.constraints.core.spi.solver.runtime

import scala.language.{
	higherKinds,
	postfixOps
	}

import scalaz._
import scalaz.iteratee._

import org.junit.runner.RunWith
import org.scalamock.scalatest.MockFactory
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import com.tubros.constraints.api
import com.tubros.constraints.core.spi

import api._
import api.solver._
import spi.solver._


/**
 * The '''AssignmentEnumeratorSpec''' type defines the expected behaviour of
 * the [[com.tubros.constraints.core.spi.solver.runtime.AssignmentEnumerator]]
 * `runtime` type.
 *
 * @author svickers
 *
 */
@RunWith (classOf[JUnitRunner])
class AssignmentEnumeratorSpec
	extends ProjectSpec
{
	/// Class Imports
	import std.list._
	import std.stream._
	import EnumerateeT._
	import IterateeT._
	
	
	/// Class Types
	type ElementType = AssignmentEnumerator[Int, Stream]#AssignmentElementType
	type ContextType = AssignmentEnumerator[Int, Stream]#ContextType
	type OptionStateType[T] = AssignmentEnumerator[Int, Option]#StateType[T]
	type StreamStateType[T] = AssignmentEnumerator[Int, Stream]#StateType[T]
	
	
	/// Testing Collaborators
	val candidate = DiscreteVariable ('z, FiniteDiscreteDomain (1, 2, 3));
	val existingAssignments = List[VariableName] ('a, 'b, 'c).zipWithIndex.map (
		Answer.fromTuple
		);
		
		
	"An AssignmentEnumerator" should "support construction with an Iteratee" in
	{
		val enumerator = AssignmentEnumerator[Int, Stream] (
			steps = emitAllAssignments ()
			);
	}
	
	it should "produce assignments for a Variable" in
	{
		val assignments = AssignmentEnumerator[Int, Stream] ().generate (
			existingAssignments,
			candidate
			);
		
		assignments should have size (candidate.domain.size);
		assignments foreach {
			assignment =>
				
			assignment should have size (1);
			assignment.last.name should be === (candidate.name);
			candidate.domain should contain (assignment.last.value);
			}
	}
	
	it should "support producing no assignments for a Variable" in
	{
		/// The steps defined here simulate a sequence of operations which
		/// end up not producing any assignments.
		val assignments = AssignmentEnumerator[Int, List] (
			steps = emitFirstAssignment ().map (_.filter (_ => false))
			).generate (existingAssignments, candidate);
		
		assignments should be ('empty);
	}
	
	it should "be able to produce more assignments than one-per-Variable" in
	{
		val assignments = AssignmentEnumerator[Int, Stream] (
			steps = emitAllAssignmentsAndAdd ((n : Int) => Answer ('zz, n * n))
			).generate (existingAssignments, candidate);
		
		assignments should have size (candidate.domain.size);
		assignments foreach {
			assignment =>
				
			/// The enumerator added 'zz to each variable assignment
			assignment should have size (2);
			
			val mapping = assignment.map (_.toTuple).toMap;
			
			mapping.keySet should be === (Set (candidate.name, 'zz));
			candidate.domain.contains (mapping (candidate.name)) should be === (
				true
				);
			
			mapping ('zz) should be === (
				mapping (candidate.name) * mapping (candidate.name)
				);
			}
	}
	
	it should "allow for varying IterateeT steps" in
	{
		val assignments = AssignmentEnumerator[Int, List] (
			steps = emitFirstAssignment ()
			).generate (existingAssignments, candidate);
		
		assignments should have size (1);
		assignments.head should have size (1);
	}


	private def emitAllAssignments () =
		collectT[Seq[ElementType], StreamStateType, Stream];
	
	
	private def emitAllAssignmentsAndAdd (f : Int => ElementType) =
		emitAllAssignments ().map (_.map (seq => seq :+ f (seq.last.value)));


	private def emitFirstAssignment () =
		head[Seq[ElementType], OptionStateType].map (_.toList);
}
