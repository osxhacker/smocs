/**
 * Created on: Dec 15, 2013
 */
package com.tubros.constraints.api

import java.lang.ref.WeakReference
import java.util.concurrent._


/**
 * The '''FastSymbol''' type is an optimized version of [[scala.Symbol]].  It
 * benchmarks at 3x faster for existing instance lookup and just under 2x
 * faster when creating a new '''FastSymbol'''.  Semantically, it is a "drop in"
 * replacement for [[scala.Symbol]].
 * 
 * @author svickers
 */
final class FastSymbol private (val name : String)
	extends Serializable
{
	override def equals (other : Any) : Boolean =
		this eq other.asInstanceOf[AnyRef]


	override def hashCode () : Int = name.hashCode ();


	override def toString : String = "'" + name;


	@throws (classOf[java.io.ObjectStreamException])
	private def readResolve () : Any = Symbol.apply (name);
}


object FastSymbol
{
	/// Instance Properties
	private val cache = new ConcurrentHashMap[String, WeakReference[FastSymbol]];


	def apply (sym : Symbol) : FastSymbol = apply (sym.name);
	
	
	def apply (name : String) : FastSymbol =
	{
		@inline
		def create (key : String, prior : WeakReference[FastSymbol])
			: FastSymbol =
		{
			val fs = new FastSymbol (key);
			val value = new WeakReference (fs);

			if (prior != null)
				cache.remove (key, prior);

			val replaced = cache.putIfAbsent (name, value);

			if (replaced != null)
				replaced.get;
			else
				fs;
		}

		val entry = cache.get (name);

        if (entry eq null)
            return (create (name, null));

        val sym = entry.get;

        if (sym eq null)
            return (create (name, entry));

        return (sym);
	}
	
	
	def unapply (that : AnyRef) : Option[FastSymbol] =
		that match {
			case fs : FastSymbol =>
				Some (fs);
			
			case Symbol (name) =>
				Some (FastSymbol (name));
				
			case _ =>
				None;
			}
}

