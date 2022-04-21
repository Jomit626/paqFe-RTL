package paqFe.util

import chisel3._
import chisel3.util._

object TreeReduce {
  def apply[T](items: Seq[T], f: (T, T) => T) : T = {
    val n = items.length
    n match {
      case 0 => items(-1)
      case 1 => items(0)
      case even if n % 2 == 0 => {
        TreeReduce((0 until (even - 1, 2)).map(i => f(items(i), items(i + 1))), f)
      }
      case odd => {
        TreeReduce((0 until (odd - 1, 2)).map(i => f(items(i), items(i + 1))) ++ Seq(items.last), f)
      }
    }
  }

  def apply[T](items: Seq[T], f: (T, T) => T, layerOp: (Int, T) => T) : T = {
    layer(0, items, f, layerOp)
  }

  private def layer[T](layerId: Int, items: Seq[T], f: (T, T) => T, layerOp: (Int,T) => T): T = {
    val newItems = items.length match {
      case 0 | 1 => items
      case even if items.length % 2 == 0 => {
        (0 until (even - 1, 2)).map(i => f(items(i), items(i + 1)))
      }
      case odd => {
        (0 until (odd - 1, 2)).map(i => f(items(i), items(i + 1))) ++ Seq(items.last)
      }
    }
    val itemsNxt = newItems map (layerOp(layerId + 1, _))
    itemsNxt.length match {
      case 0 => itemsNxt(-1)
      case 1 => itemsNxt(0)
      case _ => layer(layerId + 1, itemsNxt, f, layerOp)
    }
  }
}