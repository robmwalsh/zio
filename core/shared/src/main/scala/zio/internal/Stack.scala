/*
 * Copyright 2017-2020 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio.internal

/**
 * A very fast, growable/shrinkable, mutable stack.
 */
private[zio] final class Stack[A <: AnyRef]() {
  private[this] var array     = new Array[AnyRef](13)
  private[this] var stackSize = 0
  private[this] var nesting   = 0

  /**
   * Determines if the stack is empty.
   */
  def isEmpty: Boolean = stackSize == 0

  def size: Int = nesting * 12 + stackSize

  /**
   * Pushes an item onto the stack.
   */
  def push(a: A): Unit =
    if (stackSize == 13) {
      array = Array(array, a, null, null, null, null, null, null, null, null, null, null, null)
      stackSize = 2
      nesting += 1
    } else {
      array(stackSize) = a
      stackSize += 1
    }

  /**
   * Pops an item off the stack, or returns `null` if the stack is empty.
   */
  def pop(): A =
    if (stackSize <= 0) {
      null.asInstanceOf[A]
    } else {
      val idx = stackSize - 1
      var a   = array(idx)
      if (idx == 0 && nesting > 0) {
        array = a.asInstanceOf[Array[AnyRef]]
        a = array(12)
        array(12) = null // GC
        stackSize = 12
        nesting -= 1
      } else {
        array(idx) = null // GC
        stackSize = idx
      }
      a.asInstanceOf[A]
    }

  /**
   * Peeks the item on the head of the stack, or returns `null` if empty.
   */
  def peek(): A =
    if (stackSize <= 0) {
      null.asInstanceOf[A]
    } else {
      val idx = stackSize - 1
      var a   = array(idx)
      if (idx == 0 && nesting > 0) a = (a.asInstanceOf[Array[AnyRef]])(12)
      a.asInstanceOf[A]
    }

  //not at all optimised, needs work if used in any hot spots
  private[zio] def peekN(n: Int): List[A] =
    if (stackSize <= 0) {
      Nil
    } else {
      def loop(arr: Array[AnyRef], n: Int, idx: Int, acc: List[A]): List[A] =
        if (n == 0)
          acc
        else {
          val entry = arr(idx)
          if (entry eq null) acc
          else
            entry match {
              case arr: Array[AnyRef] =>
                loop(arr.asInstanceOf[Array[AnyRef]], n - 1, 11, arr(12).asInstanceOf[A] :: acc)
              case a: AnyRef =>
                loop(arr, n - 1, idx - 1, a.asInstanceOf[A] :: acc)
            }
        }

      val size2 = size
      loop(array, Math.min(n, size2), stackSize - 1, Nil)
    }

  def peekOrElse(a: A): A = if (stackSize <= 0) a else peek()
}

private[zio] object Stack {
  def apply[A <: AnyRef](): Stack[A] = new Stack[A]
  def apply[A <: AnyRef](a: A): Stack[A] = {
    val stack = new Stack[A]

    stack.push(a)

    stack
  }
}
