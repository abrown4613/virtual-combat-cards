/*
 * Copyright (C) 2008-2012 - Thomas Santana <tms@exnebula.org>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>
 */
package vcc.util

import org.specs2.SpecificationWithJUnit
import org.specs2.mock.Mockito
import concurrent.SyncVar

class AsynchronousDispatcherTest extends SpecificationWithJUnit {
  def is =
    "  enqueue once one tasks" ! echo().e1 ^
      "must have observer before dispatching task" ! echo().observerMustBePresent ^
      "enqueue once several tasks" ! echo().e2 ^
      "enqueue two task and get execution in order" ! echo().e3 ^
      "enqueue twice then execute" ! echo().e4 ^
      "enqueue three fail second exeuction" ! echo().e5 ^
      end

  case class echo() extends Mockito {
    private val observer = mock[AsynchronousDispatcher.Observer[Int]]
    private val dispatcher = new AsynchronousDispatcher[Int]()
    dispatcher.setObserver(observer)
    private val barrier = new Barrier

    def e1 = {
      val task = new SimpleTask
      dispatcher.queueTasks(Seq(task))
      (there was no(observer).taskComplete(any[AsynchronousTask[Int]], any[Int])) and
        (there was no(observer).taskFailed(any[AsynchronousTask[Int]], any[Throwable]))
    }

    def e2 = {
      val task = new SimpleTask
      dispatcher.queueTasks(Seq(task, barrier))
      task.finish(10)
      barrier.waitForExecution()
      there was one(observer).taskComplete(task, 10)
    }

    def e3 = {
      val task1 = new SimpleTask
      val task2 = new SimpleTask
      dispatcher.queueTasks(Seq(task1, task2, barrier))
      task1.finish(10)
      task2.finish(20)
      barrier.waitForExecution()
      there was one(observer).taskComplete(task1, 10) then
        one(observer).taskComplete(task2, 20)
    }

    def e4 = {
      val task1 = new SimpleTask
      val task2 = new SimpleTask
      dispatcher.queueTasks(Seq(task1))
      dispatcher.queueTasks(Seq(task2, barrier))
      task2.finish(120)
      task1.finish(20)
      barrier.waitForExecution()
      there was one(observer).taskComplete(task1, 20) then
        one(observer).taskComplete(task2, 120)
    }

    def e5 = {
      val task = new SimpleTask
      val task2 = new SimpleTask
      val task3 = new SimpleTask
      dispatcher.queueTasks(Seq(task, task2, task3, barrier))
      task3.finish(110)
      val exception = new Exception("Boom!")
      task2.fail(exception)
      task.finish(21)
      barrier.waitForExecution()
      there was one(observer).taskComplete(task, 21) then
        one(observer).taskFailed(task2, exception) then
        one(observer).taskComplete(task3, 110)
    }

    def observerMustBePresent = {
      dispatcher.setObserver(null)
      dispatcher.queueTasks(Seq(new SimpleTask)) must throwA(new IllegalStateException("Observer not set"))
    }
  }

  class SimpleTask extends AsynchronousTask[Int] {
    private val result = new SyncVar[Either[Int, Throwable]]()

    def execute(): Int = {
      result.get match {
        case Left(v) => v
        case Right(e) => throw e
      }
    }

    def finish(value: Int) {
      result.set(Left(value))
    }

    def fail(exception: Exception) {
      result.set(Right(exception))
    }

  }

  class Barrier extends AsynchronousTask[Int] {
    private val barrier = new SyncVar[Boolean]()

    def execute(): Int = {
      barrier.set(true)
      0
    }

    def waitForExecution() {
      barrier.get
    }

  }

}