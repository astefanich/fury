/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.8.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.                                         ║
   ║                                                                                                           ║
   ║ The primary distribution site is: https://propensive.com/                                                 ║
   ║                                                                                                           ║
   ║ Licensed under  the Apache License,  Version 2.0 (the  "License"); you  may not use  this file  except in ║
   ║ compliance with the License. You may obtain a copy of the License at                                      ║
   ║                                                                                                           ║
   ║     http://www.apache.org/licenses/LICENSE-2.0                                                            ║
   ║                                                                                                           ║
   ║ Unless required  by applicable law  or agreed to in  writing, software  distributed under the  License is ║
   ║ distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. ║
   ║ See the License for the specific language governing permissions and limitations under the License.        ║
   ╚═══════════════════════════════════════════════════════════════════════════════════════════════════════════╝
*/
package fury.ogdl

import probably._

import scala.collection.immutable.{SortedSet, TreeSet}
import scala.language.implicitConversions
import scala.util.{Try, Success}

object OgdlWriterTest extends TestApp {
  private[this] val empty = Ogdl(Vector())

  private case class Foo(bar: String, baz: Int)
  private implicit val ord: Ordering[Foo] = Ordering[Int].on[Foo](_.baz)

  private case class Quux(handle: String, data: SortedSet[String])

  override def tests(): Unit = {
    test("string") {
      val input: String = "Hello World!"
      Try(Ogdl(input))
    }.assert(_ == Success(Ogdl(Vector(("Hello World!", empty)))))

    test("case class") {
      val input = Foo(bar = "1", baz = 2)
      Try(Ogdl(input))
    }.assert(_ == Success(Ogdl(Vector(
      ("bar",Ogdl(Vector(("1", empty)))),
      ("baz",Ogdl(Vector(("2", empty))))
    ))))

    test("list of strings") {
      val input = List("3", "2", "1")
      Try{Ogdl(input)}
    }.assert(_ == Success(
      Ogdl(Vector(("3",Ogdl(Vector(("3",empty)))), ("2",Ogdl(Vector(("2",empty)))), ("1",Ogdl(Vector(("1",empty))))))
    ))

    test("sorted set of integers") {
      val input: SortedSet[Int] = TreeSet(1, 2, 3)
      Try{Ogdl(input)}
    }.assert(_ == Success(Ogdl(Vector(("1",Ogdl(Vector(("1",empty)))), ("2",Ogdl(Vector(("2",empty)))), ("3",Ogdl(Vector(("3",empty))))))))

    test("sorted set of case classes") {
      implicit val index: Index[Foo] = FieldIndex("bar")
      val input: SortedSet[Foo] = TreeSet(Foo(bar = "A", baz = 2), Foo(bar = "B", baz = 3), Foo(bar = "B", baz = 1))
      Try{Ogdl(input)}
    }.assert(_ == Success(Ogdl(Vector(
      ("B",Ogdl(Vector(("baz",Ogdl(Vector(("1",empty))))))),
      ("A",Ogdl(Vector(("baz",Ogdl(Vector(("2",empty))))))),
      ("B",Ogdl(Vector(("baz",Ogdl(Vector(("3",empty)))))))
    ))))

    test("case class with a sorted set") {
      implicit val index: Index[Quux] = FieldIndex("handle")
      val input: Quux = Quux("Q", TreeSet("A", "B", "C"))
      Try{Ogdl(input)}
    }.assert(_ == Success(Ogdl(Vector(
      ("handle",Ogdl(Vector(("Q",empty)))),
      ("data",Ogdl(Vector(
        ("A",Ogdl(Vector(("A",empty)))),
        ("B",Ogdl(Vector(("B",empty)))),
        ("C",Ogdl(Vector(("C",empty))))
      )))
    ))))
  }
}
