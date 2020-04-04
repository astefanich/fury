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
package fury

import fury.io._, fury.utils._

import probably._
import fury.core._
import fury.model._
import fury.ogdl._

import scala.collection.immutable.TreeSet
import scala.util.{Success, Try}
import scala.language.implicitConversions

object OgdlTest extends TestApp {
  private[this] val empty = Ogdl(Vector())

  override def tests(): Unit = {
    test("write permission") {
      val input = Permission("permission.name1:target1", None)
      Try{Ogdl(input)}
    }.assert(_ == Success(Ogdl(Vector(("id",Ogdl(Vector(("permission.name1:target1",empty)))), ("action",Ogdl(Vector(("None",empty))))))))

    test("write grant") {
      val input = Grant(DirectoryScope("xxx"), Permission("permission.name1:target1", Some("arg")))
      Try{Ogdl(input)}
    }.assert(_ == Success(Ogdl(Vector(
      ("scope",Ogdl(Vector(("DirectoryScope",Ogdl(Vector(("xxx",empty))))))),
      ("permission",Ogdl(Vector(
        ("id",Ogdl(Vector(("permission.name1:target1",empty)))),
        ("action",Ogdl(Vector(("Some",Ogdl(Vector(("arg",empty)))))))
      )))
    ))))

    test("write set of grants") {
      val input: TreeSet[Grant] = TreeSet(
        Grant(DirectoryScope("xxx"), Permission("permission.name1:target1", None)),
        Grant(ProjectScope(ProjectId("quux")), Permission("permission.name2:target2", Some("arg")))
      )
      val x  =Try{Ogdl(input)}
      println(x)
      x
      //Success(Ogdl(Vector((id,Ogdl(Vector((scope,Ogdl(Vector((ProjectScope,Ogdl(Vector((quux,Ogdl(Vector()))))))))))), (id,Ogdl(Vector((scope,Ogdl(Vector((DirectoryScope,Ogdl(Vector((xxx,Ogdl(Vector()))))))))))))))
    }.assert(_ == Success(Ogdl(Vector(
      ("permission.name2:target2", Ogdl(Vector(("scope",Ogdl(Vector(("ProjectScope",Ogdl(Vector(("quux",empty)))))))))),
      ("permission.name1:target1", Ogdl(Vector(("scope",Ogdl(Vector(("DirectoryScope",Ogdl(Vector(("xxx",empty))))))))))
    ))))

    test("write layer with empty project") {
      val input = Layer(version = Layer.CurrentVersion, projects = TreeSet(Project(id = ProjectId("scala"))))
      Try{Ogdl(input)}
    }.assert(_ == Success(Ogdl(Vector(
      ("version",Ogdl(Vector(("6",empty)))),
      ("projects",Ogdl(Vector(
        ("scala",Ogdl(Vector(("",empty))))
      )))
    ))))

  }
}
