#!/bin/sh
exec scalas "$0" "$@"
!#

/***
  scalaVersion := "2.10.3"

  libraryDependencies ++= Seq(
    "asm" % "asm-all" % "3.3.1",
    "org.picocontainer" % "picocontainer" % "2.13.6",
    "org.nlogo" % "NetLogo" % "6.0.0-M3" from "http://ccl.northwestern.edu/devel/6.0.0-M3/NetLogo.jar",
    "org.nlogo" % "NetLogoHeadless" % "6.0.0-M3" from "http://ccl.northwestern.edu/devel/6.0.0-M3/NetLogoHeadless.jar"
  )
*/

import org.nlogo.headless.HeadlessWorkspace
import org.nlogo.mirror
import org.nlogo.api
import org.nlogo.nvm
import org.nlogo.util.Utils.url2String

val workspace = HeadlessWorkspace.newInstance
workspace.silent = true
workspace.openFromSource(url2String(api.ModelReader.emptyModelPath))
workspace.runCompiledCommands(new api.SimpleJobOwner("test", workspace.world.mainRNG, api.AgentKind.Observer), workspace.compileCommands("resize-world -2 2 -2 2 crt 1", api.AgentKind.Observer))
mirror.Mirrorables.allMirrorables(workspace.world).map( x => {
  System.out.print("(")
  System.out.print(x.kind)
  System.out.print(" ")
  System.out.print(x.agentKey.id)
  System.out.print(" (")
  x.kind.Variables.values.toSeq.map( k => {
    System.out.print("(" + k + " ")
    x.getVariable(k.id) match {
      case s: java.lang.String => System.out.print("\"s\"")
      case v => System.out.print(v)
    }
    System.out.print(") ")
  })
  System.out.println(")")
  
  })

workspace.dispose
