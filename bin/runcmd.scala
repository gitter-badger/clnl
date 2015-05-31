#!/bin/sh
exec scalas "$0" -q "$@"
!#

/***
  logLevel := Level.Error

  logLevel in Global := Level.Error

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

System.out.println("----")
val workspace = HeadlessWorkspace.newInstance
workspace.silent = true
workspace.openFromSource(url2String("file:resources/empty.nlogo"))

val commands = io.Source.stdin.getLines.mkString("\n")

workspace.runCompiledCommands(new api.SimpleJobOwner("test", workspace.world.mainRNG, api.AgentKind.Observer), workspace.compileCommands(commands, api.AgentKind.Observer))
mirror.Mirrorables.allMirrorables(workspace.world).map( x => {
  System.out.print("(")
  System.out.print(":" + x.kind)
  System.out.print(" ")
  System.out.print(x.agentKey.id)
  System.out.print(" (")
  x.kind.Variables.values.toSeq.map( k => {
    System.out.print("(:" + k + " ")
    System.out.print(x.getVariable(k.id) match {
      case s: java.lang.String => "\"s\""
      case d: java.lang.Double => d + "d0"
      case b: java.lang.Boolean => if(b)"T" else "NIL"
      case _: org.nlogo.api.ShapeList => ":SHAPELIST"
      case v => v
    })
    System.out.print(") ")
  })
  System.out.println(")")
  
  })

/*
workspace.runCompiledCommands(new api.SimpleJobOwner("test", workspace.world.mainRNG, api.AgentKind.Observer), workspace.compileCommands("random-seed 15", api.AgentKind.Observer))
for(_ <- 1 to 40)
  System.out.println(workspace.runCompiledReporter(new api.SimpleJobOwner("test", workspace.world.mainRNG, api.AgentKind.Observer), workspace.compileReporter("random-float 30")))

val m = new org.nlogo.util.MersenneTwisterFast();
m.setSeed(15);
for(_ <- 1 to 40)
  System.out.println(30d * m.nextDouble())*/

workspace.dispose
