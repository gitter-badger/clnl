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

workspace.mainRNG.setSeed(15)
workspace.runCompiledCommands(new api.SimpleJobOwner("test", workspace.world.mainRNG, api.AgentKind.Observer), workspace.compileCommands(commands, api.AgentKind.Observer))

workspace.world.exportWorld(new java.io.PrintWriter(System.out, true), true)
System.out.println(org.nlogo.headless.Checksummer.calculateChecksum(workspace.world.exportWorld(_, true)))
workspace.dispose
