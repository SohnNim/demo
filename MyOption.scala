package com.sparrow.gittest.option

sealed abstract class MyOption

case object OptionA extends MyOption
case object OptionB extends MyOption
case class NoOpt(opt: String) extends MyOption
