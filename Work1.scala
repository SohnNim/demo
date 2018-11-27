package com.sparrow.gittest.works

import com.sparrow.gittest.option.{MyOption, OptionA, OptionB}

case class Work1(data: List[Int], option: MyOption) {

  def filterHalf: Work1 = {
    option match {
      case OptionA => {
        val oddFilteredData = data.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)
        this.copy(data = oddFilteredData)
      }
      case OptionB => {
        val evenFilteredData = data.zipWithIndex.filter(_._2 % 2 == 1).map(_._1)
        this.copy(data = evenFilteredData)
      }
      case _ => this
    }
  }

  def transData: Work1 = {
    option match {
      case OptionA => {
        val oneAddedData = data.map(_ + 1)
        this.copy(data = oneAddedData)
      }
      case OptionB => {
        val twoAddedData = data.map(_ + 2)
        this.copy(data = twoAddedData)
      }
      case _ => this
    }
  }

}
