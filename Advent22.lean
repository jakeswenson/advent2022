import Days
import Days.Day01
import Days.Day02
import Days.Day03
import Days.Day04
import Days.Day05
import Days.Day06

namespace Advent22

def days: List $ Days.Problem String  := [
  Days.Day01.solution.wrapString,
  Days.Day02.solution.wrapString,
  Days.Day03.solution.wrapString,
  Days.Day04.solution.wrapString,
  Days.Day05.solution,
  Days.Day06.solution.wrapString
]