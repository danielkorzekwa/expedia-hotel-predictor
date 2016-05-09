package expedia.stats

case class OnlineAvg() {

  private var sumValue = 0d
  private var numValue = 0d

  def add(x: Double) = {
    sumValue += x
    numValue += 1
  }

  def addWeighted(x: Double, weight: Double) = {
    sumValue += weight * x
    numValue += weight
  }

  def avg(): Double = sumValue / numValue
  def sum():Double = sumValue
}