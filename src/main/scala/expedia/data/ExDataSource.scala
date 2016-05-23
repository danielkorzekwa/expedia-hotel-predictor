package expedia.data

trait ExDataSource {

  def getAllClicks(): Seq[Click]

  def foreach(onClick: (Click) => Unit):Unit
}