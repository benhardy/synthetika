package net.bhardy.synthetika

import java.awt.{FlowLayout, Graphics, BorderLayout, Color}
import java.awt.image.BufferedImage
import java.util.concurrent.{Executors, ExecutorService}
import javax.swing._
import scala.Some

/**
 * Main GUI for Synthetika app
 */
class Synthetika extends JFrame("Synthetika") {

  import SwingUtil._

  var generator: Option[ExecutorService] = None
  var generating: Boolean = false

  val adsr_envelope1 = Envelope(0.01, 0.02, 0.05, 0.1)
  val waveform = Waveform((time: Double) => Generators.triangleWave(440 * 8)(time), adsr_envelope1)
  val sample = Sample(waveform = waveform, sampleRate = 16000, sampleTimeSecs = 1)

  setSize(600, 400)
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  getContentPane().setLayout(new BorderLayout)

  val generateButton = button("Generate") {
    generate(sample)
  }
  val listenButton = button("Listen") {
    Audio play sample
  }
  val canvas = new BufferedImage(600, 400, BufferedImage.TYPE_INT_RGB)
  val drawPanel = new JPanel {
    override def paintComponent(graphics: Graphics) = {
      graphics.drawImage(canvas, 0, 0, null)
    }
  }
  getContentPane().add(buildButtonPanel, BorderLayout.SOUTH)
  getContentPane().add(drawPanel, BorderLayout.CENTER)

  setJMenuBar(buildMenuBar)

  def buildMenuBar: JMenuBar = {
    val fileMenu = new JMenu("File")
    fileMenu.add(new JMenuItem("Quit", 'Q'))
    val newMenuBar = new JMenuBar
    newMenuBar.add(fileMenu)
    newMenuBar
  }

  def draw(sample: Sample): Unit = {
    val width = canvas.getWidth
    val height = canvas.getHeight
    val midHeight = height / 2
    val g = canvas.getGraphics
    g.setColor(Color.BLACK)
    g.fillRect(0, 0, width, height)
    val yScale = 50.00 // pixels per unit
    val xScale = 50.00
    val attenuation = 1.00 // y axis squishedness
    g.setColor(Color.YELLOW)
    g.drawLine(0, midHeight, width - 1, midHeight)
    g.setColor(Color.GREEN)
    var yPrev: Option[Int] = None
    val stream = sample.bufferStream
    for (sampleNumber <- 0 until sample.numSamples) {
      val xp = sampleNumber * width / sample.numSamples
      val t = sampleNumber.toDouble * sample.sampleTimeSecs / sample.numSamples
      val y = stream.readShort.toDouble / 16000
      val yp: Int = (midHeight + y * yScale * attenuation).toInt
      g.setColor(Color.GREEN)
      g.drawLine(xp, yPrev.getOrElse(yp), xp, yp)
      for (pp <- sample.postProcessors) {
        val levelp = pp(t)
        val yp2: Int = (midHeight - levelp * yScale * attenuation).toInt
        g.setColor(Color.BLUE)
        g.drawLine(xp, yp2, xp, yp2)

      }
      yPrev = Some(yp)
    }
    getContentPane.validate
    getContentPane.repaint
  }

  def generate(sample: Sample): Unit = {

    if (generating) {
      generator.map {
        _.shutdownNow
      }
      generating = false
      generateButton.setText("Generate")
    }
    else {
      generateButton.setText("Cancel")
      generating = true
      val genEx = Executors.newSingleThreadExecutor
      generator = Some(genEx)
      genEx.submit(sample.renderer {
        draw(sample)
        generateButton.setText("Generate")
        generating = false
      })
    }

  }

  def buildButtonPanel: JPanel = {
    val buttonPanel = new JPanel()
    buttonPanel.setLayout(new FlowLayout)
    buttonPanel.add(generateButton)
    buttonPanel.add(listenButton)
    buttonPanel
  }

}

object Synthetika {

  def main(args: Array[String]): Unit = {
    //audioSetup
    val window = new Synthetika
    window setVisible true
  }


}