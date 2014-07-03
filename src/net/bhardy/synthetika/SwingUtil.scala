package net.bhardy.synthetika

import java.awt.event.{ActionListener, ActionEvent}
import javax.swing.JButton
import java.awt.Color

/**
  */
object SwingUtil {
  implicit def toWindowListener(h: ActionEvent => Unit) = new ActionListener {
    def actionPerformed(event: ActionEvent) = h(event)
  }

  implicit def toWindowListener2(h: => Unit) = new ActionListener {
    def actionPerformed(event: ActionEvent) = h
  }

  def button(label: String)(action: => Unit): JButton = {
    val button = new JButton(label)
    button.addActionListener(action)
    button
  }

  def randomColor = new Color(math.random.toFloat, math.random.toFloat, math.random.toFloat)

}