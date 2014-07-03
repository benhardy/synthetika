package net.bhardy.synthetika

import java.nio.ByteBuffer
import javax.sound.sampled.AudioFormat
import scala.annotation.tailrec
import java.io.{DataInputStream, ByteArrayInputStream, DataOutputStream, ByteArrayOutputStream}

/**
  */

case class Envelope(attack: Double, decay: Double, sustain: Double, release: Double)
extends (Double=>Double) {

  val length = attack + decay + sustain + release

  def apply(time: Double): Double = {
    if (time < attack) {
      time / attack
    } else {
      val postAttackTime = time - attack
      if (postAttackTime < decay) {
        1.0 - 0.25 * (postAttackTime / decay)
      } else {
        val postDecayTime = postAttackTime - decay
        if (postDecayTime < sustain) {
          0.75
        } else {
          val postSustainTime = postDecayTime - sustain
          if (postSustainTime < release) {
            0.75 * (1 - postSustainTime / release)
          }
          else {
            0.0
          }
        }
      }
    }
  }
}

case class Waveform(generator: Double => Double, envelope: Envelope)

case class Sample(waveform: Waveform,
                  sampleRate: Int = 44100,
                  sampleTimeSecs: Int = 3) {

  val SAMPLE_RATE = 16000
  //44100
  val SAMPLE_SIZE_BITS = 16
  val CHANNELS = 1
  val numSamples = sampleTimeSecs * sampleRate
  val byteBuf = ByteBuffer.allocate(sampleTimeSecs * sampleRate * SAMPLE_SIZE_BITS / 8)

  val audioFormat = new AudioFormat(sampleRate,
    SAMPLE_SIZE_BITS,
    CHANNELS,
    true,
    true)

  val postProcessors: List[Double=>Double] = List(waveform.envelope)

  def renderer(onComplete: =>Unit): Runnable = {
    def processed(time:Double) = {
      postProcessors.foldLeft(waveform.generator(time)) {
        (value:Double, proc:Double=>Double) =>
          value * proc(time)
      }
    }

    new Runnable {
      override def run {
        val bos = new ByteArrayOutputStream(byteBuf.array.length)
        val writer = new DataOutputStream(bos)
        println(s"Generating ${numSamples} samples")
        for (pos <- 0 until numSamples) {
          val time = pos.toDouble / sampleRate
          val amp = processed(time)
          val y = (16000 * amp).toShort
          writer.writeShort(y) // rely on java's big-endianess
          if (pos % 1000 == 0) {
            System.out.print("%.3f\r".format(pos * 100f / numSamples))
          }

        }
        System.out.println("100%                     ")
        val result: Array[Byte] = bos.toByteArray
        for (i <- 0 until byteBuf.array().length) {
          byteBuf.array(i) = result(i)
        }
        onComplete
      }
    }
  }

  def bufferStream = new DataInputStream(new ByteArrayInputStream(byteBuf.array()) )
}

object Generators {
  def triangleWave_1hz(time: Double): Double = {
    val tp = time / (2 * math.Pi)
    val accuracy = 1000

    @tailrec
    def accumulate(total:Double, pos:Int):Double = {
      if (pos > accuracy) {
        total
      } else {
        val bump = math.sin(tp * pos) / pos
        accumulate(total + bump, pos + 1)
      }
    }
    accumulate(0.0, 1)
  }

  def triangleWave(frequency: Double)(time: Double) = triangleWave_1hz(time * frequency)

}