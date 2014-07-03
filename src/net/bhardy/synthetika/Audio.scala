package net.bhardy.synthetika

import javax.sound.sampled.{DataLine, AudioSystem, SourceDataLine}

/**
  */
object Audio {

  def play(sample:Sample): Unit = {
    System.out.println("playing audio")
    val dataLineInfo = new DataLine.Info(classOf[SourceDataLine], sample.audioFormat)
    val sourceDataLine =  AudioSystem.getLine(dataLineInfo).asInstanceOf[SourceDataLine]
    sourceDataLine.open(sample.audioFormat)
    sourceDataLine.start()
    // TODO fix demeter problems here
    sourceDataLine.write(sample.byteBuf.array(), 0, sample.byteBuf.array.length)
    sourceDataLine.drain
    sourceDataLine.stop
    sourceDataLine.close
  }
}
