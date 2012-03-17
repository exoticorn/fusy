package de.exoticorn.fusy

import javax.sound.sampled.{ AudioSystem, AudioFormat }

trait Generator {
  def run(output: Array[Float], inputs: Array[Array[Float]], numSamples: Int)
}

class SinOsci extends Generator {
  private var phase = 0.0

  def run(output: Array[Float], inputs: Array[Array[Float]], numSamples: Int) {
    val freq = inputs(0)
    val freqScale = math.Pi * 2 / 44100

    var i = 0
    while (i < numSamples) {
      output(i) = math.sin(phase).toFloat
      phase += freq(i) * freqScale
      i += 1
    }
  }
}

class AudioInterface {
  private val format = new AudioFormat(44100, 16, 1, true, false)
  private val line = AudioSystem.getSourceDataLine(format)

  private val buffer = new Array[Byte](256)

  def write(data: Array[Float]) {
    var p = 0
    val bufferSize = buffer.size / 2
    while (p < data.size) {
      val size = if (data.size > bufferSize) bufferSize else data.size
      var i = 0
      var j = 0
      while (i < size) {
        val v16 = (data(i + p) * 32768).toInt
        val vc16 = if (v16 < -32768) -32768 else if (v16 > 32767) 32767 else v16
        buffer(j) = v16.toByte
        buffer(j + 1) = (v16 >> 8).toByte
        i += 1
        j += 2
      }
      line.write(buffer, 0, size * 2)
      p += size
    }
  }

  def open() {
    line.open(format)
    line.start()
  }

  def close() {
    line.drain()
    line.close()
  }
}

object Main extends App {
  val audio = new AudioInterface
  audio.open()

  val freq = Array.fill(128)(440.0f)
  val inputs = Array(freq)
  val buffer = new Array[Float](128)
  val osci = new SinOsci

  for (i <- 1 to 400) {
    osci.run(buffer, inputs, 128)
    audio.write(buffer)
  }

  audio.close()
}
