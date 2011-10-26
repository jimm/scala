import javax.sound.midi.MidiSystem

class MidiTest {

  def run {
    println("device info:")
    MidiSystem.getMidiDeviceInfo.foreach (mdi => {
        println(mdi.getName() + ": " + mdi.getVendor() + ": " + mdi.getDescription())
        val md = MidiSystem.getMidiDevice(mdi)

      }
    )

    println
    println("receiver:")
    var r = MidiSystem.getReceiver()
    println(r)
    r.close()

    println
    println("transmitter:")
    try {
      var t = MidiSystem.getTransmitter()
      println(t)
      t.close()
    }
    catch {
      case e: Exception => println(e)
    }
  }

}

new MidiTest().run
