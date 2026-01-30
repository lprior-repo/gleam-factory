import gleeunit/should
import signal_handler

pub fn shutdown_signal_sigterm_exists_test() {
  let signal = signal_handler.Sigterm
  case signal {
    signal_handler.Sigterm -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn shutdown_signal_sigint_exists_test() {
  let signal = signal_handler.Sigint
  case signal {
    signal_handler.Sigint -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn signal_received_captures_sigterm_test() {
  let msg = signal_handler.SignalReceived(signal_handler.Sigterm)
  case msg {
    signal_handler.SignalReceived(signal_handler.Sigterm) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn signal_received_captures_sigint_test() {
  let msg = signal_handler.SignalReceived(signal_handler.Sigint)
  case msg {
    signal_handler.SignalReceived(signal_handler.Sigint) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn can_discriminate_sigterm_from_sigint_test() {
  let term = signal_handler.Sigterm
  let int_ = signal_handler.Sigint

  case term {
    signal_handler.Sigterm -> should.be_true(True)
    signal_handler.Sigint -> should.fail()
  }

  case int_ {
    signal_handler.Sigint -> should.be_true(True)
    signal_handler.Sigterm -> should.fail()
  }
}

pub fn signal_received_message_is_pattern_matchable_test() {
  let msg = signal_handler.SignalReceived(signal_handler.Sigterm)
  case msg {
    signal_handler.SignalReceived(sig) -> {
      case sig {
        signal_handler.Sigterm -> should.be_true(True)
        signal_handler.Sigint -> should.fail()
      }
    }
  }
}
