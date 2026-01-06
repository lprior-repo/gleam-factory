import gleeunit/should
import golden_master

pub fn start_link_returns_ok_subject_test() {
  golden_master.start_link("/tmp/golden")
  |> should.be_ok
}
