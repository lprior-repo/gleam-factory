import gleeunit/should
import golden_master
import types

pub fn start_link_returns_ok_subject_test() {
  golden_master.start_link("/tmp/golden")
  |> should.be_ok
}

pub fn accepts_git_hash_in_constructor_test() {
  let assert Ok(hash) =
    types.git_hash_parse("a1b2c3d4e5f6789012345678901234567890abcd")

  golden_master.new_with_hash("/tmp/golden", hash)
  |> should.be_ok
}
