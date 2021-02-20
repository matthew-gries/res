/// Determine if the page of the two given addresses are the same. The page
/// is defined as the high order byte of an address
///
/// Arguments:
/// * `addr1` (`u16`): the first address
/// * `addr2` (`u16`): the other address
///
/// Return (`bool`): `true` if the address are in the same page, `false` if they are not
pub fn check_if_page_crossed(addr1: u16, addr2: u16) -> bool {
    // check if the high bytes are different
    let addr1_high = addr1 & 0xFF00;
    let addr2_high = addr2 & 0xFF00;
    addr1_high != addr2_high
}
