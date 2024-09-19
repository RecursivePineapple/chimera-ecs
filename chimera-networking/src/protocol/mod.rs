use std::time::SystemTime;


pub mod transmit;
pub mod receive;

// #[cfg(test)]
// mod transmit_tests;

pub trait TransmitProtocol {

}

pub trait ReceiveProtocol {

}

pub trait Protocol {
    type Transmit: TransmitProtocol;
    type Receive: ReceiveProtocol;
}

pub(crate) fn now_unix_epoch_ns() -> u128 {
    SystemTime::UNIX_EPOCH.elapsed().unwrap().as_nanos()
}
