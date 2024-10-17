use crate::callisto::ReframingState;

/// This is a wrapper type around the `debugRegisterWb` component. The fields are as
/// described:
/// 0. Reframing state (`ReframingState`):
///   - `0x00`: Discriminant
///   - `0x04`: Target correction (`f32`)
///   - `0x08`: Target count (`u32`)
#[derive(Clone)]
pub struct DebugRegister {
    base_addr: *const u32,
}

impl DebugRegister {
    pub unsafe fn from_base_addr(base_addr: *const u32) -> Self {
        DebugRegister { base_addr }
    }

    pub fn set_rf_state(&mut self, rf_state: ReframingState) {
        unsafe {
            self.base_addr
                .cast_mut()
                .cast::<ReframingState>()
                .write_volatile(rf_state);
        }
    }

    pub fn get_rf_state(&self) -> ReframingState {
        unsafe { self.base_addr.cast::<ReframingState>().read_volatile() }
    }
}
