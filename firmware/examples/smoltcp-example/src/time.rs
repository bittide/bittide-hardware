use smoltcp::time::{Duration, Instant};

#[derive(Debug)]
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
pub struct Clock{
    global_time: u64,
    timer_last: u32,
    ticks_per_micro: u64,
    timer_now: *const u32
}

impl Clock {
    pub fn new(addr: usize, frequency: u64) -> Clock {

        let timer_last = unsafe { (addr as *const u32).read_volatile() };

        Clock{
            global_time:0,
            timer_last,
            ticks_per_micro: frequency / 10^6,
            timer_now: addr as *mut u32
        }
    }

    pub fn advance(&self, duration: Duration) {
        let now = self.get_ticks();
        let duration_ticks = duration.micros() * (self.ticks_per_micro);
        let target = now + (duration_ticks as u32);
        if target > now{
            let mut ticks = now;
            while ticks < target {
                ticks = self.get_ticks();
            }
        } else {
                let mut ticks = now;
                while ticks > now || ticks < target {
                    ticks = self.get_ticks();
                }
            }
        }
    // Gets the current number of ticks since the Clock was created.
    pub fn elapsed_ticks(&mut self) -> u64{
        let now = self.get_ticks();
        self.global_time = self.global_time + (tick_diffs(self.timer_last, now) as u64);
        self.timer_last = now;
        self.global_time
    }
    pub fn elapsed(&mut self) -> Instant{
        Instant::from_micros((self.elapsed_ticks() / self.ticks_per_micro) as i64)
    }

    // Gets the current current number of ticks in u32, this will overflow during runtime.
    fn get_ticks(&self) -> u32{
        unsafe {self.timer_now.read_volatile()}
    }
}

pub fn tick_diffs(old: u32, new : u32) -> u32{
    let mut ticks_since ;
    if old > new {
        ticks_since = !0 - old;
        ticks_since = ticks_since + new;
    } else {
        ticks_since = new - old;
    }
    ticks_since
}
