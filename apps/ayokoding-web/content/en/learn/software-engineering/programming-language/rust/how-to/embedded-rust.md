---
title: "Embedded Rust"
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 1000017
description: Practical guide to developing embedded systems with Rust
tags:
  [
    "rust",
    "how-to",
    "embedded",
    "no-std",
    "microcontroller",
    "iot",
    "bare-metal",
  ]
---

**Need to write Rust for embedded systems?** This guide covers no_std environments, HAL usage, interrupt handling, and embedded-specific patterns.

## Problem: Setting Up no_std Environment

### Scenario

You're targeting a microcontroller without an operating system.

### Solution: Use no_std and Core Library

**Cargo.toml**:

```toml
[dependencies]
# No std library
cortex-m = "0.7"
cortex-m-rt = "0.7"
panic-halt = "0.2"
```

**src/main.rs**:

```rust
#![no_std]
#![no_main]

use panic_halt as _;
use cortex_m_rt::entry;

#[entry]
fn main() -> ! {
    // Embedded programs never return
    loop {
        // Main loop
    }
}
```

**Memory layout** (`.cargo/config.toml`):

```toml
[target.thumbv7em-none-eabihf]
runner = "arm-none-eabi-gdb"
rustflags = [
  "-C", "link-arg=-Tlink.x",
]

[build]
target = "thumbv7em-none-eabihf"
```

---

## Problem: Blinking an LED

### Scenario

Classic "Hello World" for embedded - blink an LED.

### Solution: Use HAL Crate

```toml
[dependencies]
stm32f4xx-hal = { version = "0.14", features = ["stm32f407"] }
cortex-m = "0.7"
cortex-m-rt = "0.7"
panic-halt = "0.2"
```

```rust
#![no_std]
#![no_main]

use panic_halt as _;
use cortex_m_rt::entry;
use stm32f4xx_hal::{pac, prelude::*};

#[entry]
fn main() -> ! {
    let dp = pac::Peripherals::take().unwrap();
    let cp = cortex_m::Peripherals::take().unwrap();

    // Configure system clock
    let rcc = dp.RCC.constrain();
    let clocks = rcc.cfgr.freeze();

    // Configure LED pin
    let gpioa = dp.GPIOA.split();
    let mut led = gpioa.pa5.into_push_pull_output();

    // Configure delay
    let mut delay = cp.SYST.delay(&clocks);

    loop {
        led.set_high();
        delay.delay_ms(1000_u32);
        led.set_low();
        delay.delay_ms(1000_u32);
    }
}
```

---

## Problem: Reading Digital Input

### Scenario

You need to read button presses.

### Solution: Configure GPIO as Input

```rust
use stm32f4xx_hal::{pac, prelude::*};

#[entry]
fn main() -> ! {
    let dp = pac::Peripherals::take().unwrap();

    let gpioa = dp.GPIOA.split();
    let gpioc = dp.GPIOC.split();

    let mut led = gpioa.pa5.into_push_pull_output();
    let button = gpioc.pc13.into_pull_up_input();

    loop {
        if button.is_low() {
            led.set_high();
        } else {
            led.set_low();
        }
    }
}
```

---

## Problem: Serial Communication (UART)

### Scenario

You need to send/receive data via UART.

### Solution: Configure Serial Peripheral

```rust
use stm32f4xx_hal::{pac, prelude::*, serial::{config::Config, Serial}};
use core::fmt::Write;

#[entry]
fn main() -> ! {
    let dp = pac::Peripherals::take().unwrap();

    let rcc = dp.RCC.constrain();
    let clocks = rcc.cfgr.freeze();

    let gpioa = dp.GPIOA.split();
    let tx = gpioa.pa2.into_alternate();
    let rx = gpioa.pa3.into_alternate();

    let mut serial = Serial::new(
        dp.USART2,
        (tx, rx),
        Config::default().baudrate(115200.bps()),
        &clocks,
    ).unwrap();

    loop {
        writeln!(serial, "Hello from Rust!").unwrap();

        // Echo received bytes
        if let Ok(byte) = serial.read() {
            serial.write(byte).unwrap();
        }
    }
}
```

---

## Problem: Interrupt Handling

### Scenario

You need to respond to hardware interrupts.

### Solution: Define Interrupt Handlers

```rust
use cortex_m::interrupt::Mutex;
use core::cell::RefCell;
use stm32f4xx_hal::pac::{interrupt, Interrupt, NVIC};

static COUNTER: Mutex<RefCell<u32>> = Mutex::new(RefCell::new(0));

#[entry]
fn main() -> ! {
    let dp = pac::Peripherals::take().unwrap();
    let mut cp = cortex_m::Peripherals::take().unwrap();

    // Configure timer for interrupts
    // (HAL-specific configuration)

    // Enable interrupt
    unsafe {
        NVIC::unmask(Interrupt::TIM2);
    }

    loop {
        cortex_m::asm::wfi();  // Wait for interrupt
    }
}

#[interrupt]
fn TIM2() {
    cortex_m::interrupt::free(|cs| {
        let mut counter = COUNTER.borrow(cs).borrow_mut();
        *counter += 1;
    });

    // Clear interrupt flag
    // (HAL-specific)
}
```

---

## Problem: I2C Communication

### Scenario

You need to communicate with I2C sensors.

### Solution: Use I2C Peripheral

```rust
use stm32f4xx_hal::{pac, prelude::*, i2c::I2c};

#[entry]
fn main() -> ! {
    let dp = pac::Peripherals::take().unwrap();

    let rcc = dp.RCC.constrain();
    let clocks = rcc.cfgr.freeze();

    let gpiob = dp.GPIOB.split();
    let scl = gpiob.pb8.into_alternate_open_drain();
    let sda = gpiob.pb9.into_alternate_open_drain();

    let mut i2c = I2c::new(
        dp.I2C1,
        (scl, sda),
        400.kHz(),
        &clocks,
    );

    // Example: Read from I2C device at address 0x76
    let mut buffer = [0u8; 2];
    i2c.write_read(0x76, &[0x00], &mut buffer).unwrap();

    loop {
        // Process sensor data
    }
}
```

---

## Problem: PWM Output

### Scenario

You need to generate PWM signals (e.g., servo control, LED dimming).

### Solution: Configure Timer for PWM

```rust
use stm32f4xx_hal::{pac, prelude::*, timer::Channel};

#[entry]
fn main() -> ! {
    let dp = pac::Peripherals::take().unwrap();

    let rcc = dp.RCC.constrain();
    let clocks = rcc.cfgr.freeze();

    let gpioa = dp.GPIOA.split();
    let pin = gpioa.pa8.into_alternate();

    let mut pwm = dp.TIM1.pwm_hz(pin, 1.kHz(), &clocks);
    let max_duty = pwm.get_max_duty();

    pwm.enable(Channel::C1);

    // Set duty cycle to 50%
    pwm.set_duty(Channel::C1, max_duty / 2);

    loop {
        // Adjust PWM as needed
    }
}
```

---

## Problem: Real-Time Operating System

### Scenario

You need task scheduling and concurrency.

### Solution: Use RTIC (Real-Time Interrupt-driven Concurrency)

```toml
[dependencies]
rtic = "2.0"
cortex-m = "0.7"
```

```rust
#![no_std]
#![no_main]

use panic_halt as _;

#[rtic::app(device = stm32f4::stm32f407)]
mod app {
    use stm32f4xx_hal::prelude::*;

    #[shared]
    struct Shared {
        counter: u32,
    }

    #[local]
    struct Local {
        led: PA5<Output<PushPull>>,
    }

    #[init]
    fn init(cx: init::Context) -> (Shared, Local) {
        let dp = cx.device;
        let gpioa = dp.GPIOA.split();
        let led = gpioa.pa5.into_push_pull_output();

        (
            Shared { counter: 0 },
            Local { led },
        )
    }

    #[task(local = [led], shared = [counter])]
    fn blink(cx: blink::Context) {
        cx.local.led.toggle();
    }
}
```

---

## Problem: Low Power Modes

### Scenario

You need to minimize power consumption.

### Solution: Use Sleep Modes

```rust
use cortex_m::asm;

loop {
    // Do work
    process_sensor_data();

    // Enter sleep mode until next interrupt
    asm::wfi();  // Wait For Interrupt
}
```

**Deep sleep**:

```rust
use cortex_m::peripheral::SCB;

fn enter_deep_sleep() {
    let scb = unsafe { &*SCB::PTR };
    scb.set_sleepdeep();
    cortex_m::asm::wfi();
}
```

---

## Problem: Panic Handling

### Scenario

You need custom panic behavior for debugging.

### Solution: Implement Panic Handler

**Simple halt**:

```rust
use panic_halt as _;  // Halts on panic
```

**Print panic message** (with semihosting):

```toml
[dependencies]
panic-semihosting = "0.6"
```

```rust
use panic_semihosting as _;
```

**Custom panic handler**:

```rust
use core::panic::PanicInfo;

#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    // Log to UART, flash LED, etc.
    loop {
        cortex_m::asm::bkpt();  // Breakpoint for debugger
    }
}
```

---

## Problem: Flash Memory Access

### Scenario

You need to store data in flash memory.

### Solution: Use Flash Driver

```rust
use stm32f4xx_hal::flash::{FlashExt, LockedFlash};

fn write_to_flash() {
    let dp = pac::Peripherals::take().unwrap();

    let mut flash = dp.FLASH.constrain();
    let mut unlocked_flash = flash.unlocked();

    // Erase sector
    unlocked_flash.erase(8).unwrap();

    // Write data
    let data = [0x12, 0x34, 0x56, 0x78];
    unlocked_flash.program(0x0800_0000, &data).unwrap();
}
```

---

## Common Pitfalls

### Pitfall 1: Stack Overflow

**Problem**: Insufficient stack size.

**Solution**: Increase stack in link script.

```ld
_stack_size = 0x2000;  /* 8KB stack */
```

### Pitfall 2: Unbounded Delays

**Problem**: Using delay in interrupt handler.

**Solution**: Use state machines or timers instead.

### Pitfall 3: Not Clearing Interrupt Flags

**Problem**: Interrupt repeatedly triggers.

**Solution**: Always clear interrupt flags.

```rust
#[interrupt]
fn TIM2() {
    // Clear interrupt flag
    let tim2 = unsafe { &*TIM2::ptr() };
    tim2.sr.write(|w| w.uif().clear_bit());
}
```

---

## Related Resources

- [Embedded Rust Book](https://rust-embedded.github.io/book/)
- [Tutorials: Advanced](/en/learn/software-engineering/programming-language/rust/tutorials/advanced) - Embedded basics
- [Cookbook](/en/learn/software-engineering/programming-language/rust/how-to/cookbook) - Embedded recipes
- [Resources](/en/learn/software-engineering/programming-language/rust/reference/resources) - Embedded tools and HALs

---

**Build reliable embedded systems with Rust!**
