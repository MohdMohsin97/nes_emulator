use cpu::{CpuFlags, CPU};

mod cpu;
mod opcodes;
fn main() {
    let mut cpu = CPU::new();
    cpu.load_and_run(vec![0xa9, 0x80, 0x85, 0x10, 0x24, 0x10, 0x00]);

    assert!(!cpu.status.contains(CpuFlags::ZERO));
    assert!(cpu.status.contains(CpuFlags::NEGATIVE));
    assert!(!cpu.status.contains(CpuFlags::OVERFLOW));
}
