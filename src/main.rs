use cpu::CPU;

mod cpu;
mod opcodes;
fn main() {
    let mut cpu = CPU::new();
        cpu.register_a = 0x50;

        cpu.load_and_run(vec![0x69, 0x50, 0x00]);

        assert_eq!(cpu.register_a, 0xa0);
        assert_eq!(cpu.status.bits() & 0b0000_0001, 0b1);
}
