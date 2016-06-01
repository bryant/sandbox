#![feature(libc)]

extern crate libc;
use libc::{c_int, c_void, pid_t, size_t};
use std::mem;
use std::process::Command;

struct ReadPipe(c_int);
struct WritePipe(c_int);

impl ReadPipe {
    fn read(&self) -> Vec<u8> {
        unsafe {
            let mut buf = [0 as u8; 1024];
            let len = libc::read(self.0, buf.as_mut_ptr() as *mut c_void, 1024);
            if len == -1 {
                vec![]
            } else {
                buf[0..len as usize].iter().cloned().collect()
            }
        }
    }
}

impl WritePipe {
    fn write(&self, buf: &[u8]) -> isize {
        unsafe {
            let b = buf.as_ptr() as *const c_void;
            libc::write(self.0, b, buf.len() as size_t) as isize
        }
    }
}

impl Drop for WritePipe {
    fn drop(&mut self) { unsafe { libc::close(self.0) }; }
}

impl Drop for ReadPipe {
    fn drop(&mut self) { unsafe { libc::close(self.0) }; }
}

fn pipe() -> Option<(ReadPipe, WritePipe)> {
    let mut fds = [0 as c_int; 2];
    let f: *mut c_int = unsafe { mem::transmute(&mut fds) };
    match unsafe { libc::pipe(f) } {
        0 => Some((ReadPipe(fds[0]), WritePipe(fds[1]))),
        _ => None,
    }
}

enum ForkIdent {
    Parent(ReadPipe, WritePipe, pid_t),
    Child(ReadPipe, WritePipe),
}

use ForkIdent::*;

fn fork_with_pipe() -> Option<ForkIdent> {
    let (pr, cw) = pipe().unwrap();
    let (cr, pw) = pipe().unwrap();

    match unsafe { libc::fork() } {
        -1 => None,
        0 => Some(Child(cr, cw)),
        childpid => Some(Parent(pr, pw, childpid)),
    }
}

fn unshare(flags: &[i32]) -> i32 {
    let cflags = flags.iter().fold(0, |m, n| m | n);
    println!("unshare called with flags {}", cflags);
    unsafe { libc::unshare(cflags as c_int) as i32 }
}

// const CLONE_VM: i32 = 0x00000100;
// const CLONE_FS: i32 = 0x00000200;
// const CLONE_FILES: i32 = 0x00000400;
// const CLONE_SIGHAND: i32 = 0x00000800;
// const CLONE_PTRACE: i32 = 0x00002000;
// const CLONE_VFORK: i32 = 0x00004000;
// const CLONE_PARENT: i32 = 0x00008000;
// const CLONE_THREAD: i32 = 0x00010000;
// const CLONE_SYSVSEM: i32 = 0x00040000;
// const CLONE_SETTLS: i32 = 0x00080000;
// const CLONE_PARENT_SETTID: i32 = 0x00100000;
// const CLONE_CHILD_CLEARTID: i32 = 0x00200000;
// const CLONE_DETACHED: i32 = 0x00400000;
// const CLONE_UNTRACED: i32 = 0x00800000;
// const CLONE_CHILD_SETTID: i32 = 0x01000000;
// const CLONE_NEWUTS: i32 = 0x04000000;
// const CLONE_NEWIPC: i32 = 0x08000000;
// const CLONE_NEWNET: i32 = 0x40000000;
// const CLONE_IO: i32 = 0x80000000;
const CLONE_NEWNS: i32 = 0x00020000;
const CLONE_NEWUSER: i32 = 0x10000000;
const CLONE_NEWPID: i32 = 0x20000000;

const MS_NOEXEC: u64 = 8;
const MS_NOSUID: u64 = 2;
const MS_NODEV: u64 = 4;

fn asi8(b: &'static [u8]) -> *const i8 { b.as_ptr() as *const i8 }

fn run_child(cr: ReadPipe, cw: WritePipe) {
    println!("child invoked unshare: {}",
             unshare(&[CLONE_NEWUSER, CLONE_NEWNS, CLONE_NEWPID]));
    cw.write(b"yep");
    let pingback = cr.read();
    println!("got {:?} from parent", pingback);

    // we need to fork the shell instead of execve-ing it, as linux expects that
    // the first process (us) is init. execveing results in `sh: 2: cannot
    // fork`.
    match fork_with_pipe() {
        None => panic!("fork #2 failed"),
        Some(Parent(_, _, subpid)) => waitpid(subpid),
        Some(Child(_, _)) => {
            println!("uid: {}, gid: {}",
                     unsafe { libc::getuid() },
                     unsafe { libc::getgid() });

            println!("pid: {}", unsafe { libc::getpid() });

            let (src, targ) = (asi8(b"proc\0"), asi8(b"/proc\0"));
            let flags = MS_NOEXEC | MS_NOSUID | MS_NODEV;
            unsafe {
                let rv = libc::mount(src, targ, src, flags, std::ptr::null());
                println!("mount: {}; err? {}", rv, *libc::__errno_location());
            };

            let empty = &[std::ptr::null()] as *const *const i8;
            let bash = asi8(b"/bin/bash\0");
            let rv = unsafe { libc::execve(bash, empty, empty) };
            println!("invocation: {}", rv);
        }
    }
}

fn waitpid(subpid: pid_t) {
    unsafe {
        libc::waitpid(subpid, std::ptr::null_mut(), 0);
    }
}

fn main() {
    match fork_with_pipe() {
        None => panic!("fork_with_pipe prob"),
        Some(Child(cr, cw)) => run_child(cr, cw),
        Some(Parent(pr, pw, subpid)) => {
            println!("parent!");
            let pingback = pr.read();
            println!("got {:?} from child", pingback);

            // remap
            let mut newuidmap = Command::new("/usr/bin/newuidmap");
            let mut newgidmap = Command::new("/usr/bin/newgidmap");
            newuidmap.arg(format!("{}", subpid)).args(&["0", "1000", "1"]);
            newgidmap.arg(format!("{}", subpid)).args(&["0", "1000", "1"]);
            println!("newuidmap: {:?}", newuidmap.output().unwrap());
            println!("newgidmap: {:?}", newgidmap.output().unwrap());

            pw.write(b"remap finished. fork away.");
            waitpid(subpid);
        }
    }
}
