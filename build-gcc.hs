import System.Environment
import System.Directory
import System.Process (callProcess)
import System.Cmd (system)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError, catchIOError, ioError)
import Data.List (intercalate)

--type Package = (String, String)

tools =
    [ ("binutils", "2.24")
    , ("gcc", "4.9.0")
    ]

target_arch = "i386-linux-gnu"
sysroot = "/mnt/i386-toolchain"
prefix = sysroot </> "usr"
root = "/mnt/wksp"
srcroot = root </> "src"
buildroot = root </> "build"

opts =
    [ "--target=" ++ target_arch
    , "--with-sysroot=" ++ sysroot
    , "--prefix=" ++ prefix
    , "--enable-languages=c,c++"
    , "--enable-shared"
    , "--with-system-zlib"
    ]

lib_opts = ("--host=" ++ target_arch) : opts

rmDirQuiet :: FilePath -> IO ()
rmDirQuiet = flip catchIOError ignoreMissing . removeDirectoryRecursive
    where
    ignoreMissing e | isDoesNotExistError e = return ()
                    | otherwise = ioError e

prepFreshDir d = rmDirQuiet d >> createDirectory d >> setCurrentDirectory d

confwith configure options = callProcess configure options

preserveCWD actions = do
    cwd <- getCurrentDirectory
    actions
    setCurrentDirectory cwd

binutils = preserveCWD $ do
    prepFreshDir builddir
    configure `confwith` opts
    -- callProcess "make" ["-j7", "all", "install"]
    system "make -j7 all"
    system "make -j7 install"
    where
    configure = srcroot </> (pkg ++ "-" ++ version) </> "configure"
    builddir = buildroot </> (pkg ++ "-" ++ version)
    (pkg, version) = ("binutils", "2.24")

gcc = preserveCWD $ do
    prepFreshDir builddir
    configure `confwith` opts
    --callProcess "make" ["-j7", "all-gcc", "all-target-libgcc"]
    -- callProcess "make" ["-j7", "install-gcc", "install-target-libgcc"]
    system "make -j7 all-gcc"
    system "make -j7 all-target-libgcc"
    system "make -j7 install-gcc"
    system "make -j7 install-target-libgcc"
    where
    configure = srcroot </> (pkg ++ "-" ++ version) </> "configure"
    builddir = buildroot </> (pkg ++ "-" ++ version)
    (pkg, version) = ("gcc", "4.9.0")


main = binutils
