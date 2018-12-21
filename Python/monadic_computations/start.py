from subprocess import run, PIPE
from oslash import *


# external :: String -> EitherT String IO String
def do(cmd):
    process = run(cmd, stdout=PIPE, stderr=PIPE, shell=True)
    stdout = process.stdout.decode("utf-8")
    stderr = process.stderr.decode("utf-8")
    return Left(stderr) if process.returncode > 0 else Right(stdout)

print(do("echo 'NOK' >&2; exit 1"))
print(do("echo 'OK' >&1; exit 0"))
print(
    do("echo 'OK1' >&1; exit 0") >>
    do("echo 'OK2' >&1; exit 0") >>
    do("echo 'NOK' >&2; exit 1") >>
    do("echo 'OK3' >&1; exit 0") >>
    do("echo 'OK4' >&1; exit 0")
)
