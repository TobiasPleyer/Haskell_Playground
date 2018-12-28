from typing import Any, Callable, Tuple
from subprocess import run, PIPE, STDOUT
from functools import partial


class OverloadException(Exception):
    pass


class OverloadedFunction:
    def __init__(self, name):
        self._name = name
        self._overloads = {}

    def add_overload(self, signature: Tuple[Any, ...], func: Callable[[Any], Any]):
        if signature in self._overloads:
            raise OverloadException(f"Overloaded function '{self._name}' has already been overloaded with signature {signature}")
        self._overloads[signature] = func

    def __call__(self, *args):
        signature = tuple(map(type, args))
        func = self._overloads.get(signature, None)
        if func is None:
            raise OverloadException(f"Overloaded function '{self._name}' does not provide an overload for the signature {signature}")
        else:
            return func(*args)


def overload(overloaded_funcname: str):
    def decorator(func: Callable[[Any], Any])-> Callable[[Any], Any]:
        if overloaded_funcname not in globals():
            globals()[overloaded_funcname] = OverloadedFunction(overloaded_funcname)
        overloaded_func = globals()[overloaded_funcname]
        if not isinstance(overloaded_func, OverloadedFunction):
            raise OverloadException("Given function name does not correspond to an overloaded function")
        signature = func.__annotations__
        if 'return' in signature:
            signature.pop('return')
        signature = tuple(signature.values())
        overloaded_func.add_overload(signature, func)
        return func
    return decorator


@overload('bind')
def default():
    raise Exception("Bad")


class BindFunc:
    def __init__(self, func):
        self._func = func

    def __call__(self, a):
        return self._func(a)


class Monad:
    def __init__(self, value):
        self._value = value

    def __or__(self, k):
        return bind(self, BindFunc(k))

    def __rshift__(self, k):
        return bind(self, BindFunc(lambda _: k))

    def unwrap(self):
        return self._value

    def __str__(self):
        if self._kind == Maybe.Nothing:
            return "Nothing"
        else:
            return f"Just({self._value})"

    def __repr__(self):
        return self.__str__()


def returnMaybe(value):
    return Maybe(Maybe.Just, value)


class Maybe(Monad):
    Nothing = 0
    Just = 1
    def __init__(self, kind, value=None):
        self._kind = kind
        super().__init__(value)

    def get_return(self):
        return returnMaybe

    def __str__(self):
        if self._kind == Maybe.Just:
            return f"Just({self._value})"
        else:
            return f"Nothing"


def isNothing(e):
    return e._kind == Maybe.Nothing


def isJust(e):
    return e._kind == Maybe.Just


def nothing():
    return Maybe(Maybe.Nothing)


def just(a):
    return Maybe(Maybe.Just, a)


@overload('bind')
def bind_maybe(m: Maybe, k: BindFunc):
    if isNothing(m):
        return m
    else:
        return k(m.unwrap())


def returnEither(value):
    return Either(Either.Right, value)


class Either(Monad):
    Left = 0
    Right = 1
    def __init__(self, kind, value):
        self._kind = kind
        super().__init__(value)

    def get_return(self):
        return returnEither

    def __str__(self):
        if self._kind == Either.Left:
            return f"Left({self._value})"
        else:
            return f"Right({self._value})"


def isLeft(e):
    return e._kind == Either.Left


def isRight(e):
    return e._kind == Either.Right


def left(a):
    return Either(Either.Left, a)


def right(a):
    return Either(Either.Right, a)


@overload('bind')
def bind_either(m: Either, k: BindFunc):
    if isLeft(m):
        return m
    else:
        return k(m.unwrap())


class WriterT(Monad):
    def __init__(self, returnM, value):
        self._returnM = returnM
        super().__init__(value)

    def get_return(self):
        return lambda value: WriterT(self._returnM, self._returnM((value,[])))

    def __str__(self):
        v_str = str(self._value)
        return f"WriterT({v_str})"


@overload('bind')
def bind_writert(m: WriterT, k: BindFunc):
    inner_ret = m.unwrap().get_return()
    return WriterT(inner_ret,
    bind(m.unwrap(), BindFunc(
    lambda x1: bind(k(x1[0]).unwrap(), BindFunc(
    lambda x2: inner_ret((x2[0], x1[1] + x2[1])))))))


class EitherT(Monad):
    def __init__(self, returnM, value):
        self._returnM = returnM
        super().__init__(value)

    def get_return(self):
        return lambda value: EitherT(self._returnM, self._returnM(right(value)))

    def __str__(self):
        v_str = str(self._value)
        return f"EitherT({v_str})"


@overload('bind')
def bind_eithert(m: EitherT, k: BindFunc):
    inner_ret = m.unwrap().get_return()
    return EitherT(inner_ret,
    bind(m.unwrap(), BindFunc(
    lambda a: inner_ret(a) if isLeft(a) else k(a.unwrap()).unwrap())))


def returnIO(value):
    return IO(lambda: value)


class IO(Monad):
    def __init__(self, value):
        super().__init__(value)

    def get_return(self):
        return returnIO

    def run(self):
        return self._value()

    def __str__(self):
        v_str = str(self._value)
        return f"IO({v_str})"


@overload('bind')
def bind_io(m: IO, k: BindFunc):
    # IO is a delayed computation (aka action), so binding IO means building up
    # a new, combined delayed computation
    return IO(lambda: k(m.run()).run())


if __name__ == '__main__':
    m1 = just(2)
    m2 = just(0)
    k = lambda a: nothing() if (a==0) else just(42.0/a)
    print(m1 | k)
    print(m2 | k)
    print(m1 >> m2)

    e1 = right(2)
    e2 = left(0)
    k = lambda a: right(2*a)
    print(e1 | k)
    print(e2 | k)
    print(e1 >> e2)

    w1 = WriterT(right, right((1,["Hello"])))
    w2 = WriterT(right, right((2,["my friend"])))
    w3 = WriterT(right, right((3,["World"])))
    w4 = WriterT(right, right((4,["END"])))
    l = WriterT(just, left("Error"))

    print(w1 >> w2 >> w3)
    print(w1 >> w2 >> l >> w3)
    print((
    w1 |(
    lambda x: w2 if x>0 else w3)) >>
    w4)

    j1 = WriterT(just, just((1,["Hello"])))
    j2 = WriterT(just, just((2,["my friend"])))
    j3 = WriterT(just, just((3,["World"])))
    j4 = WriterT(just, just((4,["END"])))
    n = WriterT(just, nothing())

    print(j1 >> j2 >> j3)
    print(j1 >> j2 >> n >> j3)
    jBig = j1 >> j2 >> j3 >> j4
    print(jBig >> jBig)
    print((
    j1 |(
    lambda x: j2 if x>0 else j3)) >>
    j4)

    em1 = EitherT(just, just(right(1)))
    em2 = EitherT(just, just(right(2)))
    em3 = EitherT(just, just(right(3)))
    em4 = EitherT(just, just(right(4)))
    eme = EitherT(just, just(left('Error')))
    def h(x,cont):
        print(x)
        return cont
    print(em1 |
          (lambda x: h(x,eme) |
          (lambda x: h(x,em3) |
          (lambda x: h(x,em4)))))

    io1 = IO(lambda: print('hello'))
    io2 = IO(lambda: print('hello again'))
    io3 = IO(lambda: print('bye'))
    io = (io1 >> io2 >> io3)
    print(io)
    io.unwrap()

    # This is the big boy: A monadic stack, stacking WriterT and EitherT on top
    # of the IO monad
    # First the IO action, it must already follow the conventions of the stack,
    # i.e. return a tuple of (Either a, [String]). The first entry is used by
    # the stacked EitherT monad and the second one is the monoid instance for
    # the stacked WriterT monad
    def shell(cmd):
        process = run(cmd, stdout=PIPE, stderr=STDOUT, shell=True)
        if process.returncode > 0:
            return (left(None), [process.stdout])
        return (right(None), [process.stdout])

    mkio = lambda x: IO(lambda: x)
    wtio = lambda x: WriterT(mkio, mkio(x))
    ok1 = EitherT(wtio, WriterT(mkio, IO(lambda: shell("echo 'OK'; exit 0"))))
    ok2 = EitherT(wtio, WriterT(mkio, IO(lambda: shell("echo 'Also OK'; exit 0"))))
    ok3 = EitherT(wtio, WriterT(mkio, IO(lambda: shell("echo 'Even better'; exit 0"))))
    nok1 = EitherT(wtio, WriterT(mkio, IO(lambda: shell("echo 'Error' >&2 ; exit 1"))))
    full_action = ok1 >> ok2 >> nok1 >> ok3
    print("----------")
    print(full_action)
    print("----------")
    print(full_action.unwrap().unwrap().run())
