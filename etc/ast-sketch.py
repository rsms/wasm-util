class Sym:
  def __init__(s, name): s.name = name
  def __repr__(s): return s.name
  def __str__(s): return s.name

Module = Sym('module')
Section = Sym('section')
VarUint32 = Sym('varuint32')

class N:
  def __init__(n, type, *children):
    n.type = type
    n.children = children
    n.byteSize = sum([c.byteSize for c in n.children])
  def __repr__(n):
    return '(%s%s)' % (n.type,
      ''.join([' ' + repr(c) for c in n.children]))

class V:
  def __init__(v, type, value, bytes):
    v.type = type
    v.value = value
    v.bytes = bytes
    v.byteSize = len(v.bytes)
  def __repr__(v):
    return '(%s %r)' % (v.type, v.value)

def module(version, *sections):
  return N(Module,
    varuint32(0x6d736100),
    varuint32(version),
    *sections)

def varuint32(value):
  return V(VarUint32, value, '\0')

m = module(
  0xd,
  N(Section, varuint32(1))
)

print(repr(m))
