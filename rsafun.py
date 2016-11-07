def bunpack(s):
    rv = 0
    for c in s:
        rv <<= 8
        rv += ord(c)
    return rv

def read_chunk(s):
    from struct import unpack
    length = unpack(">I", s[:4])[0]
    assert len(s) >= length + 4
    return s[4:length + 4], s[length + 4:]

def decode_pubkey(s):
    logo, s = read_chunk(s)
    pubexp, s = read_chunk(s)
    modulus, _ = read_chunk(s)
    return bunpack(pubexp), bunpack(modulus)

def bit_reverse(n):
    return int(bin(n)[2:][::-1], 2)

def powa(m, e, n):
    """ Computes (m ** e) mod n. """
    #from pudb import set_trace; set_trace()
    if e == 0:
        return 1
    rv = 1
    pos = 1 << (e.bit_length() - 1)
    while pos > 0:
        rv = (rv * rv) % n
        if e & pos > 0:
            rv = (rv * m) % n
        pos >>= 1
    return rv

# id_rsa is pem-encoded (base64) asn1 struct:
# RSAPrivateKey ::= SEQUENCE {
#   version           Version,
#   modulus           INTEGER,  -- n
#   publicExponent    INTEGER,  -- e
#   privateExponent   INTEGER,  -- d
#   prime1            INTEGER,  -- p
#   prime2            INTEGER,  -- q
#   exponent1         INTEGER,  -- d mod (p-1)
#   exponent2         INTEGER,  -- d mod (q-1)
#   coefficient       INTEGER,  -- (inverse of q) mod p
#   otherPrimeInfos   OtherPrimeInfos OPTIONAL
# }
# decode with openssl asn1parse -inform pem < id_rsa
