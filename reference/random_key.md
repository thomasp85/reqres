# Generate a random key compatible with encryption and decryption in requests and responses

The encryption/decryption used in reqres is based on the
[sodium](https://github.com/r-lib/sodium) package and requires a 32-bit
encryption key encoded as hexadecimal values. While you can craft your
own, this function will take care of creating a compliant key using a
cryptographically secure pseudorandom number generator from
[`sodium::helpers()`](https://docs.ropensci.org/sodium/reference/helpers.html).

## Usage

``` r
random_key()
```

## Value

A 32-bit key as a hex-encoded string

## Details

Keep your encryption keys safe! Anyone with the key will be able to
eavesdrop on your communication and tamper with the information stored
in encrypted cookies through man-in-the-middle attacks. The best
approach is to use the keyring package to manage your keys, but as an
alternative you can store it as environment variables.

**NEVER STORE THE KEY IN PLAIN TEXT.**

**NEVER PUT THE KEY SOMEWHERE WHERE IT CAN ACCIDENTALLY BE COMMITTED TO
GIT OR OTHER VERSION CONTROL SOFTWARE**

## Examples

``` r
if (FALSE) {
# Store a key with keyring and use it
keyring::key_set_with_value("reqres_key", random_key())

rook <- fiery::fake_request("http://example.com")

Request$new(rook, key = keyring::key_get("reqres_key"))
}
```
