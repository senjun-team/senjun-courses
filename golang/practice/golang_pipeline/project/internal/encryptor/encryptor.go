package encryptor

import (
	"encoding/base64"
)

const key = "qwerty"

// The EncryptXOR encrypts the plaintext
// with key.
// The EncryptXOR will return Base64 string.
func EncryptXOR(plaintext string) string {
	if len(key) == 0 {
		return plaintext
	}
	bPlain := []byte(plaintext)
	bKey := []byte(key)
	cipher := make([]byte, len(bPlain))
	for i := range bPlain {
		cipher[i] = bPlain[i] ^ bKey[i%len(bKey)]
	}
	return base64.StdEncoding.EncodeToString(cipher)
}

// The DecryptXOR decrypts the string from the EncryptXOR.
func DecryptXOR(encrypted string) (string, error) {
	cipher, err := base64.StdEncoding.DecodeString(encrypted)
	if err != nil {
		return "", err
	}
	bKey := []byte(key)
	plain := make([]byte, len(cipher))
	for i := range cipher {
		plain[i] = cipher[i] ^ bKey[i%len(bKey)]
	}
	return string(plain), nil
}
