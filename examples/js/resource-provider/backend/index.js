const express = require('express');
const jwt = require('jsonwebtoken');
const jwksClient = require('jwks-rsa');
const cors = require('cors');

const app = express();
const port = 4000;
app.use(cors())

// This will count the number of clicks
let clickCount = 0;

// OIDC Config
const oidcIssuer = 'http://localhost:8080'; // Replace with your OIDC issuer
const client = jwksClient({
  jwksUri: `${oidcIssuer}/oauth/jwks`, // Adjust to match your OIDC provider's well-known JWKS endpoint
});

// Middleware to retrieve the signing key
function getKey(header, callback) {
  client.getSigningKey(header.kid, (err, key) => {
    if (err) {
      return callback(err);
    }
    const signingKey = key.getPublicKey();
    callback(null, signingKey)
  });
}

// Middleware to verify the access token
function verifyToken(req, res, next) {
  const token = req.headers.authorization?.split(' ')[1];
  if (!token) {
    return res.status(401).send('Access Denied. No token provided.');
  }

  jwt.verify(token, getKey, { algorithms: ['RS256'] }, (err, decoded) => {
    if (err) {
      console.log(err)
      return res.status(401).send('Access Denied. Invalid token.');
    }

    // Token is valid, proceed
    req.user = decoded;
    next();
  });
}

// Route for counting clicks
app.get('/count-clicks', verifyToken, (req, res) => {
  clickCount++;
  res.send(`Click count: ${clickCount}`);
});

// Start the server
app.listen(port, () => {
  console.log(`Microservice running on port ${port}`);
});

